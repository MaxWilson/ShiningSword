module App

open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core
open Chargen.Domain

importSideEffects "../sass/main.sass"

type StateMsg = Add of CharacterSheet | Replace of int * CharacterSheet

module Lst =
    type Lst<'t> = ResizeArray<'t>

    [<Emit("[]")>]
    let inline emptyArray() = new Lst<'t>()
    [<Emit("$0.push($1)")>]
    let push (array: Lst<_>) v =
        array.Add v

    [<Emit("$0.length")>]
    let len (array: Lst<_>) =
        array.Count
open Lst

let v =
    Stateful.State.create((fun () -> emptyArray()), fun msg (state: CharacterSheet Lst) ->
        match msg with
        | Add n ->
            push state n
            state
        | Replace(i,n)->
            state[i] <- n
            state
    )

module App =
    type Page =
        | Chargen of Chargen.View.Model
    type Model = { stack: Page list; error: string option; hero: Chargen.Domain.CharacterSheet option; roster: Stateful.State<CharacterSheet Lst, StateMsg>}
    type Msg =
        | Error of msg: string
        | Transform of (Model -> Model)
        | Chargen of Chargen.View.Msg
        | Pop
    let init _ =
        let model, msg = Chargen.View.init()
        { stack = [Page.Chargen model]; error = None; hero = None; roster = v }, msg |> Cmd.map Chargen
    let update msg model =
        match msg, model.stack with
        | Error msg, _ -> { model with error = Some msg }, Cmd.Empty
        | Transform f, _ -> { f model with error = None }, Cmd.Empty
        | Chargen msg, (Page.Chargen chargenModel)::rest ->
            let finishWith = function
            | Some (character: Chargen.Domain.CharacterSheet) ->
                Cmd.ofSub(fun dispatch ->
                    Transform (fun s -> { s with hero = Some character }) |> dispatch
                    Pop |> dispatch
                    )
            | None -> Cmd.ofMsg Pop
            let chargenModel, cmd = Chargen.View.update finishWith msg chargenModel
            { model with stack = (Page.Chargen chargenModel)::rest }, cmd
        | Pop, _ -> { model with stack = match model.stack with _::rest -> rest | _ -> model.stack }, Cmd.Empty
        | _ -> model, (Error $"Message '{msg}' not compatible with current page" |> Cmd.ofMsg)
    let view (model: Model) dispatch =
        let window = Browser.Dom.window;
        Html.div [
            Html.div [
                prop.children [
                    Html.text (match model.error with Some msg -> msg | None -> "Welcome to Shining Sword")
                    ]
                prop.style [style.marginBottom 10]
                ]
            match model.stack with
            | (Page.Chargen model)::_ ->
                Chargen.View.view model (Chargen >> dispatch)
            | _ -> ()
            let roster', roster = (Stateful.deref model.roster)
            let length = Lst.len roster'
            Html.text (length.ToString() + " characters in roster")
            Html.button [
                prop.text "Generate more"
                prop.onClick(fun _ ->
                    let mutable rosterState = roster
                    let newItems = [
                        let count = length
                        for ix in 1..(max count 100) do
                            let char = Chargen.Interaction.create Chargen.Interaction.roll3d6InOrder |> Chargen.Interaction.ofDraft |> Option.get
                            rosterState <- Stateful.execute (Add (char)) rosterState
                            (count + ix)
                        ]
                    dispatch (Transform (fun m -> { m with roster = rosterState}))
                    )
                ]
            ]

open App
Program.mkProgram init update view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun dispatch ->
    Browser.Dom.window.onerror <-
    fun msg ->
        if msg.ToString().Contains "SocketProtocolError" = false then
            dispatch (sprintf "Error: %A" msg |> Error)
            Browser.Dom.window.alert ("Unhandled Exception: " + msg.ToString())
        ))
|> Program.withReactBatched "feliz-app"
|> Program.run
