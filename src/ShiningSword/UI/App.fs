module App

open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
importSideEffects "../sass/main.sass"

module App =
    type Page =
        | Chargen of Chargen.View.Model
    type Model = { stack: Page list; error: string option; hero: Chargen.Domain.CharacterSheet option; roster: int list }
    type Msg =
        | Error of msg: string
        | Transform of (Model -> Model)
        | Chargen of Chargen.View.Msg
        | Pop
    let init _ =
        let model, msg = Chargen.View.init()
        { stack = [Page.Chargen model]; error = None; hero = None; roster = List.empty }, msg |> Cmd.map Chargen
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
            Html.text (model.roster.Length.ToString() + " characters in roster")
            Html.button [
                prop.text "Generate more"
                prop.onClick(fun _ ->

                    let newItems = [
                        let count = model.roster.Length
                        for ix in 1..(max count 100) do
                            (count + ix)
                        ]
                    dispatch (Transform (fun m -> { m with roster = m.roster@ (newItems)}))
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
