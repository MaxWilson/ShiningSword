module UI.App

open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core

importSideEffects "../sass/main.sass"

module App =
    open Domain.Character
    open Domain.Character.DND5e

    type Page =
        | Chargen of Chargen.View.Model
    type Model = { stack: Page list; error: string option; hero: CharacterSheet option }
    type Msg =
        | Error of msg: string
        | Transform of (Model -> Model)
        | Chargen of Chargen.View.Msg
        | Push of Page
        | Pop
        | Navigate of url: string

    let init initialCmd =
        { stack = []; error = None; hero = None; }, Cmd.batch initialCmd
    let update msg model =
        match msg, model.stack with
        | Error msg, _ -> { model with error = Some msg }, Cmd.Empty
        | Transform f, _ -> { f model with error = None }, Cmd.Empty
        | Navigate url, _ -> model, Navigation.Navigation.newUrl ("#" + url)
        | Chargen msg, (Page.Chargen chargenModel)::rest ->
            let control = function
            | Chargen.View.Complete characterSheet ->
                Cmd.ofSub(fun dispatch ->
                    Transform (fun s -> { s with hero = Some characterSheet }) |> dispatch
                    Pop |> dispatch
                    )
            | Chargen.View.Cancel -> Cmd.ofMsg Pop
            | Chargen.View.NavigateTo ruleset ->
                match ruleset with
                | Chargen.Interaction.Ruleset.ADND -> Navigate "chargen/adnd"
                | Chargen.Interaction.Ruleset.DND5e -> Navigate "chargen/5e"
                |> Cmd.ofMsg
            let cmd = (Chargen >> Cmd.ofMsg)
            let chargenModel, cmd = Chargen.View.update (Chargen >> Cmd.ofMsg) control msg chargenModel
            { model with stack = (Page.Chargen chargenModel)::rest }, cmd
        | Pop, _::[] ->
            // default to welcome screen if stack is empty
            let model', msg = Chargen.View.init()
            { model with stack = [] }, Cmd.Empty
        | Pop, _::rest -> { model with stack = rest }, Cmd.Empty
        | Push(page), stack -> { model with stack = page::stack }, Cmd.Empty // Elmish.Navigation.Navigation.newUrl "#rust"
        | _ -> model, (Error $"Message '{msg}' not compatible with current page ({model.stack |> List.tryHead}))" |> Cmd.ofMsg)
    open Feliz.Router
    let view (model: Model) dispatch =
        let window = Browser.Dom.window;
        match model.stack with
        | (Page.Chargen model)::_ ->
            Chargen.View.view model (Chargen >> dispatch)
        | _ ->
            Html.div [
                prop.className "intro"
                prop.children [
                    Html.div [
                        prop.children [
                            Html.text (match model.error with Some msg -> msg | None -> "Welcome to Shining Sword")
                            ]
                        prop.style [style.marginBottom 10]
                        ]
                    Html.button [
                        prop.text "Create a character"
                        prop.onClick(thunk1 dispatch (Navigate "chargen/adnd"))
                        ]
                    Html.div [
                        prop.className "footer"
                        prop.children [
                            Html.a [prop.href "https://www.flaticon.com/free-icons/sword"; prop.text "Sword icon created by pongsakornRed - Flaticon"]
                            ]
                        ]
                    ]
               ]

module Url =
    open App
    open Chargen.View
    module Parse =
        open Browser.Types
        open Packrat
        let locationParser (rootActivePattern: ParseRule<_>) (loc: Location) =
            let (|Root|_|) = rootActivePattern
            match ParseArgs.Init loc.hash with
            | Str "#" (Root(v, End)) -> v
            | _ -> []

        let (|Page|_|) = function
            | Str "chargen/adnd/DarkSun" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Push (Page.Chargen model'))
                        Cmd.ofMsg (SetMethod DarkSunMethodI |> Chargen)
                        msg |> Cmd.map Chargen
                        ]
                Some(cmd, ctx)
            | Str "chargen/adnd" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Push (Page.Chargen model'))
                        msg |> Cmd.map Chargen
                        ]
                Some(cmd, ctx)
            | Str "chargen/5e" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Push (Page.Chargen model'))
                        msg |> Cmd.map Chargen
                        Cmd.ofMsg (Chargen (SetRuleset UI.Chargen.Interaction.Ruleset.DND5e))
                        ]
                Some(cmd, ctx)
            | Str "chargen" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Push (Page.Chargen model'))
                        msg |> Cmd.map Chargen
                        ]
                Some(cmd, ctx)
            | _ -> None

        let page = locationParser (|Page|_|)
    let parse loc =
        let parsed = Parse.page loc
        parsed
    let unpack cmds model =
        model, Cmd.batch cmds

open App
open Elmish
open Elmish.Navigation

Program.mkProgram init update view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun dispatch ->
    Browser.Dom.window.onerror <-
    fun msg ->
        if msg.ToString().Contains "SocketProtocolError" = false then
            dispatch (sprintf "Error: %A" msg |> Error)
            Browser.Dom.window.alert ("Unhandled Exception: " + msg.ToString())
        ))
|> Program.toNavigable Url.parse Url.unpack
|> Program.withReactBatched "feliz-app"
|> Program.run
