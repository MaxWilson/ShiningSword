module App
open UI.Ribbit
open Elmish

open Elmish.React
//open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
importAll "../../App/sass/main.sass"

module Url =
    type ParseResult = Battle | Neither
    module Parse =
        open Browser.Types
        open Packrat
        let locationParser (rootActivePattern: ParseInput -> (_ * ParseInput) option) (loc: Location) =
            let (|Root|_|) = rootActivePattern
            match ParseArgs.Init loc.hash with
            | Str "#" (Root(v, End)) -> v
            | _ -> Neither

        let (|Page|_|) = function
            | Str "ribbit" ctx ->
                Some(Battle, ctx)
            | _ -> None

        let page = locationParser (|Page|_|)
    let parse = Parse.page

module App =
    type Model = UI.Ribbit.Model
    type Msg = Battle of UI.Ribbit.Cmd
    let update msg model =
        match msg with
        | Battle msg ->
            UI.Ribbit.update msg model
    let init = function
        | Url.ParseResult.Battle ->
            UI.Ribbit.init ()
        | Url.ParseResult.Neither ->
            UI.Ribbit.init ()
    let view (model: Model) dispatch =
        UI.Ribbit.view model (Msg.Battle >> dispatch)
    let urlUpdate (parseResult: Url.ParseResult) (model: Model) =
        match parseResult with
        | Url.ParseResult.Battle ->
            UI.Ribbit.init ()
        | Url.ParseResult.Neither -> model, Cmd.Empty

open App
Program.mkProgram init update view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun d ->
    Browser.Dom.window.onerror <-
    fun msg ->
        if msg.ToString().Contains "SocketProtocolError" = false then
            d (App.Msg.Battle <| UI.Ribbit.Error (sprintf "Error: %A" msg))
        ))
#if DEBUG
|> Program.toNavigable Url.parse App.urlUpdate
//|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
