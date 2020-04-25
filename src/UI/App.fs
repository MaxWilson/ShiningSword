module App
open UI.Ribbit
open Elmish

//open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
importAll "../sass/main.sass"

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

open UI.Display
module App =
    type Model = UI.Ribbit.Model
    type Msg = Battle of UI.Ribbit.Cmd | UpdateAnimationTime of AnimationTime
    let duration = AnimationTime 3000.
    let update msg (model: Model) =
        match msg with
        | Battle msg ->
            let model' =
                match msg with
                | ENTER msg ->
                    let newAnimation =
                        { FadingText.color = chooseRandom [Color.Black; Color.Red; Color.Grey]
                          FadingText.duration = duration
                          FadingText.endTime = model.animations.currentTime + duration
                          text = msg
                        }
                    let filterExpired (texts: FadingText list) =
                        texts |> List.filter (fun t -> t.endTime >= model.animations.currentTime)
                    { model with animations = { model.animations with texts = newAnimation::model.animations.texts |> filterExpired } }
                | _ -> model
            UI.Ribbit.update msg model'
        | UpdateAnimationTime (AnimationTime time as t) ->
            { model with animations = { model.animations with currentTime = t }}, Cmd.Empty
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
|> Program.withSubscription(fun m ->
    Cmd.ofSub(fun dispatch ->
        Browser.Dom.window.onerror <-
            fun msg _src _lineNo _colNo err ->
                if msg.ToString().Contains "SocketProtocolError" = false then
                    (App.Msg.Battle <| UI.Ribbit.Error (sprintf "Error: %A" msg)) |> dispatch
        let rec animationLoop time =
            time |> AnimationTime |> UpdateAnimationTime |> dispatch
            Browser.Dom.window.requestAnimationFrame(animationLoop) |> ignore
        Browser.Dom.window.requestAnimationFrame(animationLoop) |> ignore
    ))
#if DEBUG
|> Program.toNavigable Url.parse App.urlUpdate
//|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
