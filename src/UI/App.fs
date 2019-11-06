module App
open System
open Elmish
open Elmish.Browser.Navigation
open Model.Types
open Model.Operations
open Common
open Fable.Import.Browser

open Elmish.React
open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
window.onerror <-
    fun msg _src _lineNo _colNo err ->
        if msg.Contains "SocketProtocolError" = false then
            window.alert (sprintf "Bug alert! Unhandled exception. Email Max and tell him what happened. Error details: %A %A" msg err)

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
importAll "../../sass/main.sass"

type Model = { value: string; greeting: string option; log: string list }
type Cmd = NewValue of string | ENTER | ClearLog
let view m dispatch =
    let currentInput, greeting, log = (m.value, m.greeting, m.log)
    div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
        yield p[ClassName "summaryPane"][str <| match greeting with Some greeting -> greeting | None -> ""]
        yield p[ClassName "queryPane"][
            h2[][str "What's your name?"]
            br[]
            form [OnSubmit (fun _ -> dispatch ENTER)] [
                input [OnChange (fun e -> e.Value |> NewValue |> dispatch); HTMLAttr.Value currentInput; HTMLAttr.AutoFocus true]
                button[Type "submit"][str "OK"]
            ]
            br[]
            ]
        if log <> [] then
            yield div[ClassName "sidebar"][
                ul[] [for entry in log -> li[][str entry]]
                button[OnClick (fun _ -> dispatch ClearLog)][str "Clear"]
            ]
    ]

let init _ = { value = ""; greeting = None; log = [] }, Cmd.Empty
let update msg model =
    match msg with
    | NewValue s -> { model with value = s; greeting = None }, Cmd.Empty
    | ENTER ->
        if not <| String.IsNullOrWhiteSpace model.value then
            let greeting = sprintf "Hi, %s" model.value
            { model with greeting = Some greeting; value = ""; log = model.log@[greeting] }, Cmd.Empty
        else
            model, Cmd.Empty
    | ClearLog -> { model with log = [] }, Cmd.Empty

// App
Program.mkProgram init update view
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
