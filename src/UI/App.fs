module App
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

type ViewCmd = ChangeTo of string | OK
let view (currentInput, greeting) dispatch = div [] [
        h2[][str "What's your name?"]
        br[]
        form [OnSubmit (fun _ -> dispatch OK)] [
            input [OnChange (fun e -> e.Value |> ChangeTo |> dispatch); unbox <| HTMLAttr.Value currentInput]
            button[Type "submit"][str "OK"]
        ]
        br[]
        str <| match greeting with Some greeting -> sprintf "Hi, %s" greeting | None -> ""
    ]
type Model = { value: string; greeting: string option }
type Cmd = NewValue of string | ENTER
let init _ = { value = ""; greeting = None }, Cmd.Empty
let update msg model =
    match msg with
    | NewValue s -> { model with value = s; greeting = None }, Cmd.Empty
    | ENTER -> { model with greeting = Some model.value; value = "" }, Cmd.Empty

// App
Program.mkProgram init update (fun m d -> view (m.value, m.greeting) ((function (ViewCmd.ChangeTo v) -> NewValue v | OK -> ENTER) >> d))
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
