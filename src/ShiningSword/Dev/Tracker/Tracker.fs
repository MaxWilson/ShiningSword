module Dev.Tracker.App

open Elmish
open Elmish.React
open Dev.Tracker.UI
open Browser.Types

module Cmd =
    let ofSub fnDispatch =
        [   [], fnDispatch
            ]
    let onDispose f =
        { new System.IDisposable with
            member this.Dispose(): unit = f()
            }
    let noDispose = onDispose ignore

let start() =
    Program.mkSimple init update view
    |> Program.withSubscription(fun _ ->
        Cmd.ofSub(fun dispatch ->
            Browser.Dom.window.onkeydown <- fun e ->
                let (|Ctrl|_|) = function
                    | str when e.ctrlKey -> Some str
                    | _ -> None
                let mutable handled = true // shortcut so we don't have to type e.preventDefault() in every valid case
                match e.key.ToLowerInvariant() with
                | Ctrl "l" ->
                    dispatch (ToggleLog None)
                | Ctrl "?" ->
                    dispatch (ToggleHelp None)
                | Ctrl "c" ->
                    match (Browser.Dom.window.document.getElementById "userInput") with
                    | null -> handled <- false
                    | e -> e.focus()
                | Ctrl "arrowup" ->
                    dispatch (LogNav(-1, 0))
                | Ctrl "arrowdown" ->
                    dispatch (LogNav(+1, 0))
                | Ctrl "arrowleft" ->
                    dispatch (LogNav(0, -1))
                | Ctrl "arrowright" ->
                    dispatch (LogNav(0, +1))
                | _ -> handled <- false
                if handled then e.preventDefault()
            Cmd.noDispose
            ))
    |> Program.withReactBatched "feliz-dev"
    |> Program.run
