module Dev.Tracker.App

open Elmish
open Elmish.React
open Dev.Tracker.UI
open Browser.Types

let start() =
    Program.mkSimple init update view
    |> Program.withSubscription(fun _ ->
        [[], fun dispatch ->
            let handle (e: KeyboardEvent) =
                if e.ctrlKey then
                    let mutable handled = true // shortcut so we don't have to type e.preventDefault() in every valid case
                    match e.key.ToLowerInvariant() with
                    | "l" ->
                        dispatch (ToggleLog None)
                    | "?" ->
                        dispatch (ToggleHelp None)
                    | "c" ->
                        match (Browser.Dom.window.document.getElementById "userInput") with
                        | null -> handled <- false
                        | e -> e.focus()
                    | "arrowup" ->
                        dispatch (LogNav(-1, 0))
                    | "arrowdown" ->
                        dispatch (LogNav(+1, 0))
                    | "arrowleft" ->
                        dispatch (LogNav(0, -1))
                    | "arrowright" ->
                        dispatch (LogNav(0, +1))
                    | _ -> handled <- false
                    if handled then e.preventDefault()
            Browser.Dom.window.onkeydown <- handle
            { new System.IDisposable with
                  member this.Dispose(): unit =
                    Browser.Dom.window.onkeydown <- ignore
                  }
            ])
    |> Program.withReactBatched "feliz-dev"
    |> Program.run
