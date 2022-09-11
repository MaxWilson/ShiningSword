module Dev.Tracker.App

open Elmish
open Elmish.React
open Dev.Tracker.UI

let start() =
    Program.mkSimple init update view
    |> Program.withSubscription(fun _ ->
        Cmd.ofSub(fun dispatch ->
            Browser.Dom.window.onkeydown <- fun e ->
                if e.ctrlKey then
                    match e.key.ToLowerInvariant() with
                    | "l" ->
                        e.preventDefault()
                        dispatch (ToggleLog None)
                    | "?" ->
                        e.preventDefault()
                        dispatch (ToggleHelp None)
                    | "c" ->
                        e.preventDefault()
                        match (Browser.Dom.window.document.getElementById "userInput") with
                        | null -> ()
                        | e -> e.focus()
                    | _ -> ()
            ))
    |> Program.withReactBatched "feliz-dev"
    |> Program.run
