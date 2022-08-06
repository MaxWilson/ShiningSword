module Dev.App.Tracker

open Elmish
open Elmish.React
open Dev.App.Tracker.UI

let start() =
    Program.mkSimple init update view
    |> Program.withReactBatched "feliz-dev"
    |> Program.run
