module Dev.Tracker.App

open Elmish
open Elmish.React
open Dev.Tracker.UI

let start() =
    Program.mkSimple init update view
    |> Program.withReactBatched "feliz-dev"
    |> Program.run
