module Main

open App
open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

importSideEffects "./styles/global.scss"

type State = { count: int }
type Msg = Smaller | Bigger
let init _ = { count = 2 }
let update msg state =
    match msg with
    | Smaller -> { state with count = state.count - 1 |> max 1 }
    | Bigger -> { state with count = state.count + 1 }

let render state dispatch =
    let maze = Domain.newMaze(state.count, state.count, false)
    Html.div [
        Maze.render maze
        Html.button [
            prop.text "Smaller"
            prop.onClick (fun _ -> dispatch Smaller)
            ]
        Html.button [
            prop.text "Bigger"
            prop.onClick (fun _ -> dispatch Bigger)
            ]
        ]

Program.mkSimple init update render
|> Program.withReactSynchronous "feliz-app"
|> Program.run
