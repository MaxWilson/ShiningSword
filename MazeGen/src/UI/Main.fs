module Main

open App
open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva
open Domain

importSideEffects "./styles/global.scss"

type State = { size: int * int; maze: Maze }
type Msg = Smaller | Bigger | Fresh | RandomCarve | RandomPermute | SkipToEnd
let fresh ((x,y) as size) = { size = size; maze = Domain.newMaze(x, y, false) }
let init _ = fresh (12, 20)
let update msg state =
    match msg with
    | Smaller ->
        let (x,y) = state.size
        fresh (x - 1 |> max 1, y - 1 |> max 1)
    | Bigger ->
        let (x,y) = state.size
        fresh (x + 1, y + 1)
    | Fresh -> fresh state.size
    | RandomCarve ->
        // remove 30% of interior walls
        { state with maze = state.maze |> Domain.carve 30 }
    | RandomPermute ->
        // permute 20% of interior connections
        { state with maze = state.maze |> Domain.permute 20 }
    | _ -> state

let render state dispatch =
    Html.div [
        Maze.render state.maze
        Html.button [
            prop.text "Reset"
            prop.onClick (fun _ -> dispatch Fresh)
            ]
        Html.button [
            prop.text "Carve"
            prop.onClick (fun _ -> dispatch RandomCarve)
            ]
        Html.button [
            prop.text "Permute"
            prop.onClick (fun _ -> dispatch RandomPermute)
            ]
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
