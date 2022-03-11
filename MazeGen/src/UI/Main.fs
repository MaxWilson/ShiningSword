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

type State = { size: int * int; maze: Maze; mode: Maze.MouseMode }
type Msg =
    | Smaller | Bigger | Fresh | RandomCarve | RandomPermute | SkipToEnd | Transform of (State -> State)
let fresh ((x,y) as size) = { size = size; maze = Domain.newMaze(x, y, false); mode = Maze.Inactive }
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
        { state with maze = state.maze |> Domain.carve 30 |> normalize }
    | RandomPermute ->
        // permute 20% of interior connections
        { state with maze = state.maze |> Domain.permute 20 |> normalize }
    | Transform f ->
        f state
    | _ -> state

let render state dispatch =
    let modeEvent(mode', coord) =
        match mode', coord with
        | Maze.Erasing, Some(x', y') when Connection(x',y').isValid() ->
            Transform (fun state -> { state with mode = Maze.Erasing; maze = state.maze |> map (fun x y state -> if (x,y) = (x',y') then Open else state) |> normalize })
            |> dispatch
        | Maze.Inactive, _ ->
            Transform (fun state -> { state with mode = Maze.Inactive })
            |> dispatch
        | Maze.Erasing, _ ->
            // we're not on a connection currently (it's a corner or space), but start erasing anyway
            Transform (fun state -> { state with mode = Maze.Erasing })
            |> dispatch
    Html.div [
        Maze.render (state.maze, state.mode, modeEvent)
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
