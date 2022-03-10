module Main

open App
open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

importSideEffects "./styles/global.scss"

type State = { x: int }
type Msg = Left | Right
let init _ = { x = 200 }
let update msg state =
    match msg with
    | Left -> { state with x = state.x - 100 }
    | Right -> { state with x = state.x + 100 }

let render state dispatch =
    let maze = Domain.newMaze(2,2,false)
    Html.div [
        stage [
            "width" ==> window.innerWidth - 100.
            "height" ==> window.innerHeight - 200.
            "children" ==> [
                layer [
                    "children" ==> [
                        text [
                            "text" ==> sprintf "%A" (maze.connections[1][1])
                            "fontSize" ==> "15"
                            ]
                        circle [
                            "x" ==> state.x
                            "y" ==> "100"
                            "radius" ==> "50"
                            "fill" ==> "green"
                            ]
                        ]
                    ]
                ]
            ]
        Html.button [
            prop.text "Left"
            prop.onClick (fun _ -> dispatch Left)
            ]
        Html.button [
            prop.text "Right"
            prop.onClick (fun _ -> dispatch Right)
            ]
        ]

Program.mkSimple init update render
|> Program.withReactSynchronous "feliz-app"
|> Program.run
