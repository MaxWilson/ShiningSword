namespace UI.Components

open Feliz
open Feliz.Router

type Components =

    static member rand = System.Random()
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
            ]
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                match currentUrl with
                | [ ] -> Html.h1 $"Index: {currentUrl}"
                | [ "hello" ] -> Components.HelloWorld()
                | [ "counter" ] -> Components.Counter()
                | otherwise ->
                    let messages = ["Not found, y'all"; "sorry, couldn't find it"; "Nope, try again"]
                    Html.h1 (messages[Components.rand.Next(3)])
            ]
        ]

open Fable.Core
open Fable.Core.JsInterop
module Konva =

    [<Erase>]
    type ICircleProperty =
        interface end
    [<Erase>]
    type IRectProperty =
        interface end
    [<Erase>]
    type IShapeProperty =
        interface
            inherit ICircleProperty
            inherit IRectProperty
            end


    module Interop =
        let inline mkShapeAttr (key: string) (value: obj) : IShapeProperty = unbox (key, value)
        let inline mkCircleAttr (key: string) (value: obj) : ICircleProperty = unbox (key, value)
        let inline mkRectAttr (key: string) (value: obj) : IRectProperty = unbox (key, value)
    let stage props = Interop.reactApi.createElement(import "Stage" "react-konva", createObj !!props)
    let layer props = Interop.reactApi.createElement(import "Layer" "react-konva", createObj !!props)
    let circle (props: ICircleProperty list) = Interop.reactApi.createElement(import "Circle" "react-konva", createObj !!props)
    let rect (props: IRectProperty list) = Interop.reactApi.createElement(import "Rect" "react-konva", createObj !!props)
    let text props = Interop.reactApi.createElement(import "Text" "react-konva", createObj !!props)
[<Erase>]
type Color = Red | Green | Blue | Yellow | Grey | Orange
open Konva.Interop

type Shape =
    static member inline key (key:_) = mkShapeAttr "key" key
    static member inline width (w:int) = mkShapeAttr "width" w
    static member inline height (h:int) = mkShapeAttr "height" h
    static member inline width (w:float) = mkShapeAttr "width" w
    static member inline height (h:float) = mkShapeAttr "height" h
    static member inline x (x:int) = mkShapeAttr "x" x
    static member inline y (y:int) = mkShapeAttr "y" y
    static member inline x (x:float) = mkShapeAttr "x" x
    static member inline y (y:float) = mkShapeAttr "y" y
    static member inline fill (color:Color) = mkShapeAttr "fill" color
    static member inline fill (color:string) = mkShapeAttr "fill" color
    static member draggable = mkShapeAttr "draggable" true
    static member inline onDragStart (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onDragStart" f
    static member inline onDragEnd (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onDragEnd" f
    static member inline onClick (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onClick" f
    static member inline onMouseDown (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onMouseDown" f
    static member inline onMouseUp (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onMouseUp" f
    static member inline onMouseOver (f: ({| target: 'a |} -> 'b)) = mkShapeAttr "onMouseOver" f

type Circle =
    inherit Shape
    static member inline radius (r:float) = mkCircleAttr "radius" r
[<Erase>]
type LineJoin = Miter | Round | Bevel

type Rect =
    inherit Shape
    static member inline lineJoin (lineJoin:LineJoin) = mkRectAttr "lineJoin" lineJoin

open Konva

module Maze =
    open Domain
    open Fable.Core.JsInterop
    type MouseMode = CarvingSpace | BuildingWalls | Inactive
    let nearestIntersection (xPixel, yPixel) =
        let gridSize = 20
        let maybe (xPixel, yPixel) =
            let x,y = xPixel / gridSize, yPixel / gridSize
            let candidate = Connection(x,y)
            if candidate.isValid() then Some(candidate)
            else None
        match maybe (xPixel, yPixel) with
        | Some x -> x
        | None ->
            // we're going to try everything within 1 manhattan distance square of the
            // current grid square, see which is closest to our actual point
            // first, we normalize the coordinates to the approximate CENTER of the grid square x and y are in

            // now, get all the neighboring squares and sort them by actual distance from x and y
            let candidates =
                [
                    let square x = x * x |> float
                    let centerX, centerY = xPixel / gridSize * gridSize + gridSize/2, yPixel / gridSize * gridSize + gridSize/2
                    for xMod in [-gridSize;0;gridSize] do
                        for yMod in [-gridSize;0;gridSize] do
                            let gridCenter = (centerX+xMod, centerY+yMod)
                            match maybe gridCenter with
                            | Some candidate -> sqrt((square((gridCenter |> fst) - xPixel)) + (square((gridCenter |> snd) - yPixel))), candidate
                            | None -> ()
                    ]
                |> List.sortBy fst
            candidates.Head |> snd

    let render (maze: Maze, mode: MouseMode, modeChange) =
        let window = Browser.Dom.window;
        let isRightClick e = e?evt?button = 2
        stage [
            "width" ==> window.innerWidth - 100.
            "height" ==> window.innerHeight - 100.
            "children" ==> [
                layer [
                    "children" ==> [
                        let zip a b = a,b
                        for y, row in maze.grid |> Array.mapi zip do
                            for x, cell in row |> Array.mapi zip do
                                if cell = Closed then
                                    rect [
                                        Rect.x (x * 20)
                                        Rect.y (y * 20)
                                        Rect.height 20
                                        Rect.width 20
                                        Rect.fill Grey
                                        Shape.key (x,y)
                                        match mode with
                                        | Inactive ->
                                            Rect.onMouseDown (fun e -> modeChange((if isRightClick e then BuildingWalls else CarvingSpace), Some(Connection(x, y))))
                                        | CarvingSpace ->
                                            Rect.onMouseOver (fun e -> modeChange(CarvingSpace, Some(Connection(x, y))))
                                        | BuildingWalls ->
                                            ()
                                        ]
                        ]
                    ]
                ]
            "onMouseUp" ==> fun _ -> modeChange(Inactive, None)
            "onMouseEnter" ==> fun _ -> modeChange(Inactive, None)
            "onMouseDown" ==> fun e ->
                let pos = e?target?getStage()?getPointerPosition()
                let pos = nearestIntersection(pos?x, pos?y)
                if isRightClick e then
                    modeChange(BuildingWalls, Some(pos))
                else
                    modeChange(CarvingSpace, Some(pos))
            "onContextMenu" ==> fun e -> e?evt?preventDefault()
            if mode = BuildingWalls then
                "onMouseOver" ==> fun e ->
                    let pos = e?target?getStage()?getPointerPosition()
                    let pos = nearestIntersection(pos?x, pos?y)
                    modeChange(BuildingWalls, Some(pos))
            ]
