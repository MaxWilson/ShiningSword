namespace UI.Components

open Feliz
open Feliz.Router
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
type Color = Red | Green | Blue | Yellow | Grey | Orange | LightGrey | DarkGrey | Black
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
    static member inline opacity (v:float) = mkShapeAttr "opacity" v
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
