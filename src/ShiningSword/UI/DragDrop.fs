module UI.DragDrop
open Feliz
open Feliz.Router
open Fable.Core
open Fable.Core.JsInterop


[<Erase>]
type IContainerProperty =
    interface
        end
[<Erase>]
type ITargetProperty =
    interface
        end
[<Erase>]
type IDragDropProperty = interface
        inherit ITargetProperty
        inherit IContainerProperty
    end
[<Erase>]
type DropEventData =
    {
        dropData: obj option
        dropElem: obj
        dragData: obj option
        target: obj
        preventDefault: unit -> unit
    }

let inline mkDragDropAttr (key: string) (value: obj) : IDragDropProperty = unbox (key, value)
let inline mkContainerAttr (key: string) (value: obj) : IContainerProperty = unbox (key, value)
let inline mkTargetAttr (key: string) (value: obj) : ITargetProperty = unbox (key, value)
type Prop() =
    member inline _.key (key:_) = mkDragDropAttr "key" key
    member inline _.targetKey (key:_) = mkDragDropAttr "targetKey" key
    member inline _.children children = mkDragDropAttr "children" children
    member inline _.dragData data = mkContainerAttr "dragData" data
    member inline _.dropData data = mkTargetAttr "dropData" data
    member inline _.onDrop (f: DropEventData -> unit)  = mkContainerAttr "onDrop" f
let prop = Prop()
let container (props: IContainerProperty list) = Interop.reactApi.createElement(import "DragDropContainer" "react-drag-drop-container", createObj !!props)
let target (props: ITargetProperty list) = Interop.reactApi.createElement(import "DropTarget" "react-drag-drop-container", createObj !!props)
