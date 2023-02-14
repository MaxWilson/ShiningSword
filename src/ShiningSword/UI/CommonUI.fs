[<AutoOpen>]
module UI.CommonUI
open Feliz

let class' (className: string) ctor (elements: ReactElement seq) = ctor [prop.className className; prop.children elements]
let classP' (className: string) ctor (elements: IReactProperty list) = ctor (prop.className className)::elements
let classTxt' (className: string) ctor (txt: string) = ctor [(prop.className className); prop.text txt]

let checkbox (id: string) (text:string) (isChecked, onClick) =
    Html.section [
        Html.input [prop.id id; prop.type'.checkbox; prop.isChecked isChecked; prop.readOnly true; prop.onClick (onClick)]
        Html.label [prop.htmlFor id; prop.text text]
        ]
