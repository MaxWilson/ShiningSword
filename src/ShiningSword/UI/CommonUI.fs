[<AutoOpen>]
module UI.CommonUI
open Feliz

let class' (className: string) ctor (elements: ReactElement seq) = ctor [prop.className className; prop.children elements]
let classP' (className: string) ctor (elements: IReactProperty list) = ctor (prop.className className)::elements
let classTxt' (className: string) ctor (txt: string) = ctor [(prop.className className); prop.text txt]
