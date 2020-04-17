module UI.Display
open Fable.Core.JsInterop
open Fable.React.ReactBindings

let pixi: obj = importAll "pixi.js"
let rpf: obj = importAll "react-pixi-fiber"

let bunny props =
    React.createElement(rpf?Text, createObj ["text" ==> props?text], [])

let foo txt =
    React.createElement(rpf?Stage, createObj ["options" ==> (createObj ["backgroundColor" ==> 0x10bb99; "height" ==> 200; "width" ==> 800])], [
        React.createElement(bunny, createObj ["text" ==> txt ], [])
        ])
