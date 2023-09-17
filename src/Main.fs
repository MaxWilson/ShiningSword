module Main

open Feliz
open App
open Browser.Dom
open Fable
open Fable.Core.JsInterop
Fable.Core.JsInterop.importSideEffects "./sass/main.sass"

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Components.Counter())
