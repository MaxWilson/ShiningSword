module Main

open Feliz
open App
open Browser.Dom
open Fable
open Fable.Core.JsInterop
open Common.UI
importSideEffects "./sass/main.sass"

let main() =
    Html.div [
        classP' "srcLink" Html.a [
            prop.href "https://github.com/MaxWilson/POCArena/"
            prop.children [Html.img [prop.src "img/GitHub_Logo.png"]]
            prop.target "_blank"
            ]
        Components.Counter()
        ]

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(main())
