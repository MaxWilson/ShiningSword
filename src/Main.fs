module Main

open Feliz
open Feliz.Router
open Browser.Dom
open Fable
open Fable.Core.JsInterop

importSideEffects "../main.sass"


[<ReactComponent>]
let Router() =
    let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
    React.router [
        router.onUrlChanged updateUrl
        router.children [
            class' "header" Html.div [
                classP' "srcLink" Html.a [
                    prop.href "https://github.com/MaxWilson/rpg/"
                    prop.children [Html.img [prop.ariaLabel "GitHub"; prop.src "img/GitHub_Logo.png"]]
                    prop.target "_blank"
                    ]
                ]
            match currentUrl with
            | [ "spells" ] -> UI.PriestSpellsView.View()
            | otherwise ->
                UI.PriestSpellsView.View()
            ]
        ]

let main() =
    ReactErrorBoundary.renderCatchSimple ReactErrorBoundary.err <| Router()

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
