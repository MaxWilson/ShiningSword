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
            match currentUrl with
            | [ "spells" ] -> UI.PriestSpells.View()
            | otherwise ->
                UI.PriestSpells.View()
            ]
        ]

let main() =
    ReactErrorBoundary.renderCatchSimple ReactErrorBoundary.err <| Router()

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
