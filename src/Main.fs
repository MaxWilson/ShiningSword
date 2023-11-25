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
                    prop.href "https://github.com/MaxWilson/ShiningSword/"
                    prop.children [Html.img [prop.ariaLabel "GitHub"; prop.src "img/GitHub_Logo.png"]]
                    prop.target "_blank"
                    ]
                ]
            let lookup = [
                "priestSpells", "Priest Spells by Sphere", (fun () -> UI.PriestSpellsView.View())
                ]
            let (|Segment|_|) segment =
                lookup |> List.tryFind (fun (s, _, _) -> s = segment)
            match currentUrl with
            | [ Segment (_, _, view) ] -> view()
            | otherwise ->
                class' "mainPage" Html.div [
                    Html.h1 "Shining Sword RPG apps"
                    for (segment, name, _) in lookup do
                        Html.a [prop.text name; prop.href ("#" + segment)]
                    ]
            ]
        ]

let main() =
    ReactErrorBoundary.renderCatchSimple ReactErrorBoundary.err <| Router()

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
