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
            let lookup = [
                "priestSpells", "Priest Spells by Sphere", (fun () -> UI.ADND.PriestSpells.View.View())
                "dfrpgChargen", "Create a character for Dungeon Fantasy RPG", (fun () -> UI.DFRPG.Chargen.View.View())
                "dfrpgChargen/swash", "Create a swashbuckler for Dungeon Fantasy RPG", (fun () -> UI.DFRPG.Chargen.View.View())
                "playground", "Ribbit Playground (under construction)", (fun () -> UI.Ribbit.PlaygroundView.View())
                ]
            let (|Segment|_|) segments =
                lookup |> List.tryPick (fun ((rootTag, _, _) as args) -> if [rootTag] = segments then Some args else None)
            match currentUrl with
            | Segment (_, _, view) -> view()
            | otherwise ->
                class' "mainPage" Html.div [
                    srcLink
                    Html.h1 "Shining Sword RPG apps"
                    for (segment, name, _) in lookup do
                        Html.a [prop.text name; prop.href ("#" + segment)] |> List.singleton |> Html.div
                    ]
            ]
        ]

let main() =
    ReactErrorBoundary.renderCatchSimple (Router())

let root = ReactDOM.createRoot(document.getElementById "feliz-app")
root.render(Html.div [ main() ])
