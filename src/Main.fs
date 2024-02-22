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
                "priestSpells", "Priest Spells by Sphere", (fun _ -> UI.ADND.PriestSpells.View.View())
                "dfrpgChargen", "Create a character for Dungeon Fantasy RPG", (fun tail -> UI.DFRPG.Chargen.View.View tail)
                "playground", "Ribbit Playground (under construction)", (fun _ -> UI.Ribbit.PlaygroundView.View())
                ]
            let (|Segment|_|) segments =
                match segments with
                | [] -> None
                | head::tail ->
                    lookup |> List.tryPick (fun ((segmentName, _, view) as args) -> if segmentName = head then Some (view, tail) else None)
            match currentUrl with
            | Segment (view, tail) -> view tail
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
