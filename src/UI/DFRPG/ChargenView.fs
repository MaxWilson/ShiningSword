module UI.DFRPG.Chargen.View
open Feliz
open UI.DFRPG.Chargen

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let profession = swash
    let output = run profession (model.currentOutput |> Option.defaultWith (fun _ -> OfferOutput<_>.fresh DFRPGCharacter.fresh [])) (RefreshedOutput >> dispatch)
    Html.div [
        srcLink
        output.toReactElements()
        ]