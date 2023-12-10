module UI.DFRPG.Chargen.View
open Feliz
open UI.DFRPG.Chargen

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let profession = model.template[0]
    let output = run [profession] DFRPGCharacter.fresh []
    output.toReactElements()
