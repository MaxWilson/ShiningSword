module UI.DFRPG.Chargen.View
open Feliz
open UI.DFRPG.Chargen

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let profession = model.template[0]
    // let output = run [profession] DFRPGCharacter.fresh []
    // output.toReactElements()
    shouldntHappen "This error should be caught by React Boundary and displayed in the UI, but it isn't. Or at least, it isn't after the first Dismiss. Instead it triggers some kind of React error about updating during render()."
