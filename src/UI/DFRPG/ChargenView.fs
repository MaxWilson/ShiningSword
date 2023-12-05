module UI.DFRPG.Chargen.View
open Feliz
open UI.DFRPG.Chargen
let View() =
    let model, dispatch = React.useElmishSimple init update
    Html.div "placeholder"