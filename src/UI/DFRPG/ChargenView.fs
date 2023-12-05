module UI.DFRPG.Chargen.View
open Feliz
open UI.DFRPG.Chargen

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    Html.div "placeholder"