module UI.PriestSpells
open Feliz

module Impl =
    type Model = { filter: string }
    type Msg = NoOp
    let init() = { filter = "" }
    let update msg model = model
open Impl

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    Html.div [
        Html.h1 "Priest Spells"
        ]