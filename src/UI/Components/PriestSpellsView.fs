// separate from UI.Priestspells for the sake of HMR.
module UI.PriestSpellsView

open Feliz
open UI.PriestSpells

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let filter, setFilter = React.useState ""
    Html.div [
        Html.h1 "Priest Spells"
        Html.input [
            prop.value filter
            prop.placeholder "Spell name, sphere or deity"
            prop.onChange (fun txt -> setFilter txt)
            ]
        Html.ul [
            for spell in filteredSpells filter model do
                Html.li [
                    Html.span [
                        prop.text (spell.ToString())
                        ]
                    Html.span [
                        prop.text (match model.picks.TryFind(spell.name) with Some(n) -> $" ({n})" | None -> "")
                        ]
                    ]
            ]
        ]
