// separate from UI.Priestspells for the sake of HMR.
module UI.PriestSpellsView

open Feliz
open UI.PriestSpells

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let filter, setFilter = React.useState ""
    class' "mainPage horizontalStack" Html.div [
        class' "scrollParent" Html.div [
            Html.h1 "Priest Spells"
            Html.input [
                prop.value filter
                prop.placeholder "Spell name, sphere or deity"
                prop.onChange (fun txt -> setFilter txt)
                ]
            class' "scrollable" Html.ul [
                let spells = filteredSpells filter model |> List.groupBy _.level |> List.sortBy fst
                for level, spells in spells do
                    let ordinalToText = function
                        | 1 -> "1st"
                        | 2 -> "2nd"
                        | 3 -> "3rd"
                        | n -> sprintf "%dth" n
                    Html.h2 [prop.text (ordinalToText level)]
                    for spell in spells |> List.sort do
                        Html.li [
                            Html.span [
                                prop.text ($"""{spell.name} ({spell.spheres |> String.join "/"})""")
                                ]
                            Html.span [
                                prop.text (match model.picks.TryFind(spell.name) with Some(n) -> $" ({n})" | None -> "")
                                ]
                            ]
                ]
            ]
        class' "scrollParent" Html.div [
            srcLink
            Html.h2 "Worship"
            class' "scrollable" Html.ul [
                for deity in filteredDeities filter model do
                    Html.li [prop.text (deity.name + ": " + String.join ", " [for sphere in deity.spheres -> (sphere.sphere + if sphere.access = Minor then "*" else "")])]
                ]
            ]
        ]
