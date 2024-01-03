// separate from UI.Priestspells for the sake of HMR.
module UI.ADND.PriestSpells.View

open Feliz
open Domain.ADND.PriestSpells

module private Impl =
    open UI
    type Options = { spells: Spell list; notes: Map<SpellName, string>; spheres: Sphere list; deities: Deity list }
    type Model = { options: Options; picks: Map<SpellName, int> }
    type Msg = NoOp
    let init() =
        let spheres = LocalStorage.Spheres.read()
        let options = { spells = consolidateSpells spheres; notes = LocalStorage.Notes.read(); spheres = spheres; deities = LocalStorage.Deities.read() }
        { options = options; picks = LocalStorage.SpellPicks.read() }
    let update msg model = model

    let filteredSpells (filter: string) (options: Options) =
        match filter.Trim() with
        | "" -> options.spells
        | filter ->
            let fragments = filter.Split(' ') |> List.ofArray
            let grantorsBySphere =
                [
                for sphere in options.spheres do
                    let grantors = [
                        for d in options.deities do
                            match d.spheres |> List.tryFind (fun s -> String.equalsIgnoreCase s.sphere sphere.name) with
                            | Some v -> d.name, v.access
                            | None -> ()
                        ]
                    sphere.name, grantors
                ]
                |> Map.ofList
            let isMatch (spell: Spell) fragment =
                if String.containsIgnoreCase(spell.ToString()) fragment then true
                else spell.spheres |> List.exists (fun sphere -> grantorsBySphere[sphere] |> List.exists (fun (deity, access) -> String.containsIgnoreCase deity fragment && (spell.level <= 3 || access = Major)))
            options.spells |> List.filter (fun spell -> fragments |> List.every (isMatch spell))

    let filteredDeities (filter: string) (options: Options) =
        match filter.Trim() with
        | "" -> options.deities
        | filter ->
            let fragments = filter.Split(' ') |> List.ofArray
            let matchingDeities = options.deities |> List.filter (fun deity -> fragments |> List.exists (fun fragment -> String.containsIgnoreCase deity.name fragment || deity.spheres |> List.exists (fun sphere -> String.containsIgnoreCase sphere.sphere fragment)))
            match matchingDeities with
            | [] -> options.deities // they must not be trying to filter by deity
            | lst -> lst

open Impl

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
                let spells = filteredSpells filter model.options |> List.groupBy _.level |> List.sortBy fst
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
            class' "fullWidth" Html.div [
                srcLink
                Html.h2 "Worship"
                ]
            class' "scrollable" Html.ul [
                for deity in filteredDeities filter model.options do
                    Html.li [prop.text (deity.name + ": " + String.join ", " [for sphere in deity.spheres -> (sphere.sphere + if sphere.access = Minor then "*" else "")])]
                ]
            ]
        ]
