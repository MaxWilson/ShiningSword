module UI.Chargen.DF
open Domain.Ribbit.Properties
open type Eval
open Domain.Character
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Feliz
open UI

type Model = Character

let init _ = createRandom { Constraints.fresh with randomizationMethod = NonRandom }

type Msg =
    | Reroll of Constraints
    | FwdRoleplaying of UI.Roleplaying.Msg
    | ChangeRace of Race
    | ChangeProfession of Profession
    | TraitChange of TraitView.TraitMsg

let update msg model =
    match msg with
    | Reroll random -> createRandom random
    | FwdRoleplaying msg ->
        { model with header = model.header |> UI.Roleplaying.update msg }
    | ChangeProfession prof ->
        changeProfession model prof
    | ChangeRace race ->
        changeRace model race
    | TraitChange (TraitView.Add t) ->
        { model with traits = t :: model.traits }
    | TraitChange (TraitView.Remove t) ->
        { model with traits = model.traits |> List.filter ((<>) t) }

[<AutoOpen>]
module Helpers =
    let raceName = function
        | Catfolk -> "Cat-folk"
        | HalfElf -> "Half-elf"
        | HalfOgre -> "Half-ogre"
        | HalfOrc -> "Half-orc"
        | v -> v.ToString()

[<ReactComponent>]
let View (mkHeader: _ -> ReactElement) model dispatch =
    class' "character" Html.div [
        let showRerollPreferences, setShowRerollPreferences = React.useState true
        let randomize, setRandomize = React.useState NonRandom
        let (raceConstraint: Race Templates.Package Constraint option), setRaceConstraint =
            React.useState None
            |> Tuple2.mapfst (function None when randomize <> NonRandom -> Some Arbitrary | _ when randomize = NonRandom -> None | v -> v)
        let sexConstraint, setSexConstraint = React.useState Arbitrary
        let nationPreference, setNationPreference = React.useState None
        let rerollSection =
            let rerollButton = Html.button [prop.text "Reroll"; prop.onClick (thunk1 dispatch (Reroll { Constraints.fresh with randomizationMethod = randomize; race = raceConstraint; sex = sexConstraint; nationPreference = nationPreference }))]
            let renameButton = Html.button [prop.text "New name"; prop.onClick (thunk1 dispatch (FwdRoleplaying UI.Roleplaying.RecomputeName))]

            if showRerollPreferences then
                class' "reroll" Html.fieldSet [
                    Html.legend "Reroll settings"
                    Html.div [
                        Html.button [prop.text "-"; prop.onClick (thunk1 setShowRerollPreferences false)]
                        rerollButton
                        renameButton
                        ]
                    let selection (label:string) (options: _ seq) display (current, set) =
                        class' "selection" Html.fieldSet [
                            classTxt' "subtitle" Html.legend label
                            for o in options do
                                let txt = (display o)
                                let isChecked = current = Some o
                                checkbox ("chk" + label + txt) txt (isChecked, fun _ -> set (if isChecked then None else Some o))
                            ]
                    Html.fieldSet [
                        Html.legend "Stat generation"
                        checkbox "chkNoRandom" "Nonrandom" (randomize = NonRandom, fun _ -> setRandomize NonRandom)
                        checkbox "chkExponential" "Power curve" (randomize = Exponential, fun _ -> setRandomize Exponential)
                        checkbox "chk3d6Avg" "Average of 3d6 and 3d6" (randomize = Average3d6, fun _ -> setRandomize Average3d6)
                        ]
                    if randomize <> NonRandom then
                        selection "Race" (Templates.races |> List.map (snd >> Specific)) (function Specific r -> r.name |> Helpers.raceName | _ -> "Random") (raceConstraint, setRaceConstraint)

                    selection "Sex" [Specific Male; Specific Female] (function Specific sex -> sprintf "%A" sex | _ -> "Random") (sexConstraint |> Some, (function Some (Specific v) -> Specific v | _ -> Arbitrary) >> setSexConstraint)
                    let nations = Onomastikon.nameLists.Keys |> Seq.map fst |> Seq.distinct
                    selection "Origin" nations id (nationPreference, setNationPreference)
                    ]
            else
                class' "reroll" Html.section [
                    Html.button [prop.text "+"; prop.onClick (thunk1 setShowRerollPreferences true)]
                    rerollButton
                    renameButton
                    ]
        mkHeader rerollSection
        let char = model.header
        let name = char.name
        let sex = char.sex
        let race = model.race
        let profession = Templates.professions[model.profession]
        match char.nationalOrigin with
        | "" ->
            Html.div [
                classTxt' "title" Html.div name
                classTxt' "subtitle" Html.div $"{sex} {race |> raceName} {profession.name}"
                ]
        | nation ->
            Html.div [
                classTxt' "title" Html.div name
                classTxt' "subtitle" Html.div $"{sex} {race |> raceName} {profession.name} from {nation}"
                ]
        let showWork, setShowWork = React.useState true
        let stats = model.stats
        let vwAttribute (elements: _ seq) = Html.div elements
        let show (txt, v: int RValue) =
            let total = sum v
            if showWork && (v.description.IsSome || v.modifiers.Length > 0) then
                (v.modifiers |> List.map (fun (m, reason) -> $" %+d{m} for {reason}"))
                |> List.append [match v.description with Some d -> $" from {d}" | _ -> ()]
                |> String.concat ""
                |> fun s ->
                    vwAttribute [
                        Html.span $"{txt}: {total}"
                        classTxt' "footnote" Html.span $"{v.baseValue}{s}"
                        ]
            else vwAttribute [Html.text $"{txt}: {total}"]
        let showF (txt, v: float RValue) =
            let total = sum v
            if showWork && (v.description.IsSome || v.modifiers.Length > 0) then
                v.modifiers |> List.map (fun (m, reason) -> $" %+.2f{m} for {reason}")
                |> List.append [match v.description with Some d -> $" from {d}" | _ -> ()]
                |> String.concat ""
                |> fun s ->
                    vwAttribute [
                        Html.span $"{txt}: {total}"
                        classTxt' "footnote" Html.span $"{v.baseValue}{s}"
                        ]
            else vwAttribute [Html.text $"{txt}: %.2f{total}"]
        class' "gridContainer" Html.div [
            for txt, attr, prop in ["ST", stats.ST, ST; "DX", stats.DX, DX; "IQ", stats.IQ, IQ; "HT", stats.HT, HT] do
                show(txt, prop stats)
            for txt, prop in ["Will", Will; "Per", Per; "HP", HP; "FP", FP] do
                show(txt, prop stats)
            showF("Speed", Speed stats)
            for txt, prop in ["Move", Move; "Dodge", Dodge; "SZ", SM] do
                show(txt, prop stats)
            ]
        checkbox "chkShowWork" "Show stat derivation" (showWork, fun _ -> showWork |> not |> setShowWork)
        if model.canChangeRace then
            class' "selection" Html.fieldSet [
                classTxt' "subtitle" Html.legend "Race"
                for race in Templates.races |> List.map snd do
                    let txt = race.name |> Helpers.raceName
                    let isChecked = model.race = race.name
                    checkbox ("chkCurrent" + txt) txt (isChecked, fun _ -> (if not isChecked then dispatch (ChangeRace race.name)))
                ]

        Html.fieldSet [
            Html.legend "Profession"
            class' "gridContainer" Html.div [
                for prof in Templates.professions do
                    let chkId = ("chk" + prof.Value.displayName)
                    Html.div [
                        Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked (model.profession = prof.Key); prop.onChange (fun select -> if select then prof.Key |> ChangeProfession |> dispatch)]
                        Html.label [prop.htmlFor chkId; prop.text prof.Value.displayName]
                        ]

                ]
            Html.hr []
            ]
        Html.fieldSet [
            Html.legend profession.displayName
            if profession.name = Swashbuckler then
                swash (TraitView.ReactBuilder(model, (TraitChange >> dispatch)))
            ]

        ]