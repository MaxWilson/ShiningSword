module UI.Chargen.Components
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Fable.React
open Feliz
open Fable.Core
open UI
open UI.Chargen

[<AutoOpen>]
module TraitView =
    open Builders
    [<ReactComponent>]
    let TraitView (profession: Templates.Package<Profession>, char: Character, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
        let builder = reactBuilder(ReactCtx.create(char, queue, dispatch))
        class' "traitview" Html.fieldSet [
            classTxt' "subtitle" Html.legend profession.displayName
            (Templates.menusFor profession.name |> builder)
            ]

module DF =
    open Domain.Ribbit.Properties
    open type Domain.Ribbit.Properties.Eval
    open Domain.Character.Core
    open UI.Chargen.DF
    [<ReactComponent>]
    let View (mkHeader: _ -> ReactElement) model dispatch =
        let char = model.char

        class' "character" Html.div [
            let showRerollPreferences, setShowRerollPreferences = React.useState true
            let randomize, setRandomize = React.useState NonRandom
            let (raceConstraintOrPreference: Race Templates.Package option), setRaceConstraintOrPreference = React.useState None
            let professionPreference, setProfessionPreference = React.useState None
            let sexConstraint, setSexConstraint = React.useState Arbitrary
            let nationPreference, setNationPreference = React.useState None
            let rerollSection =
                let toPreference = function
                    | Some r -> Some (if randomize = NonRandom then Prefer r else Require r)
                    | None when randomize = NonRandom -> None
                    | None -> // force a random race
                        (Templates.races |> List.map snd) |> chooseRandom |> Require |> Some
                let rerollButton = Html.button [prop.text "Reroll"; prop.onClick (thunk1 dispatch (Reroll { Constraints.fresh with randomizationMethod = randomize; race = raceConstraintOrPreference |> toPreference; professionPreference = professionPreference; sex = sexConstraint; nationPreference = nationPreference }))]
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
                        selection "Race" (Templates.races |> List.map snd) (fun r -> r.name |> Helpers.raceName) (raceConstraintOrPreference, setRaceConstraintOrPreference)

                        selection "Sex" [Specific Male; Specific Female] (function Specific sex -> sprintf "%A" sex | _ -> "Random") (sexConstraint |> Some, (function Some (Specific v) -> Specific v | _ -> Arbitrary) >> setSexConstraint)
                        selection "Profession" Templates.professions.Keys Helpers.professionName (professionPreference, setProfessionPreference)
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
            let rp = char.header
            let name = rp.name
            let sex = rp.sex
            let race = char.race
            let profession = Templates.professions[char.profession]
            match rp.nationalOrigin with
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
            let stats = char.stats
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
                for txt, prop in ["Move", Move; "Dodge", Dodge; "SM", SM] do
                    show(txt, prop stats)
                ]
            checkbox "chkShowWork" "Show stat derivation" (showWork, fun _ -> showWork |> not |> setShowWork)
            if char.canChangeRace then
                class' "selection" Html.fieldSet [
                    classTxt' "subtitle" Html.legend "Race"
                    for race in Templates.races |> List.map snd do
                        let txt = race.name |> Helpers.raceName
                        let isChecked = char.race = race.name
                        checkbox ("chkCurrent" + txt) txt (isChecked, fun _ -> (if not isChecked then dispatch (ChangeRace race.name)))
                    ]

            Html.fieldSet [
                classTxt' "subtitle" Html.legend "Profession"
                class' "gridContainer selection" Html.div [
                    for prof in Templates.professions do
                        let chkId = ("chk" + prof.Value.displayName)
                        Html.div [
                            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked (char.profession = prof.Key); prop.onChange (fun select -> if select then prof.Key |> ChangeProfession |> dispatch)]
                            Html.label [prop.htmlFor chkId; prop.text prof.Value.displayName]
                            ]

                    ]
                Html.hr []
                ]
            TraitView(profession, char, model.queue, (TraitChange >> dispatch))
            match model.practiceFight with
            | None ->
                Html.button [prop.text "Practice fight"; prop.onClick (fun _ -> goblinFight model.char |> Some |> PracticeFight |> dispatch)]
            | Some practiceFight ->
                // Okay, so what's the problem right now? The immediate problem, which
                // may be a sidetrack, is that we're trying to display unlabeled
                // things like the ST function. Probably we should ignore this and focus
                // on more important things like getting the actual fight working.
                class' "statsTable" Html.table [
                    let stat (f: _ -> int RValue) (c:AdHoc.Combatant) = f c.stats |> sum
                    let props = ["ST", stat ST; "DX", stat DX; "IQ", stat IQ; "HT", stat HT; "HP", AdHoc.CurrentHP]
                    Html.thead [
                        Html.tr [
                            Html.th "Name"
                            for txt, prop in props do
                                Html.th txt
                            Html.th "Recent"
                            Html.th "Current action"
                            ]
                        ]
                    Html.tbody [
                        for combatant in practiceFight |> Helpers.combatants |> Seq.sortBy(fun c -> c.team)  do
                            let teamClass = if combatant.team = 0 then "teamFriendly" else "teamHostile"
                            class' teamClass Html.tr [
                                Html.td [prop.text combatant.name; prop.className "name"]
                                for txt, prop in props do
                                    Html.td (combatant |> prop |> toString)
                                let txtProperty propertyName otherwise =
                                    combatant.scratchPad |> Map.tryFind propertyName |> function Some (AdHoc.Text t) -> t | _ -> otherwise
                                Html.td (txtProperty "AdHocLastRoundMsg" "")
                                Html.td (txtProperty "AdHocIntention" "Attacking")
                                ]
                        ]
                    ]
                Html.button [prop.text "Execute"; prop.onClick(fun _ -> fightOneRound practiceFight |> Some |> PracticeFight |> dispatch)]
                Html.button [prop.text "Cancel fight"; prop.onClick(fun _ -> None |> PracticeFight |> dispatch)]
            Html.button [prop.text "OK"]
            ]
