module UI.Chargen.DF
open Domain.Ribbit.Properties
open type Eval
open Domain.Character
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Feliz
open UI
open Optics
open type Optics.Operations

type Model =
    {   char: Character
        queue: Map<TraitView.Key, string>
        }

let init constraints =
    {   char = createRandom (defaultArg constraints { Constraints.fresh with randomizationMethod = NonRandom })
        queue = Map.empty }

type Msg =
    | Reroll of Constraints
    | FwdRoleplaying of UI.Roleplaying.Msg
    | ChangeRace of Race
    | ChangeProfession of Profession
    | TraitChange of TraitView.TraitMsg

let update msg model =
    let Char = Lens.create (fun m -> m.char) (fun v m -> { m with char = v })
    let Queue = Lens.create (fun m -> m.queue) (fun v m -> { m with queue = v })
    let Header = Lens.create (fun c -> c.header) (fun v c -> { c with header = v })
    let Profession = Lens.create (fun c -> c.profession) (fun v c -> { c with profession = v })
    match msg with
    | Reroll random -> init (Some random)
    | FwdRoleplaying msg ->
        model |> over (Char => Header) (UI.Roleplaying.update msg)
    | ChangeProfession prof ->
        model |> over Char (changeProfession prof)
    | ChangeRace race ->
        model |> over Char (changeRace race)
    | TraitChange (TraitView.Queue key) ->
        model |> over Queue (Map.add key "")
    | TraitChange (TraitView.QueueData(key,data)) ->
        model |> over Queue (Map.add key data)
    | TraitChange (TraitView.Unqueue key) ->
        model |> over Queue (Map.remove key)
    | TraitChange (TraitView.ClearStrictlyUnder key) ->
        let clearUnder (queue: Map<TraitView.Key,_>) =
            let len = key.Length
            let rec under (k: TraitView.Key) =
                if k = key then true
                elif k.Length > len then under k.Tail
                else false
            let rec strictlyUnder (key': TraitView.Key) =
                if key'.Length > len then // in order to be strictly under, key' has to be longer (more specific) than key
                    under key'.Tail
                else false
            queue |> Map.filter (fun k v -> k |> strictlyUnder |> not)
        model |> over Queue clearUnder

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
    let char = model.char
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
        TraitView.TraitView(profession, char, model.queue, (TraitChange >> dispatch))
        Html.button [prop.text "OK"]
        ]
