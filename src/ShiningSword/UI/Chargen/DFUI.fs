module UI.Chargen.DF
open Domain.Ribbit.Properties
open type Eval
open Domain.Character
open Domain.Character.DungeonFantasy
open Feliz
open UI

type Model = Character

let init _ = createRandom { Constraints.fresh with randomizationMethod = NonRandom }

type Msg =
    | Reroll of Constraints
    | FwdRoleplaying of UI.Roleplaying.Msg
    | ChangeProfession of Profession

let update msg model =
    match msg with
    | Reroll random -> createRandom random
    | FwdRoleplaying msg ->
        { model with header = model.header |> UI.Roleplaying.update msg }
    | ChangeProfession prof ->
        changeProfession model prof

[<ReactComponent>]
let View model dispatch =
    class' "character" Html.div [
        let char = model.header
        let name = char.name
        let sex = char.sex
        let race = model.race
        let profession = Templates.professions[model.profession]
        match char.nationalOrigin with
        | "" ->
            Html.div [
                classTxt' "title" Html.div name
                classTxt' "subtitle" Html.div $"{sex} {race} {profession.name}"
                ]
        | nation ->
            Html.div [
                classTxt' "title" Html.div name
                classTxt' "subtitle" Html.div $"{sex} {race} {profession.name} from {nation}"
                ]
        let showWork, setShowWork = React.useState true
        let stats = model.stats
        let show (v: int RValue) =
            let total = match v.description with Some d when showWork -> $"{sum v} from {d}" | _ -> $"{sum v}"
            if showWork && v.modifiers.Length > 0 then
                v.modifiers |> List.map (fun (m, reason) -> $" %+d{m} for {reason}") |> String.concat ""
                |> fun s -> $"{total} ({v.baseValue}{s})"
            else $"{total}"
        let showF (v: float RValue) =
            let total = match v.description with Some d when showWork -> $"{sum v} from {d}" | _ -> $"{sum v}"
            if showWork && v.modifiers.Length > 0 then
                v.modifiers |> List.map (fun (m, reason) -> $" %+.2f{m} for {reason}") |> String.concat ""
                |> fun s -> $"{total} ({v.baseValue}{s})"
            else $"{total}"
        for txt, attr, prop in ["ST", stats.ST, ST; "DX", stats.DX, DX; "IQ", stats.IQ, IQ; "HT", stats.HT, HT] do
            Html.div $"{txt}: {prop stats |> show}"
        for txt, stat in ["Will", Will; "Per", Per; "HP", HP; "FP", FP] do
            Html.div $"{txt}: {stat model.stats |> show}"
        Html.div $"Speed: {Speed model.stats |> showF}"
        for txt, stat in ["Move", Move; "Dodge", Dodge] do
            Html.div $"{txt}: {stat model.stats |> show}"
        checkbox "chkShowWork" "Show stat derivation" (showWork, fun _ -> showWork |> not |> setShowWork)
        Html.button [prop.text "New name"; prop.onClick (thunk1 dispatch (FwdRoleplaying UI.Roleplaying.RecomputeName))]
        Html.div [
            for prof in Templates.professions do
                let chkId = ("chk" + prof.Value.name)
                Html.div [
                    Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked (model.profession = prof.Key); prop.readOnly true; prop.onClick (fun _ -> prof.Key |> ChangeProfession |> dispatch)]
                    Html.label [prop.htmlFor chkId; prop.text prof.Value.name]
                    ]

            ]
        let randomize, setRandomize = React.useState NonRandom
        let racePreference, setRacePreference = React.useState None
        let sexPreference, setSexPreference = React.useState None
        let nationPreference, setNationPreference = React.useState None
        let selection (label:string) (options: _ seq) display (current, set) =
            class' "selection" Html.div [
                classTxt' "subtitle" Html.span label
                checkbox ("chk" + label + "Any") "Flexible" (current = None, fun _ -> set None)
                for o in options do
                    let txt = (display o)
                    checkbox ("chk" + label + txt) txt (current = Some o, fun _ -> set (Some o))
                ]
        Html.fieldSet [
            Html.legend "Stat generation"
            checkbox "chkNoRandom" "Nonrandom" (randomize = NonRandom, fun _ -> setRandomize NonRandom)
            checkbox "chkExponential" "Power curve" (randomize = Exponential, fun _ -> setRandomize Exponential)
            checkbox "chk3d6Avg" "Average of 3d6 and 3d6" (randomize = Average3d6, fun _ -> setRandomize Average3d6)
            selection "Race" (Templates.races |> List.map snd) (fun r -> r.name) (racePreference, setRacePreference)
            selection "Sex" [Male; Female] (sprintf "%A") (sexPreference, setSexPreference)
            let nations = Onomastikon.nameLists.Keys |> Seq.map fst |> Seq.distinct
            selection "Origin" nations id (nationPreference, setNationPreference)
            Html.button [prop.text "Reroll"; prop.onClick (thunk1 dispatch (Reroll { Constraints.fresh with randomizationMethod = randomize; race = racePreference; sex = sexPreference; nationPreference = nationPreference }))]
            ]
        ]
