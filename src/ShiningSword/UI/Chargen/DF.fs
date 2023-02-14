module UI.Chargen.DF
open Domain.Character.DungeonFantasy
open Feliz
open UI

type Model = Character

let init _ = createRandom NonRandom

type Msg =
    | Reroll of RandomizationMethod
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
    class' "characterHeader" Html.div [
        let char = model.header
        class' "title" Html.div [
            Html.text $"{char.name}"
            ]
        let line (txt:string) = Html.div [prop.text txt]
        let name = char.name
        let sex = char.sex
        let race = model.race
        let profession = professions[model.profession]
        match char.nationalOrigin with
        | "" ->
            line $"\n{name}, {sex} {race} {profession.name}"
        | nation ->
            line $"\n{name}, {sex} {race} {profession.name} from {nation}"
        let empty = (Stats.Create [])
        let showRandomEffects, setShowRandomEffects = React.useState true
        for stat in [ST; DX; IQ; HT] do
            if showRandomEffects && stat.Get model.baseRolls <> stat.Get empty then
                Html.div $"{stat}: (%+d{stat.Get model.baseRolls - stat.Get empty}) {stat.Get model.stats}"
            else
                Html.div $"{stat}: {stat.Get model.stats}"
        checkbox "chkShowRandom" "Display randomization" (showRandomEffects, fun _ -> showRandomEffects |> not |> setShowRandomEffects)
        for stat in [Will; Per; HP; FP] do
            Html.div $"{stat}: {stat.Get model.stats}"
        Html.div $"Speed: %.2f{(SpeedTimesFour.Get model.stats |> float) / 4.0}"
        for stat in [Move; Dodge] do
            Html.div $"{stat}: {stat.Get model.stats}"
        Html.button [prop.text "New name"; prop.onClick (thunk1 dispatch (FwdRoleplaying UI.Roleplaying.RecomputeName))]
        Html.div [
            for prof in professions do
                let chkId = ("chk" + prof.Value.name)
                Html.div [
                    Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked (model.profession = prof.Key); prop.readOnly true; prop.onClick (fun _ -> prof.Key |> ChangeProfession |> dispatch)]
                    Html.label [prop.htmlFor chkId; prop.text prof.Value.name]
                    ]

            ]
        let randomize, setRandomize = React.useState NonRandom

        Html.fieldSet [
            Html.legend "Stat generation"
            checkbox "chkNoRandom" "Nonrandom" (randomize = NonRandom, fun _ -> setRandomize NonRandom)
            checkbox "chkExponential" "Power curve" (randomize = Exponential, fun _ -> setRandomize Exponential)
            checkbox "chk3d6Avg" "Average of 3d6 and 3d6" (randomize = Average3d6, fun _ -> setRandomize Average3d6)
            Html.button [prop.text "Reroll"; prop.onClick (thunk1 dispatch (Reroll randomize))]
            ]
        ]
