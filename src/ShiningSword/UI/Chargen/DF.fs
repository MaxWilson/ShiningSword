module UI.Chargen.DF
open Domain.Character.DungeonFantasy
open Feliz
open UI

type Model = Character

let init _ = createRandom true

type Msg =
    | Reroll of randomize: bool
    | FwdRoleplaying of UI.Roleplaying.Msg

let update msg model =
    match msg with
    | Reroll random -> createRandom random
    | FwdRoleplaying msg ->
        { model with header = model.header |> UI.Roleplaying.update msg }

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
        Html.div [
            Html.input [prop.id "chkShowEffects"; prop.type'.checkbox;
                        prop.isChecked showRandomEffects; prop.readOnly true;
                        prop.onClick (fun _ -> showRandomEffects |> not |> setShowRandomEffects)]
            Html.label [prop.htmlFor "chkShowEffects"; prop.text "Display randomization"]
            ]
        for stat in [Will; Per; HP; FP] do
            Html.div $"{stat}: {stat.Get model.stats}"
        Html.div $"Speed: %.2f{(SpeedTimesFour.Get model.stats |> float) / 4.0}"
        for stat in [Move; Dodge] do
            Html.div $"{stat}: {stat.Get model.stats}"
        Html.button [prop.text "New name"; prop.onClick (thunk1 dispatch (FwdRoleplaying UI.Roleplaying.RecomputeName))]
        let randomize, setRandomize = React.useState true
        Html.div [
            Html.button [prop.text "Reroll"; prop.onClick (thunk1 dispatch (Reroll randomize))]
            Html.input [prop.id "chkRandomize"; prop.type'.checkbox; prop.isChecked randomize; prop.readOnly true; prop.onClick (fun _ -> randomize |> not |> setRandomize)]
            Html.label [prop.htmlFor "chkRandomize"; prop.text "Randomize stats"]
            ]
        ]

        //printfn $"\n{name}, {sex} {race.name} {profession.name} from {nation}"
        //    for stat in [ST; DX; IQ; HT; Will; Per; HP; FP; Move; SpeedTimesFour; Dodge] do
        //        match stat with
        //        | SpeedTimesFour ->
        //            printf  $"Speed: {(SpeedTimesFour.Get stats |> float) / 4.0}  "
        //        | _ ->
        //            printf  $"{stat}: {stat.Get stats}  "
