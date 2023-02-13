namespace UI.Chargen
open UI
// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.

module View =
    open Elmish
    open Domain
    open Domain.Character
    open DND5e
    open Feliz
    open Core.DerivedTraits
    open Domain.Character.Universal


    type Ruleset = TSR | WotC | DungeonFantasy
    type Model = {
        ruleset: Ruleset
        dfChar: DF.Model
        }
    type ParentMsg =
        | SaveAndQuit of CharacterSheet
        | BeginAdventuring of CharacterSheet
        | Cancel
        | UpdateUrl of suffix: string
    type Msg =
        | Reroll
        | SetRuleset of Ruleset
        | FwdDungeonFantasy of DF.Msg
    let rec init _ =
        let sex = chooseRandom [Male; Female]
        let nationalOrigin, name = makeNameAnyNation sex

        {
            ruleset = DungeonFantasy
            dfChar = DF.init()
            } |> reroll
    and reroll model =
        match model.ruleset with
        | _ ->
            model

    let update cmd informParent msg model =
        match msg with
        | Reroll ->
            reroll model, Cmd.Empty
        | SetRuleset ruleset ->
            match ruleset with
            | Ruleset.DungeonFantasy -> (), "df"
            | _ -> (), "tsr"
            |> fun (method, urlFragment) ->
                { model with ruleset = ruleset },
                    Cmd.batch [
                        //SetMethod method |> cmd
                        informParent (UpdateUrl urlFragment)]
        | FwdDungeonFantasy msg ->
            match model.ruleset with
            | DungeonFantasy ->
                { model with dfChar = DF.update msg model.dfChar }, []
            | _ -> shouldntHappen()
    let getPercentile =
        // for a given stat like 18, how many people have a lower stat?
        let normalPersonDistribution =
            [
                for x in 1..6 do
                    for y in 1..6 do
                        for z in 1..6 do
                            x+y+z
                ]
        let lessThanEqualGroups =
            [
                for x in 1..25 do
                    let count = normalPersonDistribution |> Seq.filter (fun stat -> stat <= x) |> Seq.length
                    x, (float count/(float normalPersonDistribution.Length))
                ] |> dict
        fun statValue ->
            if statValue > lessThanEqualGroups.Count then 1.
            elif statValue > 0 then lessThanEqualGroups[statValue]
            else 0.

    let describeCharacter =
        let line (txt: string) = Html.div [Html.text txt]
        function
        | Detail2e (char: CharacterSheet2e) ->
            [
                let classes' =
                    let classes = char.levels |> Array.map fst |> Array.distinct
                    let getLevel class' =
                        char.levels |> Array.filter (fst >> (=) class') |> Array.max
                    let classLevels = classes |> Array.map getLevel
                    classLevels |> Array.map (fun (class', lvl) -> $"{class'} {lvl}") |> String.join "/"
                let race = char.traits.summary |> Seq.pick (function (Trait2e.RaceOf _) as race -> Some (ADND2nd.describeTrait race) | _ -> None)
                match char.origin.nationalOrigin with
                | "" ->
                    line $"{char.sex} {race} {classes'} "
                | place ->
                    line $"{char.sex} {race} {classes'} from {place}"
                line $"{char.origin.ruleSystem}, {char.origin.statRollMethod}, from level {char.origin.startingLevel}"
                line ""
                let hpTotal = char.hp |> Array.sumBy(fun (hpRoll,conBonus) -> hpRoll + conBonus)
                let hpBreakdown =
                    char.hp |> Array.map (function (hpRoll, 0) -> hpRoll.ToString() | (hpRoll, conBonus) -> $"{hpRoll}%+d{conBonus}")
                    |> List.ofArray
                    |> String.oxfordJoin
                line $"AC: {char.ac}, HP: {hpTotal} ({hpBreakdown})"
                line $"""{if char.attacks > 1 then $"{char.attacks} attacks, " else ""}THAC0 {20 - char.toHitBonus}, damage: {char.damage}"""
                line $"XP: {char.xp}"
                let displayFilter = Set.filter (function Trait2e.StatMod _ | Trait2e.RaceOf _ | Trait2e.Level _ | Trait2e.SingleClass -> false | _ -> true)
                line $"""{char.traits.summary |> displayFilter |> Seq.map ADND2nd.describeTrait |> String.join "; "}"""
                line $"{char.wealth} gp"
                ]
        | Detail5e (char: CharacterSheet5e) ->
            [
                let describe = DND5e.describeTrait
                let race = char.traits.summary |> Seq.find (fun trait1 -> DND5e.races |> List.contains trait1)
                let classes' =
                    let classes = char.levels |> Array.map fst |> Array.distinct
                    let getLevel class' =
                        char.levels |> Array.filter (fst >> (=) class') |> Array.max
                    let classLevels = classes |> Array.map getLevel
                    classLevels |> Array.map (fun (class', lvl) -> $"{class'} {lvl}") |> String.join "/"
                match char.origin.nationalOrigin with
                | "" ->
                    line $"{char.sex} {describe race} {classes'} "
                | place ->
                    line $"{char.sex} {describe race} {classes'} from {place}"
                line $"{char.origin.ruleSystem}, {char.origin.statRollMethod}, from level {char.origin.startingLevel}"
                line ""
                let hpTotal = char.hp |> Array.sumBy(fun (hpRoll,conBonus) -> hpRoll + conBonus)
                let hpBreakdown =
                    char.hp |> Array.map (function (hpRoll, 0) -> hpRoll.ToString() | (hpRoll, conBonus) -> $"{hpRoll}%+d{conBonus}")
                    |> List.ofArray
                    |> String.oxfordJoin
                line $"AC: {char.ac}, HP: {hpTotal} ({hpBreakdown})"
                let attacks = char.traits.summary |> Seq.choose (function Trait5e.ExtraAttack n -> Some (n+1) | _ -> None) |> Seq.fold max 1
                line $"""{if attacks > 1 then $"{attacks} attacks, " else ""}%+d{char.toHit} to hit, damage: {char.damage}"""
                line $"XP: {char.xp}"
                let displayFilter = Set.filter (function Trait5e.StatMod _  | Trait5e.Level _ | Trait5e.Race _ | Trait5e.StartingClass _ | Trait5e.Feat -> false | r when races |> List.contains r -> false | _ -> true)
                line $"""{char.traits.summary |> displayFilter |> Seq.map DND5e.describeTrait |> String.join "; "}"""
                line $"{char.wealth} gp"
                ]
            | DetailDF (char:CharacterSheetDF) ->
                []

    open Fable.Core.JsInterop
    open Fable.React
    open Feliz
    open UI
    let ddprop = DragDrop.prop

    // helper method to make assigning css classes more concise
    let class' f className children =
        f [
            prop.className (className: string)
            prop.children (children: _ list)
            ]
    [<ReactComponent>]
    let AutoFocusInput props =
        let self = React.useRef None
        React.useEffectOnce(fun _ ->
            if self.current.IsSome then
                self.current?focus()
                self.current?select())
        Html.input (prop.ref self::props)

    [<ReactComponent>]
    let View (model: Model) (control: ParentMsg -> unit) dispatch =
        class' Html.div "charGen" [
            class' Html.div "header" [
                match model.ruleset with
                | Ruleset.TSR ->
                    Html.text "Create a character for Advanced Dungeons and Dragons!"
                | Ruleset.WotC ->
                    Html.text "Create a character for Fifth Edition Dungeons and Dragons!"
                | Ruleset.DungeonFantasy ->
                    Html.text "Create a character for Dungeon Fantasy RPG (powered by GURPS)!"
                ]
            match model.ruleset with
                | Ruleset.TSR -> ()
                | Ruleset.WotC -> ()
                | Ruleset.DungeonFantasy ->
                    DF.View model.dfChar (FwdDungeonFantasy >> dispatch)
            ]
