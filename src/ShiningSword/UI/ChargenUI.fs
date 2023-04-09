namespace UI.Chargen
open UI
// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.

module View =
    open Elmish
    open Domain
    open Domain.Character
    open Domain.Character.DungeonFantasy
    open Domain.Character.Universal
    open Feliz

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
        {
            ruleset = DungeonFantasy
            dfChar = DF.init None
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
