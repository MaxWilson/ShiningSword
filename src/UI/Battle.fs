module UI.Battle
open Model.Types
open UI.Global
open UI.Types
open UI.Types.Battle
open Fable.Helpers.React
open Fable.Helpers.React.Props

let battleSummary fullInfo (combatants:Combatant seq) =
    // fullInfo: controls whether full info is displayed in the UI for this creature e.g. exact HP totals, current orders
    let giveOrders pc _ = ()
    table [ClassName "table"] [
        thead [] [
            tr [] [
                yield th [] [str "ID"]
                yield th [] [str "Name"]
                yield th [] [str "Type"]
                yield th [] [str "Status"]
                yield th [] [str "AC"]
                yield th [] [str "HP"]
                yield th [] [str "Last Round"]
                if fullInfo then yield th [] [str "This Round"]
                ]
            ]
        tbody [] (combatants |> Seq.map (fun npc ->
                let maxHP = npc.stats.hp
                let hp =
                    match npc.usages.TryGetValue("HP") with
                    | true, v -> v
                    | _ -> maxHP
                tr [OnClick (giveOrders npc); Style[Color (sprintf "%A" npc.team)]] ([
                    yield str <| npc.id.ToString()
                    yield str npc.stats.name
                    yield str <| defaultArg npc.stats.typeName ""
                    yield str <| describeStatus hp maxHP
                    yield str (describeAC fullInfo npc.stats.ac)
                    if fullInfo then yield str <| (sprintf "%d/%d" hp maxHP) else yield str <| sprintf "About %d" (5. * (System.Math.Round (float hp / 5.)) |> int)
                    yield str "Attack!"
                    if fullInfo then yield str "Attack!"
                    ] |> List.map (fun v -> td [] [v]))
            ))
        ]

let view1 dispatch modalOperation buttonsWithHotkeys logOutput (game:GameState) (battle: Battle1.State1) =
    let winBattle _ =
        game |> Model.Gameplay.finishTower
            |> modalOperation dispatch (Finish >> BattleUpdate >> dispatch)
    let fightOneRound _ =
        let state = game |> Model.Gameplay.fight
        state |> Model.Gameplay.finishTower
            |> modalOperation dispatch (Finish >> BattleUpdate >> dispatch)
    let teams = battle.combatants |> Seq.map (function KeyValue(_,(c:Combatant)) -> c) |> Seq.groupBy (fun c -> c.team) |> Map.ofSeq
    [   div [ClassName "battleSummary"][
            for team in teams do
                yield div [ClassName "heading"] [str (match team.Key with TeamId.Blue -> "Friendlies" | TeamId.White as teamId -> "Neutrals" | _ -> "Hostiles")]
                yield battleSummary (match team.Key with Blue | White -> true | _ -> false) team.Value
            yield! buttonsWithHotkeys ["Fight", fightOneRound; "Win", winBattle]
            ]
        logOutput
        ]

let view battle =
    [div [] [str "Battle placeholder"]]
