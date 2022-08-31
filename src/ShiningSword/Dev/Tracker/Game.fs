[<AutoOpen>]
module Dev.Tracker.Game
open Domain.Ribbit

let helpText = """
    Example commands:
    define Beholder
    add Beholder
    Beholder hp 180, xp 10000
    add Bob, Lara, Harry
    Harry hits Beholder #1 for 80
    Beholder #1 hits Lara for 30
    Beholder #1 hits Bob for 60
    clear dead
    Lara will kill Beholder #1
    Beholder #1 will Dodge
    rename Beholder #1 Stinky Beholder
    Beholder #1 hits Harry for 18, Lara for 27
    ok
    """

[<AutoOpen>]
module DataTypes =
    type Name = string
    type XP = int
    type HP = int

module Game =
    open Domain.Ribbit

    type Action = Action of string
    type WoundLog = { victims: Map<Name, int>; woundedBy: Map<Name, int> }
        with static member fresh = { victims = Map.empty; woundedBy = Map.empty }
    type Creature = { name: Name; templateType: Name option; actionDeclaration: Action option; initiativeMod: int option; xpEarned: int; woundLog: WoundLog; notes: string list }
        with static member fresh name templateType = { name = name; templateType = templateType; actionDeclaration = None; initiativeMod = None; xpEarned = 0; woundLog = WoundLog.fresh; notes = [] }
    module Properties =
        let initiativeMod (c: Creature) = c.initiativeMod
        let templateType (c: Creature) = c.templateType
        let actionDeclaration (c: Creature) = c.actionDeclaration
        let notes (c: Creature) = c.notes
        let xpEarned (c: Creature) = c.xpEarned

    type Command =
        | Define of Name
        | DeclareAction of Name * Action
        | DeclareInitiativeMod of Name * int
        | DeclareCurrentInitiative of Name * int
        | AddNotes of Name * string list
        | SetNotes of Name * string list
        | DeclareXP of Name * XP
        | DeclareHP of Name * HP
        | DeclareMaxHP of Name * HP
        | Add of Name
        | InflictDamage of src:Name * target:Name * hp:HP
        | ClearDeadCreatures
        | Remove of Name list
        | Rename of Name * newName:Name

    type d = Ribbit
    let fresh = Ribbit.Fresh

    module Getters =
        let tryGetRibbit name (prop: Property<_,Ribbit>) (game:d) =
            let data = game.data
            match data.roster |> Map.tryFind name |> Option.orElse (data.kindsOfMonsters |> Map.tryFind name) with
            | Some id ->
                match prop.GetM id (EvaluationContext.Create game) with
                | Ok v -> Some v
                | _ -> None
            | None -> None

    let update msg (model:d) =
        let inline updateByName nameStr (property:Property<_,_>) value (model:d) =
            match model.data.roster |> Map.tryFind nameStr |> Option.orElse (model.data.kindsOfMonsters |> Map.tryFind nameStr) with
            | Some id -> model |> property.Set(id, value)
            | None -> shouldntHappen()
        match msg with
        | Define name ->
            model.transform(stateChange { do! Domain.Ribbit.Operations.addKind name (fun _ rbt -> (), rbt) })
        | DeclareAction (name, Action action) ->
            model |> updateByName name Operations.actionDeclarationTextP action
        | DeclareInitiativeMod (name, initiativeMod) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with initiativeMod = Some initiativeMod }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareInit name initiativeMod }
        | DeclareCurrentInitiative (name, actualInit) ->
            { model with initRolls = model.initRolls |> Map.add name actualInit }
        | DeclareXP (name, (XP v as xp)) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with xpEarned = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareXP name xp }
        | DeclareMaxHP (name, (HP maxHP as hp)) ->
            // adjust damageTaken if necessary so that remaining HP remain constant unless they exceed maximum
            match Getters.tryGetRibbit name Operations.damageTakenP model, Getters.tryGetRibbit name Operations.hpP model with
            | Some damageTaken, Some hp ->
                let remainingHP = hp - damageTaken
                let damageTaken' = maxHP - remainingHP |> max 0
                model
                |> updateByName name damageTakenP damageTaken'
                |> updateByName name hpP maxHP
            | _ ->
                model |> updateByName name hpP maxHP
        | DeclareHP (name, hp) ->
            match Getters.tryGetRibbit name Operations.damageTakenP model with
            | Some damageTaken ->
                let hp' = hp + damageTaken
                model |> updateByName name hpP hp'
            | None ->
                model |> updateByName name hpP hp
        | Add (name) ->
            let add =
                stateChange {
                    let name = name
                    let! isMonsterKind = Ribbit.GetM(fun d -> d.data.kindsOfMonsters.ContainsKey name)
                    do! if isMonsterKind then Operations.addMonster name (fun _ r -> (), r) >> ignoreM
                        else Operations.addCharacterToRoster name >> ignoreM
                    }
            model.transform add
        | InflictDamage(src, target, hpLoss) ->
            match model.stats |> Map.tryFind src, model.stats |> Map.tryFind target with
            | Some src, Some target ->
                let hp = Getters.tryGetRibbit target.name.extract Domain.Ribbit.Operations.hpP model |> Option.defaultValue 0
                let damageTaken = Getters.tryGetRibbit target.name.extract Domain.Ribbit.Operations.damageTakenP model |> Option.defaultValue 0
                // we don't give credit for overkill damage, for XP purposes, unless it's overkill damage against a PC (who might not have even had their HP recorded yet)
                let damageCredit = hpLoss |> (if target.templateType.IsSome then min (hp - damageTaken |> max 0) else id)
                let recordInteraction (name: Name) amount = Map.change name (function None -> Some amount | Some v -> Some (v+amount))
                let target' = { target with woundLog = { target.woundLog with woundedBy = target.woundLog.woundedBy |> recordInteraction src.name hpLoss } }
                let src' = { src with woundLog = { src.woundLog with victims = src.woundLog.victims |> recordInteraction target.name hpLoss } }
                let model' = { model with
                                ribbit = model.ribbit.update (Set(PropertyAddress(model.ribbit.data.roster[target.name.extract], Operations.damageTakenP.Name), Number (damageTaken + hpLoss)))
                                stats = model.stats |> Map.add src.name src' |> Map.add target.name target' }
                let isKill = damageTaken >= hp
                if isKill = false then
                    model' // no need to update XP if no kill was achieved
                else
                    let woundLog = target'.woundLog
                    let awards =
                        [
                            let denominator = (woundLog.victims |> Map.values |> Seq.sum) + (woundLog.woundedBy |> Map.values |> Seq.sum)
                            let xpTotal =
                                match target'.templateType with
                                | Some monsterKind ->
                                    match model.bestiary[monsterKind].xp with Some (XP xp) -> xp | None -> 0
                                | None -> 0
                            for name in (woundLog.victims |> Map.keys |> Seq.append (woundLog.woundedBy |> Map.keys) |> Seq.distinct) do
                                let numerator =
                                    (woundLog.victims |> Map.tryFind name |> Option.defaultValue 0)
                                        + (woundLog.woundedBy |> Map.tryFind name |> Option.defaultValue 0)
                                let award = (numerator * xpTotal)/denominator
                                name, award
                        ]
                    let allocateXP (roster: Map<Name, Creature>) = function
                        | name, award ->
                            roster |> Map.change name (function None -> None | Some c -> Some { c with xpEarned = c.xpEarned + award })
                    { model' with stats = awards |> List.fold allocateXP model'.stats }
            | None, _ ->
                failwith $"{src} does not exist!"
            | _, None ->
                failwith $"{target} does not exist!"
        | ClearDeadCreatures ->
            let roster' =
                model.roster
                |> List.filter (fun name ->
                    let creature = model.stats[name]
                    if creature.templateType.IsNone then
                        true // never clear PCs
                    else
                        match Getters.tryGetRibbit name.extract Domain.Ribbit.Operations.hpP model with
                        | Some v -> v >= 0 // clear unambiguously-dead monsters
                        | _ -> false // if HP hasn't been set yet it can't be dead
                    )
            { model with roster = roster'; stats = model.stats |> Map.filter (fun name _ -> roster' |> List.contains name) }
        | Remove names ->
            let roster' = model.roster |> List.filter (not << flip List.contains names)
            { model with
                roster = roster'
                bestiary = model.bestiary |> Map.filter (fun name _ -> names |> List.contains name |> not)
                stats = model.stats |> Map.filter (fun name cr -> roster' |> List.contains name && (match cr.templateType with Some type1 -> names |> List.contains type1 |> not | None -> true)) }
        | Rename(name, newName) ->
            let roster' = model.roster |> List.map(function name' when name' = name -> newName | unchanged -> unchanged)
            { model with
                roster = roster'
                stats = model.stats |> Seq.map (function KeyValue(name', stats) when name' = name -> (newName, { stats with name = newName }) | KeyValue unchanged -> unchanged)
                    |> Map.ofSeq
                }
        | SetNotes(name, notes) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with notes = notes }}
            | None ->
                notImpl()
        | AddNotes(name, notes) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with notes = notes@creature.notes }}
            | None ->
                notImpl()

    type FSX =
        // FSX-oriented script commands
        static member define name = update (Define (Name name))
        static member declareHP name hp = update (DeclareHP (Name name, HP hp))
        static member declareXP name xp = update (DeclareXP (Name name, XP xp))
        static member add name = update (Add (Name name))
        static member damage src target hp = update (InflictDamage (Name src, Name target, HP hp))
        static member getXPEarned name (model:d) = model.stats[Name name].xpEarned


