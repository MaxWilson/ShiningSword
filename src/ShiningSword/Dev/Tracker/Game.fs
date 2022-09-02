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
        | DeclareNumber of Name * NumberProperty * int
        | DeclareTextual of Name * TextProperty * string
        | DeclareAction of Name * Action
        | AddNotes of Name * string list
        | SetNotes of Name * string list
        | DeclareRemainingHP of Name * HP
        | DeclareMaxHP of Name * HP
        | Add of Name
        | InflictDamage of src:Name * target:Name * hp:int
        | ClearDeadCreatures
        | Remove of Name list
        | Rename of Name * newName:Name

    type d = Ribbit
    let fresh = Ribbit.Fresh
    let woundlogP =
        let typeConvert (input: obj) =
            match input with
            | :? WoundLog as w -> Some w
            | _ -> None
        GenericProperty("woundLog", typeConvert)
    let xpEarnedP = NumberProperty("XPEarned", 0)
    let xpValueP = NumberProperty("XPValue")

    module Getters =
        let tryGetRibbit name (prop: Property<_,Ribbit>) (game:d) =
            let data = game.data
            match data.roster |> Map.tryFind name |> Option.orElse (data.kindsOfMonsters |> Map.tryFind name) with
            | Some id ->
                match prop.GetM id (EvaluationContext.Create game) with
                | Ok v -> Some v
                | _ -> None
            | None -> None
        // for use with properties with defaults
        let get name (prop: Property<_,Ribbit>) (game:d) =
            let data = game.data
            match data.roster |> Map.tryFind name |> Option.orElse (data.kindsOfMonsters |> Map.tryFind name) with
            | Some id ->
                match prop.GetM id (EvaluationContext.Create game) with
                | Ok v -> v
                | _ -> shouldntHappen()
            | None -> shouldntHappen()

    let update msg (model:d) =
        let inline transformByName nameStr (property:Property<_,_>) updateFunction (model:d) =
            match model.data.roster |> Map.tryFind nameStr |> Option.orElse (model.data.kindsOfMonsters |> Map.tryFind nameStr) with
            | Some id ->
                let current = model |> property.Get(id)
                model |> property.Set(id, updateFunction current)
            | None -> shouldntHappen()
        let inline updateByName nameStr (property:Property<_,_>) value (model:d) =
            match model.data.roster |> Map.tryFind nameStr |> Option.orElse (model.data.kindsOfMonsters |> Map.tryFind nameStr) with
            | Some id -> model |> property.Set(id, value)
            | None -> shouldntHappen()
        match msg with
        | Define name ->
            model.transform(stateChange { do! Domain.Ribbit.Operations.addKind name (fun _ rbt -> (), rbt) })
        | DeclareNumber(name, prop, value) ->
            model |> updateByName name prop value
        | DeclareTextual(name, prop, value) ->
            model |> updateByName name prop value
        | DeclareAction (name, Action action) ->
            model |> updateByName name Operations.actionDeclarationTextP action
        | DeclareMaxHP (name, (maxHP as hp)) ->
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
        | DeclareRemainingHP (name, hp) ->
            // adjust maxHP based on damageTaken so that remaining HP match the specified HP unless they exceed maximum
            let damageTaken = Getters.tryGetRibbit name Operations.damageTakenP model |> Option.defaultValue 0 |> max 0
            let hp' = hp + damageTaken
            model |> updateByName name hpP hp'
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
            let hp = Getters.tryGetRibbit target hpP model |> Option.defaultValue 0
            let damageTaken = Getters.tryGetRibbit target damageTakenP model |> Option.defaultValue 0
            let templateType name model: string option =
                // logically, we want the name of the prototype, but it's not a property so much as an expression.
                notImpl()
            // we don't give credit for overkill damage, for XP purposes, unless it's overkill damage against a PC (who might not have even had their HP recorded yet)
            let damageCredit = hpLoss |> (if (model |> templateType target).IsSome then min (hp - damageTaken |> max 0) else id)
            let recordInteraction (src: Name) (target: Name) amount = notImpl()
            let model' =
                model |> updateByName target damageTakenP (damageTaken + hpLoss)
                |> recordInteraction src target hpLoss
            let awardXP model =
                let woundLog = Getters.get target woundlogP model
                let awards =
                    [
                        let denominator = (woundLog.victims |> Map.values |> Seq.sum) + (woundLog.woundedBy |> Map.values |> Seq.sum)
                        let xpTotal = model |> Getters.tryGetRibbit target xpValueP |> Option.defaultValue 0
                        for name in (woundLog.victims |> Map.keys |> Seq.append (woundLog.woundedBy |> Map.keys) |> Seq.distinct) do
                            let numerator =
                                (woundLog.victims |> Map.tryFind name |> Option.defaultValue 0)
                                    + (woundLog.woundedBy |> Map.tryFind name |> Option.defaultValue 0)
                            let award = (numerator * xpTotal)/denominator
                            name, award
                    ]
                let allocateXP (model:d) = function
                    | name, award ->
                        model |> transformByName name xpEarnedP ((+) award)
                awards |> List.fold allocateXP model'
            let isKill = damageTaken >= hp
            if isKill then
                awardXP model'
            else model'
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
        static member declareHP name hp = update (DeclareRemainingHP (Name name, HP hp))
        static member declareXP name xp = update (DeclareXP (Name name, XP xp))
        static member add name = update (Add (Name name))
        static member damage src target hp = update (InflictDamage (Name src, Name target, HP hp))
        static member getXPEarned name (model:d) = model.stats[Name name].xpEarned


