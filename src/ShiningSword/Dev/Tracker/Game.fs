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
    open Domain.Random

    type WoundLog = { victims: Map<Name, int>; woundedBy: Map<Name, int> }
        with static member fresh = { victims = Map.empty; woundedBy = Map.empty }

    type Command =
        | DeclareNumber of Name * NumberProperty * int
        | DeclareTextual of Name * TextProperty * string
        | DeclareAction of Name * string
        | AddNotes of Name * string list
        | SetNotes of Name * string list
        | DeclareRemainingHP of Name * HP
        | DeclareMaxHP of Name * HP
        | InflictDamage of src:Name * target:Name * hp:int
        | ClearDeadCreatures
        | RibbitCommand of Domain.Ribbit.Commands.Command
        | Print of string
        | Eval of RollSpec // first basic automation! Only generates a log entry, no other side effects.

    type d = Ribbit
    let fresh = Ribbit.Fresh
    module Getters =
        let getId name (model: d) = let data = model.data in data.roster |> Map.tryFind name |> Option.orElse (data.kindsOfMonsters |> Map.tryFind name)
        let withId f name : Expression<_> = fun ctx ->
            match ctx.ribbit |> getId name with
            | Some id -> f id ctx
            | None -> Error (BugReport $"There is no such creature as {name}")
        let tryGetRibbit name (prop: Property<_,Ribbit>) (model:d) =
            let data = model.data
            match model |> getId name with
            | Some id ->
                match prop.GetM id (EvaluationContext.Create model) with
                | Ok v -> Some v
                | _ -> None
            | None -> None
        // for use with properties with defaults
        let get name (prop: Property<_,Ribbit>) (model:d) =
            match model |> getId name with
            | Some id ->
                match prop.GetM id (EvaluationContext.Create model) with
                | Ok v -> v
                | _ -> shouldntHappen()
            | None -> shouldntHappen()

    let woundlogP =
        let typeConvert (input: obj) =
            match input with
            | :? WoundLog as w -> Some w
            | _ -> None
        GenericProperty("woundLog", WoundLog.fresh, typeConvert)
    let notesP =
        let typeConvert (input: obj) =
            match input with
            | :? (string list) as w -> Some w
            | _ -> None
        GenericProperty("notes", [], typeConvert)
    let xpEarnedP = NumberProperty("XPEarned", 0)
    let xpValueP = NumberProperty("XPValue")
    let remainingHP = Getters.withId <| fun id ctx ->
        match hpP.GetM id ctx, damageTakenP.GetM id ctx with
        | Ok hp, Ok dmg -> Ok(hp - dmg)
        | (Error _ as e, _) | _, (Error _ as e) -> e // if there's at least one error then we have an error
    let templateTypeName = Getters.withId <| fun id ctx ->
        match monsterKindNameP.GetM id ctx with
        | Ok name -> Ok name
        | err -> err

    let isAPlayerCharacter = Getters.withId <| fun id ctx ->
        match prototypeP.GetM id ctx with
        | Ok id when id > 0 -> Ok false
        | _ -> Ok true // PCs do not currently have prototypes

    let inline setByName nameStr (property:Property<_,_>) value (model:d) =
        match model.data.roster |> Map.tryFind nameStr |> Option.orElse (model.data.kindsOfMonsters |> Map.tryFind nameStr) with
        | Some id -> model |> property.Set(id, value)
        | None -> shouldntHappen()

    let update msg (model:d) =
        let inline transformByName nameStr (property:Property<_,_>) updateFunction (model:d) =
            match model.data.roster |> Map.tryFind nameStr |> Option.orElse (model.data.kindsOfMonsters |> Map.tryFind nameStr) with
            | Some id ->
                let current = model |> property.Get(id)
                model |> property.Set(id, updateFunction current)
            | None -> shouldntHappen()
        match msg with
        | RibbitCommand(cmd) ->
            model |> Domain.Ribbit.Commands.executeCommand cmd
        | DeclareNumber(name, prop, value) ->
            model |> setByName name prop value
        | DeclareTextual(name, prop, value) ->
            model |> setByName name prop value
        | DeclareAction (name, action) ->
            model |> setByName name Operations.actionDeclarationTextP action
        | DeclareMaxHP (name, (maxHP as hp)) ->
            // adjust damageTaken if necessary so that remaining HP remain constant unless they exceed maximum
            match Getters.tryGetRibbit name damageTakenP model, Getters.tryGetRibbit name hpP model with
            | Some damageTaken, Some hp ->
                let remainingHP = hp - damageTaken
                let damageTaken' = maxHP - remainingHP |> max 0
                model
                |> setByName name damageTakenP damageTaken'
                |> setByName name hpP maxHP
            | _ ->
                model |> setByName name hpP maxHP
        | DeclareRemainingHP (name, hp) ->
            // adjust damageTaken so that remaining HP match the specified HP. Boost max hp if necessary.
            let damageTaken = Getters.tryGetRibbit name Operations.damageTakenP model |> Option.defaultValue 0 |> max 0
            match Getters.tryGetRibbit name hpP model with
            | Some maxHP ->
                if hp >= maxHP then
                    // wipe out damage and increase maxHP
                    model |> setByName name damageTakenP 0 |> setByName name hpP hp
                else
                    model |> setByName name damageTakenP (maxHP - hp)
            | None ->
                model |> setByName name damageTakenP 0 |> setByName name hpP hp
        | InflictDamage(src, target, hpLoss) ->
            let hp = Getters.tryGetRibbit target hpP model |> Option.defaultValue 0
            let damageTaken = Getters.tryGetRibbit target damageTakenP model |> Option.defaultValue 0
            let templateType name model: string option =
                // logically, we want the name of the prototype, but it's not a property so much as an expression.
                match templateTypeName name (EvaluationContext.Create model) with
                | Ok v -> Some v
                | _ -> None
            // we don't give credit for overkill damage, for XP purposes, unless it's overkill damage against a PC (who might not have even had their HP recorded yet)
            let damageCredit = hpLoss |> (if (model |> templateType target).IsSome then min (hp - damageTaken |> max 0) else id)
            let recordInteraction (src: Name) (target: Name) amount model =
                model
                |> transformByName src woundlogP (fun w -> { w with victims = w.victims |> Map.change target (function None -> Some amount | Some amount0 -> amount0 + amount |> Some) })
                |> transformByName target woundlogP (fun w -> { w with woundedBy = w.woundedBy |> Map.change src (function None -> Some amount | Some amount0 -> amount0 + amount |> Some) })
            let damageTaken' = (damageTaken + hpLoss)
            let model =
                model |> setByName target damageTakenP damageTaken'
                |> recordInteraction src target damageCredit
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
                awards |> List.fold allocateXP model
            let isKill = damageTaken' >= hp
            if isKill then
                awardXP model
            else model
        | ClearDeadCreatures ->
            let ctx = EvaluationContext.Create model
            let msgs =
                model.data.roster |> Map.keys
                |> Seq.choose (fun name ->
                    match isAPlayerCharacter name ctx with
                    | Ok true -> None
                    | _ ->
                        match remainingHP name ctx with
                        | Ok v -> if v > 0 then Some (RemoveRosterEntry name) else None // clear unambiguously-dead monsters
                        | Error _ -> None // if hp or damageTaken have not yet been set then it can't be dead
                    )
            msgs |> Seq.fold (flip Ribbit.Update) model
        | SetNotes(name, notes) ->
            model |> setByName name notesP notes
        | AddNotes(name, notes) ->
            model |> transformByName name notesP (fun notes' -> notes@notes')
        | Print txt -> model |> Domain.Ribbit.Commands.executeCommand (Domain.Ribbit.Commands.AddLogEntry([], txt))
        | Eval r -> notImpl()

    type FSX =
        // FSX-oriented script commands
        static member declareHP name hp = update (DeclareRemainingHP (name, hp))
        static member declareXP name xp = update (DeclareNumber (name, xpValueP, xp))
        static member damage src target hp = update (InflictDamage (src, target, hp))
        static member getXPEarned name (model:d) = model |> Getters.tryGetRibbit name xpEarnedP


