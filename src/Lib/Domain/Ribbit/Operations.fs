namespace Domain.Ribbit
open Domain.Ribbit
open Domain.Character

[<AutoOpen>]
module Operations =
    let propFail rowId propName (ribbit: RibbitData) =
        let name =
            match ribbit.scope.rows with
            | Map.Lookup rowId (Map.Lookup "PersonalName" (Text personalName)) -> personalName
            | _ -> $"Unnamed individual (ID = {rowId})"
        failwith $"{propName} should have been set on {name}"
    let request rowId propName (ribbit: RibbitData) =
        let name =
            match ribbit.scope.rows with
            | Map.Lookup rowId (Map.Lookup "PersonalName" (Text personalName)) -> personalName
            | _ -> $"Unnamed individual (ID = {rowId})"
        failwith $"{propName} should have been set on {name}"
    let prototypeP = IdProperty("prototype", 0) // for Javascript-style prototype inheritance
    let personalNameP = TextProperty("PersonalName") // self-reference may not even be necessary
    let selfP = IdProperty("UniqueId") // self-reference may not even be necessary
    let hdP = RollProperty("HitDice")
    let hpP = NumberProperty("MaxHP")
    let damageTakenP = NumberProperty("DamageTaken", 0)
    let acP = NumberProperty("AC")
    let toHitP = NumberProperty("ToHit")
    let numberOfAttacksP = NumberProperty("NumberOfAttacks", 1)
    let weaponDamageP = RollsProperty("WeaponDamage")
    let isFriendlyP = BoolProperty("IsFriendly", false)
    let currentTargetP = IdProperty("CurrentTarget", 0)

    //module Proto =
    //    let attack = Sequence [
    //        let prop paramName p = PropertyExpression(
    //        let num = LocalAddress("numberOfAttacks")
    //        Assign (num, propOf(numberOfAttacksP))
    //        ]
    //    let attack ids id : StateChange<DeltaRibbit, _> = stateChange {
    //        let! numberOfAttacks = numberOfAttacksP.Get id |> getM
    //        let! toHit = toHitP.Get id |> getM
    //        let! dmgs = weaponDamageP.Get id |> getM
    //        let! name = personalNameP.Get id |> getM
    //        let mutable msgs = []
    //        for ix in 1..numberOfAttacks do
    //            let! isAlive = getM(fun state -> hpP.Get id state > damageTakenP.Get id state)
    //            if isAlive then
    //                let! target = findTarget ids id // there seems to be a potential Fable bug with match! which can result in a dead target still getting hit (for another "fatal" blow)
    //                match target with               // so I'm using regular let! and match instead.
    //                | Some targetId ->
    //                    let! targetName = personalNameP.Get targetId |> getM
    //                    let! ac = acP.Get targetId |> getM
    //                    match rand 20 with
    //                    | 20 as n
    //                    | n when n + toHit + ac >= 20 ->
    //                        let! targetDmg = damageTakenP.Get targetId |> getM
    //                        let dmg = dmgs[ix % dmgs.Length]
    //                        let damage = dmg.roll() |> max 0
    //                        do! damageTakenP.SetM(targetId, targetDmg + damage)
    //                        let! targetHp = hpP.Get targetId |> getM
    //                        let isFatal = targetDmg + damage >= targetHp
    //                        let killMsg = if isFatal then ", a fatal blow" else ""
    //                        let! isFriendly = isFriendlyP.Get id |> getM
    //                        msgs <- msgs@[LogEntry.create($"{name} hits {targetName} for {damage} points of damage{killMsg}! [Attack roll: {n}, Damage: {dmg} = {damage}]", isFatal, if isFriendly then Good else Bad)]
    //                    | n ->
    //                        msgs <- msgs@[LogEntry.create $"{name} misses {targetName}. [Attack roll: {n}]"]
    //                | None -> ()
    //        return msgs
    //        }

    let getValue id (property: Property<'t, Ribbit>) = stateChange {
        let! value = property.GetM(id) |> withEvaluation
        return value
        }

    let nextId(): StateChange<Ribbit, Id> = stateChange {
        let! ribbit = Ribbit.DataM
        let nextId = (defaultArg ribbit.scope.biggestIdSoFar 0) + 1
        do! (Ribbit.ExecuteM (ReserveId nextId))
        return nextId
        }

    let addKind (name: Name) initialize = stateChange {
        let! nextId = nextId()
        let! state = get()
        do! initialize nextId
        do! AssociateMonsterKind(name, nextId) |> Ribbit.ExecuteM
        }

    // there are enough subtle differences with addMonster that I don't want to refactor these together. Some minor duplication is okay in this case.
    let addCharacterToRoster personalName = stateChange {
        let! monsterId = nextId()
        do! AssociateIndividual(personalName, monsterId, None) |> Ribbit.UpdateM
        do! personalNameP.SetM(monsterId, personalName)
        do! selfP.SetM(monsterId, monsterId)
        do! isFriendlyP.SetM(monsterId, true)
        return monsterId
        }

    let addMonster (kindOfMonster: Name) initialize: StateChange<_,_> = stateChange {
        let! monsterId = nextId()
        let! ribbit = Ribbit.DataM
        let kinds, categories = ribbit.kindsOfMonsters, ribbit.categories
        if not <| kinds.ContainsKey kindOfMonster then
            shouldntHappen()
        do! transform (prototypeP.Set(monsterId, kinds[kindOfMonster]))
        let rec makeUnique ix =
            // todo, make these personal names more interesting, like "fat troll" vs. "white-spotted troll"
            let candidateName = $"{kindOfMonster} #{ix}"
            match categories |> Map.tryFind kindOfMonster with
            | None -> candidateName
            | Some existing ->
                if existing |> List.exists (snd >> (=) candidateName) then
                    makeUnique (ix+1)
                else candidateName
        let personalName = makeUnique 1
        do! AssociateIndividual(personalName, monsterId, Some(kindOfMonster)) |> Ribbit.UpdateM
        do! personalNameP.SetM(monsterId, personalName)
        do! selfP.SetM(monsterId, monsterId)
        do! initialize monsterId
        return personalName
        }

    let findTarget ids id : StateChange<Ribbit, Id option> = stateChange {
        let! currentTarget = Ribbit.GetM (currentTargetP.Get id)
        let! isAlive = Ribbit.GetM (fun ribbit -> (currentTarget > 0) && (hpP.Get currentTarget ribbit > damageTakenP.Get currentTarget ribbit))
        if isAlive then
            return Some currentTarget
        else
            let! newTarget = Ribbit.GetM (fun ribbit ->
                let myTeam = isFriendlyP.Get id ribbit
                let candidates = ids |> Array.filter (fun targetId -> isFriendlyP.Get targetId ribbit <> myTeam && hpP.Get targetId ribbit > damageTakenP.Get targetId ribbit)
                if candidates.Length > 0 then
                    let choice = candidates |> chooseRandom
                    Some choice
                else None)
            match newTarget with
            | Some newTarget ->
                do! currentTargetP.SetM (id, newTarget)
                return Some newTarget
            | None -> return None
        }

