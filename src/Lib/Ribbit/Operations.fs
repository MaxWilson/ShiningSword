module Domain.Ribbit.Operations
open Domain.Ribbit
open Domain.Character
open Domain.Ribbit.Ops

let propFail rowId propName (ribbit: Ribbit) =
    let name =
        match ribbit.scope.rows with
        | Map.Lookup rowId (Map.Lookup "PersonalName" (Text personalName)) -> personalName
        | _ -> $"Unnamed individual (ID = {rowId})"
    failwith $"{propName} should have been set on {name}"
let request rowId propName (ribbit: Ribbit) =
    let name =
        match ribbit.scope.rows with
        | Map.Lookup rowId (Map.Lookup "PersonalName" (Text personalName)) -> personalName
        | _ -> $"Unnamed individual (ID = {rowId})"
    failwith $"{propName} should have been set on {name}"
let prototypeP = IdProperty("prototype", 0)
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

let getValue id (property: Property<'t>) = stateChange {
    let! value = property.GetM(id) |> withEvaluation
    return value
    }

let nextId(): StateChange<DeltaRibbit, Id> = stateChange {
    let! ribbit = Delta.derefM
    let nextId = (defaultArg ribbit.scope.biggestIdSoFar 0) + 1
    do! (Delta.executeM (ReserveId nextId))
    return nextId
    }

let addKind (name: Name) initialize = stateChange {
    let! nextId = nextId()
    let! state = get()
    do! initialize nextId
    do! AssociateMonsterKind(name, nextId) |> Delta.executeM
    }

// there are enough subtle differences with addMonster that I don't want to refactor these together. Some minor duplication is okay in this case.
let addCharacterToRoster personalName = stateChange {
    let! monsterId = nextId()
    do! AssociateIndividual(personalName, monsterId, None) |> Delta.executeM
    do! personalNameP.SetM(monsterId, personalName)
    do! selfP.SetM(monsterId, monsterId)
    do! isFriendlyP.SetM(monsterId, true)
    return monsterId
    }

let addMonster (kindOfMonster: Name) initialize: StateChange<_,_> = stateChange {
    let! monsterId = nextId()
    let! ribbit = Delta.derefM
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
    do! AssociateIndividual(personalName, monsterId, Some(kindOfMonster)) |> Delta.executeM
    do! personalNameP.SetM(monsterId, personalName)
    do! selfP.SetM(monsterId, monsterId)
    do! initialize monsterId
    return personalName
    }

let findTarget ids id : StateChange<DeltaRibbit, Id option> = stateChange {
    let! currentTarget = Delta.getM (currentTargetP.Get id)
    let! isAlive = Delta.getM (fun ribbit -> (currentTarget > 0) && (hpP.Get currentTarget ribbit > damageTakenP.Get currentTarget ribbit))
    if isAlive then
        return Some currentTarget
    else
        let! newTarget = Delta.getM (fun ribbit ->
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

module Treasure =
// helper type so I can control the display order instead of being alphabetical
    type Currency = Copper | Silver | Electrum | Gold | Platinum | Gems | Jewelry
    let treasureValue treasureTypes =
        let mutable loot = []
        let incr description amt =
            loot <- loot @ [amt, description]
        let money multiplier description probability (roll: RollSpec) =
            if random.NextDouble() <= probability then (roll.roll() * multiplier) |> incr description
        let copper = money 1 Copper 1
        let silver = money 1 Silver 1
        let electrum = money 1 Electrum 1
        let gold = money 1 Gold 1
        let platinum = money 5 Platinum 1
        let copper1000 = money 1000 Copper
        let silver1000 = money 1000 Silver
        let electrum1000 = money 1000 Electrum
        let gold1000 = money 1000 Gold
        let platinum100 = money 100 Platinum
        let gems = money 1 Gems
        let jewelry = money 1 Jewelry
        let roll n d = RollSpec.create(n,d)
        let rollb n d (b:int) = RollSpec.create(n,d,b)
        for treasureType in treasureTypes do
            match treasureType with
            | A ->
                roll 1 6 |> copper1000 0.25
                roll 1 6 |> silver1000 0.3
                roll 1 6 |> electrum1000 0.35
                roll 1 10 |> gold1000 0.4
                roll 1 4 |> platinum100 0.25
                roll 4 10 |> gems 0.6
                roll 3 10 |> jewelry 0.5
            | B ->
                roll 1 8 |> copper1000 0.50
                roll 1 6 |> silver1000 0.25
                roll 1 4 |> electrum1000 0.25
                roll 1 3 |> gold1000 0.25
                roll 1 8 |> gems 0.3
                roll 1 4 |> jewelry 0.2
            | C ->
                roll 1 12 |> copper1000 0.20
                roll 1 6 |> silver1000 0.3
                roll 1 4 |> electrum1000 0.10
                roll 1 6 |> gems 0.25
                roll 1 3 |> jewelry 0.25
            | D ->
                roll 1 8 |> copper1000 0.10
                roll 1 2 |> silver1000 0.15
                roll 1 8 |> electrum1000 0.15
                roll 1 6 |> gold1000 0.5
                roll 1 10 |> gems 0.3
                roll 1 6 |> jewelry 0.25
            | E ->
                roll 1 10 |> copper1000 0.05
                roll 1 12 |> silver1000 0.25
                roll 1 6 |> electrum1000 0.25
                roll 1 8 |> gold1000 0.25
                roll 1 12 |> gems 0.15
                roll 1 2 |> jewelry 0.10
            | F ->
                roll 1 20 |> silver1000 0.1
                roll 1 12 |> electrum1000 0.15
                roll 1 10 |> gold1000 0.4
                roll 1 8 |> platinum100 0.35
                roll 2 10 |> gems 0.2
                roll 1 10 |> jewelry 0.1
            | G ->
                roll 10 4 |> gold1000 0.5
                roll 1 20 |> platinum100 0.5
                roll 5 4 |> gems 0.3
                roll 1 10 |> jewelry 0.25
            | H ->
                roll 5 4 |> copper1000 0.25
                roll 1 100 |> silver1000 0.4
                roll 10 4 |> electrum1000 0.4
                roll 10 6 |> gold1000 0.55
                roll 5 10 |> platinum100 0.25
                roll 1 100 |> gems 0.5
                roll 1 40 |> jewelry 0.5
            | I ->
                roll 3 6 |> platinum100 0.3
                roll 2 10 |> gems 0.55
                roll 1 12 |> jewelry 0.5
            | J -> roll 3 8 |> copper
            | K -> roll 3 6 |> silver
            | L -> roll 2 6 |> electrum
            | M -> roll 2 4 |> gold
            | N -> roll 1 6 |> platinum
            | O ->
                roll 1 4 |> copper1000 0.25
                roll 1 3 |> silver1000 0.20
            | P ->
                roll 1 6 |> silver1000 0.3
                roll 1 2 |> electrum1000 0.25
            | Q -> roll 1 4 |> gems 0.5
            | R ->
                roll 2 4 |> gold1000 0.4
                roll 10 6 |> platinum100 50
                roll 4 8 |> gems 0.55
                roll 1 12 |> jewelry 0.45
            | S -> roll 2 4 |> jewelry 1.
            | T -> roll 1 4 |> jewelry 1.
            | U ->
                roll 10 8 |> gems 0.9
                roll 5 6 |> jewelry 0.8
            | V -> ()
            | W ->
                roll 5 6 |> gold1000 0.6
                roll 1 8 |> platinum100 0.15
                roll 10 8 |> gems 0.6
                roll 5 8 |> jewelry 0.5
            | X -> ()
            | Y -> roll 2 6 |> gold1000 0.7
            | Z ->
                roll 1 3 |> copper1000 0.3
                roll 1 4 |> silver1000 0.25
                roll 1 4 |> electrum1000 0.25
                roll 1 4 |> gold1000 0.3
                roll 1 6 |> platinum100 0.3
                roll 10 6 |> gems 0.55
                roll 5 6 |> jewelry 0.5
        let sumsByCurrency =
            loot |> List.groupBy snd |> List.map (fun (currency, items) -> currency, items |> List.sumBy fst)
        let totalValueInCopper =
            sumsByCurrency |> List.map (function
            | Copper, v -> v
            | Silver, v -> 10 * v
            | Gold, v -> 100 * v
            | Electrum, v -> 50 * v
            | Platinum, v -> 500 * v
            | Gems, v -> 10000 * v // TODO: use actual gem and jewelry values instead of just 100 gp per gem and 500 gp per jewelry piece
            | Jewelry, v -> 50000 * v)
            |> List.sum

        if totalValueInCopper > 100 then
            let descriptions = sumsByCurrency |> List.sortBy fst |> List.map (function currency, n -> $"{n} {currency}") |> String.oxfordJoin
            (totalValueInCopper / 100 * 1<gp>), descriptions
        else
            0<gp>, "nothing of value"
