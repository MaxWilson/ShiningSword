module Domain.Ribbit.Rules5e
open Domain.Ribbit
open Domain.Treasure
open Domain.Random
open Domain.Character
open Domain.Character.DND5e

let traitsP = FlagsProperty<Trait>("Traits")
let initBonusP = NumberProperty("InitiativeBonus", 0)
let getM = Ribbit.GetM

type MonsterKind = {
    name: string
    numberAppearing: RollSpec
    hd: RollSpec
    ac: int
    attacks: int
    toHit: int
    weaponDamage: RollSpec list
    traits: Trait list
    xp: int
    treasureType: TreasureType list
    lairTreasure: TreasureType list
    }
    with
    static member create (name, numberAppearing, hd, ac, attacks, toHit, weaponDamage, traits, xp, treasureType, lairTreasure) =
        {
        name = name; numberAppearing = numberAppearing; hd = hd; ac = ac;
        attacks = attacks; toHit = toHit; weaponDamage = weaponDamage; traits = traits
        xp = xp; treasureType = treasureType; lairTreasure = lairTreasure
        }

let load (monsterKind:MonsterKind) =
    let initialize kindId = stateChange {
        do! hdP.SetM (kindId, monsterKind.hd)
        do! acP.SetM (kindId, monsterKind.ac)
        do! toHitP.SetM (kindId, monsterKind.toHit)
        do! numberOfAttacksP.SetM (kindId, monsterKind.attacks)
        do! weaponDamageP.SetM (kindId, monsterKind.weaponDamage)
        do! traitsP.SetAllM (kindId, monsterKind.traits |> List.map string |> Set.ofList)
        }
    stateChange {
        do! addKind monsterKind.name initialize
        }

let create (monsterKind: MonsterKind) (n: int) =
    let initialize monsterId : StateChange<Ribbit, unit> = stateChange {
        // every monster has individual HD
        let! hdRoll = getF (hdP.Get monsterId)
        // always have at least 1 HP        
        do! (hpP.SetM (monsterId, hdRoll.roll() |> max 1))
        }
    stateChange {
        let! alreadyLoaded = getM(fun ribbit -> ribbit.data.kindsOfMonsters.ContainsKey monsterKind.name)
        if alreadyLoaded |> not then
            do! load monsterKind
        for ix in 1..n do
            let! personalName = addMonster monsterKind.name initialize
            ()
        }


let monsterKinds =
    [
    let roll n d = RollSpec.create(n,d)
    let rollb n d (b: int) = RollSpec.create(n,d,b)
    "Jackal", roll 1 6, roll 1 6, 12, 1, +1, [rollb 1 4 -1], [PackTactics], 10, [], []
    "Porcupine", roll 1 2, roll 1 4, 13, 1, +2, [roll 2 3], [], 25, [], []
    "Wolf", roll 2 6, rollb 2 8 +2, 13, 1, +4, [rollb 2 4 +2], [PackTactics], 50, [], []
    "Kobold", roll 5 4, rollb 2 6 -2, 12, 1, +4, [rollb 1 4 +2], [PackTactics], 25, [J], [O;Q;Q;Q;Q;Q]
    "Goblin", roll 4 6, roll 2 6, 15, 1, +4, [rollb 1 6 +2], [NimbleEscape], 50, [K], [C]
    "Guard", roll 2 10, rollb 2 8 +2, 18, 1, +3, [rollb 1 8 +1], [], 100, [J;M], [D;Q;Q;Q;Q;Q]
    "Hobgoblin", roll 2 10, rollb 2 8 +2, 18, 1, +3, [rollb 1 8 +1], [MartialAdvantage2d6], 100, [J;M;D], [Q;Q;Q;Q;Q]
    "Black Bear", roll 1 3, rollb 3 8 +6, 11, 2, +3, [rollb 1 6 +2; rollb 2 4 +2], [], 100, [], []
    "Owlbear", StaticBonus 1, rollb 7 10 +21, 13, 3, +7, [rollb 1 10 +5; rollb 2 8 +5], [], 700, [], [C]
    "Hill Giant", roll 1 12, (rollb 10 12 +40), 13, 2, +8, [rollb 3 8 +5], [], 1800, [D], []
    "Frost Giant", roll 1 8, (rollb 12 12 +60), 15, 2, +9, [rollb 3 12 +6], [], 3900, [E], []
    ]
    |> List.map (fun args -> MonsterKind.create args |> fun monster -> monster.name, monster)
    |> Map.ofList

let createByName name n : StateChange<Ribbit, unit> =
    create (monsterKinds[name]) n


let getOk f state =
    match getF f state with
    | Ok (), state -> (), state
    | _ -> shouldntHappen "Shouldn't call getOk unless underlying property is already set"
let attack ids id = stateChange {
    let! numberOfAttacks = numberOfAttacksP.Get id |> getM
    let! toHit = toHitP.Get id |> getM
    let! dmgs = weaponDamageP.Get id |> getM
    let! name = personalNameP.Get id |> getM
    let mutable msgs = []
    for ix in 1..numberOfAttacks do
        let! isAlive = getM(fun state -> hpP.Get id state > damageTakenP.Get id state)
        if isAlive then
            let! target = findTarget ids id // there seems to be a potential Fable bug with match! which can result in a dead target still getting hit (for another "fatal" blow)
            match target with               // so I'm using regular let! and match instead.
            | Some targetId ->
                let! targetName = personalNameP.Get targetId |> getM
                let! ac = acP.Get targetId |> getM
                let! packTacticsApplies = getM(fun ribbit ->
                    if traitsP.Check(id, PackTactics) ribbit then
                        let myTeam = isFriendlyP.Get id ribbit
                        ids |> Array.exists (fun otherId -> otherId <> id && isFriendlyP.Get otherId ribbit = myTeam && hpP.Get otherId ribbit > damageTakenP.Get otherId ribbit)
                    else false)
                let hasAdvantage =
                    packTacticsApplies
                printfn $"{name} has PackTactics? {packTacticsApplies}"
                let attackRoll = rand 20
                let secondRoll = rand 20
                let attackRollDescr n = if hasAdvantage then $"adv({attackRoll},{secondRoll})+{toHit} = {n+toHit}" else $"{attackRoll}+{toHit} = {n+toHit}"
                match if hasAdvantage then max attackRoll secondRoll else attackRoll with
                | 20 as n
                | n when n + toHit >= ac ->
                    let! targetDmg = damageTakenP.Get targetId |> getM
                    let! ham = traitsP.Check(targetId, HeavyArmorMaster) |> getM
                    let dmg = dmgs[ix % dmgs.Length]
                    let dmg = if ham then dmg - StaticBonus 3 else dmg
                    let damage = dmg.roll() |> max 0
                    do! damageTakenP.SetM(targetId, targetDmg + damage)
                    let! targetHp = hpP.Get targetId |> getM
                    let isFatal = targetDmg + damage >= targetHp
                    let killMsg = if isFatal then ", a fatal blow" else ""
                    let! isFriendly = isFriendlyP.Get id |> getM
                    msgs <- msgs@[LogEntry.create($"{name} hits {targetName} for {damage} points of damage! [Attack roll: {attackRollDescr n}, Damage: {dmg} = {damage}]", isFatal, if isFriendly then Good else Bad)]
                | n ->
                    msgs <- msgs@[LogEntry.create $"{name} misses {targetName}. [Attack roll: {attackRollDescr n}]"]
            | None -> ()
    return msgs
    }

let fightLogic = stateChange {
    let! initiativeOrder =
        getM(fun ribbit -> ribbit.data.roster |> Map.values |> Array.ofSeq |> Array.map (fun id -> id, rand 20 + initBonusP.Get id ribbit) |> Array.sortByDescending snd)
    let ids = initiativeOrder |> Array.map fst
    let mutable msgs = []
    for id, init in initiativeOrder do
        let! msgs' = attack ids id
        msgs <- msgs@msgs'

    let! factions = getM(fun state -> ids |> Array.filter(fun id -> hpP.Get id state > damageTakenP.Get id state) |> Array.groupBy (fun id -> isFriendlyP.Get id state) |> Array.map fst |> Array.sortDescending)
    let outcome = match factions with [|true|] -> Victory | [|true;false|] -> Ongoing | _ -> Defeat // mutual destruction is still defeat
    return outcome, msgs
    }

let fightUntilFixedPoint (ribbit: Ribbit) : RoundResult =
    let (outcome, msg), ribbit = fightLogic ribbit
    { outcome = outcome ; msgs = msg; ribbit = ribbit }
