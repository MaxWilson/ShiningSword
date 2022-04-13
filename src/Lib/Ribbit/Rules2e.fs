module Domain.Ribbit.Rules2e
open Domain.Ribbit
open Domain.Ribbit.Operations
open Domain.Character
open Domain.Character.ADND2nd

let traitsP = FlagsProperty<Trait>("Traits", notImpl)

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
    let initialize kindId = state {
        do! transform (hdP.Set (kindId, monsterKind.hd))
        do! transform (acP.Set (kindId, monsterKind.ac))
        do! transform (toHitP.Set (kindId, monsterKind.toHit))
        do! transform (numberOfAttacksP.Set (kindId, monsterKind.attacks))
        do! transform (weaponDamageP.Set (kindId, monsterKind.weaponDamage))
        do! traitsP.SetAllM (kindId, monsterKind.traits |> List.map string |> Set.ofList)
        }
    state {
        do! addKind monsterKind.name initialize
        }

let create (monsterKind: MonsterKind) (n: int) =
    let initialize monsterId = state {
        // every monster has individual HD
        do! transform <| fun state ->
            // always have at least 1 HP
            (hdP.Get monsterId state |> fun hdRoll -> (hpP.Set (monsterId, hdRoll.roll() |> max 1)) state)
        }
    state {
        let! alreadyLoaded = getF <| fun (state: State) -> state.kindsOfMonsters.ContainsKey monsterKind.name
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
    "Jackal", roll 1 6, roll 1 4, 7, 1, +0, [roll 1 2], [], 7, [], []
    "Porcupine", roll 1 2, roll 1 4, 6, 1, +0, [roll 1 3], [], 15, [], []
    "Wolf", roll 2 6, roll 3 8, 7, 1, +2, [rollb 1 4 +1], [], 120, [], []
    "Kobold", roll 5 4, rollb 1 8 -1, 7, 1, +0, [roll 1 6], [], 7, [J], [O;Q;Q;Q;Q;Q]
    "Goblin", roll 4 6, rollb 1 8 -1, 6, 1, +0, [roll 1 6], [], 15, [K], [C]
    "Guard", roll 2 10, rollb 1 8 +1, 5, 1, +1, [roll 2 4], [], 35, [J;M;D], [Q;Q;Q;Q;Q]
    "Hobgoblin", roll 2 10, rollb 1 8 +1, 5, 1, +1, [roll 2 4], [], 35, [J;M], [D;Q;Q;Q;Q;Q]
    "Black Bear", roll 1 3, rollb 3 8 +3, 7, 2, +3, [roll 2 3; roll 1 6], [], 175, [], []
    "Owlbear", StaticBonus 1, rollb 5 8 +2, 5, 3, +5, [roll 1 6; roll 1 6; roll 2 6], [], 420, [], [C]
    "Hill Giant", roll 1 12, (roll 12 8 + roll 1 2), 3, 1, +11, [rollb 2 6 +7], [], 3000, [D], []
    "Frost Giant", roll 1 8, (roll 15 4 + roll 1 4), 0, 1, +15, [rollb 2 8 +9], [], 7000, [E], []
    ]
    |> List.map (fun args -> MonsterKind.create args |> fun monster -> monster.name, monster)
    |> Map.ofList

let createByName name n =
    create (monsterKinds[name]) n

let attack ids id = state {
    let! numberOfAttacks = numberOfAttacksP.GetM id
    let! toHit = toHitP.GetM id
    let! dmgs = weaponDamageP.GetM id
    let! name = personalNameP.GetM id
    let mutable msgs = []
    for ix in 1..numberOfAttacks do
        let! isAlive = getF(fun state -> hpP.Get id state > damageTakenP.Get id state)
        if isAlive then
            let findTarget (ribbit: State) =
                let myTeam = isFriendlyP.Get id ribbit
                ids |> Array.tryFind (fun targetId -> isFriendlyP.Get targetId ribbit <> myTeam && hpP.Get targetId ribbit > damageTakenP.Get targetId ribbit)
            let! targetId = getF findTarget
            match targetId with
            | Some targetId ->
                let! targetName = personalNameP.GetM targetId
                let! ac = acP.GetM targetId
                match rand 20 with
                | 20 as n
                | n when n + toHit + ac >= 20 ->
                    let! targetDmg = damageTakenP.GetM targetId
                    let dmg = dmgs[ix % dmgs.Length]
                    let damage = dmg.roll()
                    do! damageTakenP.SetM(targetId, targetDmg + damage)
                    msgs <- msgs@[$"{name} hits {targetName} for {damage} points of damage! [Attack roll: {n}, Damage: {dmg} = {damage}]"]
                | n ->
                    msgs <- msgs@[$"{name} misses {targetName}. [Attack roll: {n}]"]
            | None -> ()
    return msgs
    }

let fightLogic = state {
    let! ids = getF(fun state -> state.roster |> Map.values |> Array.ofSeq)
    let initiativeOrder =
        ids |> Array.map (fun id -> id, rand 10) |> Array.sortBy snd
    let mutable msgs = []
    for id, init in initiativeOrder do
        let! msgs' = attack ids id
        msgs <- msgs@msgs'

    let! factions = getF(fun state -> ids |> Array.filter(fun id -> hpP.Get id state > damageTakenP.Get id state) |> Array.groupBy (fun id -> isFriendlyP.Get id state) |> Array.map fst |> Array.sortDescending)
    let outcome = match factions with [|true|] -> Victory | [|true;false|] -> Ongoing | _ -> Defeat // mutual destruction is still defeat
    return outcome, msgs
    }

let fightOneRound (ribbit: State) : RoundResult =
    let (outcome, msg), ribbit = fightLogic ribbit
    { outcome = outcome ; msgs = msg; ribbit = ribbit }
