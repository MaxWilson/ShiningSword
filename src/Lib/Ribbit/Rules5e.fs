module Domain.Ribbit.Rules5e
open Domain.Ribbit
open Domain.Ribbit.Operations
open Domain.Character
open Domain.Character.DND5e

let traitsP = FlagsProperty<Trait>("Traits", propFail)
let initBonusP = NumberProperty("InitiativeBonus", 0)

type MonsterKind = {
    name: string
    numberAppearing: RollSpec
    hd: RollSpec
    ac: int
    attacks: int
    toHit: int
    weaponDamage: RollSpec
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
        do! hdP.SetM (kindId, monsterKind.hd)
        do! acP.SetM (kindId, monsterKind.ac)
        do! toHitP.SetM (kindId, monsterKind.toHit)
        do! numberOfAttacksP.SetM (kindId, monsterKind.attacks)
        do! weaponDamageP.SetM (kindId, monsterKind.weaponDamage)
        }
    state {
        do! addKind monsterKind.name initialize
        }

let create (monsterKind: MonsterKind) (n: int) =
    let initialize monsterId = state {
        // every monster has individual HD
        do! transform <| fun state ->
            (hdP.Get monsterId state |> fun hdRoll -> (hpP.Set (monsterId, hdRoll.roll())) state)
        do! traitsP.SetAllM (monsterId, monsterKind.traits |> List.map DND5e.describeTrait |> Set.ofList)
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
    "Jackal", roll 1 6, roll 1 6, 12, 1, +1, rollb 1 4 -1, [PackTactics], 10, [], []
    "Porcupine", roll 1 2, roll 1 4, 13, 1, +2, roll 2 3, [], 25, [], []
    "Wolf", roll 2 6, rollb 2 8 +2, 13, 1, +4, rollb 2 4 +2, [PackTactics], 50, [], []
    "Kobold", roll 5 4, rollb 2 6 -2, 12, 1, +4, rollb 1 4 +2, [PackTactics], 25, [J;O], [Q;Q;Q;Q;Q]
    "Goblin", roll 4 6, roll 2 6, 15, 1, +4, rollb 1 6 +2, [NimbleEscape], 50, [K], [C]
    "Hobgoblin", roll 2 10, rollb 2 8 +2, 18, 1, +3, rollb 1 8 +1, [MartialAdvantage2d6], 100, [J;M;D], [Q;Q;Q;Q;Q]
    "Black Bear", roll 1 3, rollb 3 8 +6, 11, 1, +3, rollb 1 6 +2 + rollb 2 4 +2, [], 100, [], []
    "Owlbear", StaticBonus 1, rollb 7 10 +21, 13, 1, +7, rollb 1 10 +5 + rollb 2 8 +5, [], 700, [], [C]
    "Hill Giant", roll 1 12, (rollb 10 12 +40), 13, 2, +8, rollb 3 8 +5, [], 1800, [D], []
    "Frost Giant", roll 1 8, (rollb 12 12 +60), 15, 2, +9, rollb 3 12 +6, [], 3900, [E], []
    ]
    |> List.map (fun args -> MonsterKind.create args |> fun monster -> monster.name, monster)
    |> Map.ofList

let createByName name n =
    create (monsterKinds[name]) n


let attack ids id = state {
    let! numberOfAttacks = numberOfAttacksP.GetM id
    let! toHit = toHitP.GetM id
    let! dmg = weaponDamageP.GetM id
    let! name = personalNameP.GetM id
    let mutable msgs = []
    for ix in 1..numberOfAttacks do
        let findTarget (ribbit: State) =
            let myTeam = isFriendlyP.Get id ribbit
            ids |> Array.tryFind (fun targetId -> isFriendlyP.Get targetId ribbit <> myTeam && hpP.Get targetId ribbit > damageTakenP.Get targetId ribbit)
        let! targetId = getF findTarget
        match targetId with
        | Some targetId ->
            let! targetName = personalNameP.GetM targetId
            let! ac = acP.GetM targetId
            let! packTactics = traitsP.GetM(id, PackTactics)
            let! hasLivingBuddy =
                getF(fun ribbit ->
                    let myTeam = isFriendlyP.Get id ribbit
                    ids |> Array.exists (fun otherId -> otherId <> id && isFriendlyP.Get targetId ribbit = myTeam && hpP.Get targetId ribbit > damageTakenP.Get targetId ribbit))
            let hasAdvantage =
                packTactics &&
                    hasLivingBuddy
            let attackRoll = if hasAdvantage then max (rand 20) (rand 20) else rand 20
            match attackRoll with
            | 20 as n
            | n when n + toHit >= ac ->
                let! targetDmg = damageTakenP.GetM targetId
                let! ham = traitsP.GetM(targetId, HeavyArmorMaster)
                let dmg = if ham then max 0 (dmg.roll() - 3) else dmg.roll()
                do! damageTakenP.SetM(targetId, targetDmg + dmg)
                msgs <- msgs@[$"{name} hits {targetName} for {dmg} points of damage!"]
            | _ ->
                msgs <- msgs@[$"{name} misses {targetName}."]
        | None -> ()
    return msgs
    }

let fightLogic = state {
    let! initiativeOrder =
        getF(fun ribbit -> ribbit.roster |> Map.values |> Array.ofSeq |> Array.map (fun id -> id, rand 20 + initBonusP.Get id ribbit) |> Array.sortBy snd)
    let ids = initiativeOrder |> Array.map fst
    let mutable msgs = []
    for id, init in initiativeOrder do
        let! msgs' = attack ids id
        msgs <- msgs@msgs'

    let! factions = getF(fun state -> ids |> Array.filter(fun id -> hpP.Get id state > damageTakenP.Get id state) |> Array.groupBy (fun id -> isFriendlyP.Get id state) |> Array.map fst |> Array.sortDescending)
    let outcome = match factions with [|true|] -> Victory | [|true;false|] -> Ongoing | _ -> Defeat // mutual destruction is still defeat
    return outcome, msgs
    }

let fightUntilFixedPoint (ribbit: State) : RoundResult =
    let (outcome, msg), ribbit = fightLogic ribbit
    { outcome = outcome ; msgs = msg; ribbit = ribbit }
