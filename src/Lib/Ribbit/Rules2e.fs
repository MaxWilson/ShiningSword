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
        do! transform (hdP.Set (kindId, monsterKind.hd))
        do! transform (acP.Set (kindId, monsterKind.ac))
        do! transform (toHitP.Set (kindId, monsterKind.toHit))
        do! transform (attacksP.Set (kindId, monsterKind.attacks))
        do! transform (weaponDamageP.Set (kindId, monsterKind.weaponDamage))
        }
    state {
        do! addKind monsterKind.name initialize
        }

let create (monsterKind: MonsterKind) (n: int) =
    let initialize monsterId = state {
        // every monster has individual HD
        do! transform <| fun state ->
            (hdP.Get monsterId state |> fun hdRoll -> (hpP.Set (monsterId, hdRoll.roll())) state)
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
    "Jackal", roll 1 6, roll 1 4, 7, 1, +0, roll 1 2, [], 7, [], []
    "Porcupine", roll 1 2, roll 1 4, 6, 1, +0, roll 1 3, [], 15, [], []
    "Wolf", roll 2 6, roll 3 8, 7, 1, +2, rollb 1 4 +1, [], 120, [], []
    "Kobold", roll 5 4, rollb 1 8 -1, 7, 1, +0, roll 1 6, [], 7, [J;O], [Q;Q;Q;Q;Q]
    "Goblin", roll 4 6, rollb 1 8 -1, 6, 1, +0, roll 1 6, [], 15, [K], [C]
    "Hobgoblin", roll 2 10, rollb 1 8 +1, 5, 1, +1, roll 2 4, [], 35, [J;M;D], [Q;Q;Q;Q;Q]
    "Black Bear", roll 1 3, rollb 3 8 +3, 7, 1, +3, roll 2 3 + roll 1 6, [], 175, [], []
    "Owlbear", StaticBonus 1, rollb 5 8 +2, 5, 1, +5, roll 4 6, [], 420, [], [C]
    "Hill Giant", roll 1 12, (roll 12 8 + roll 1 2), 3, 1, +11, rollb 2 6 +7, [], 3000, [D], []
    "Frost Giant", roll 1 8, (roll 15 4 + roll 1 4), 0, 1, +15, rollb 2 8 +9, [], 7000, [E], []
    ]
    |> List.map (fun args -> MonsterKind.create args |> fun monster -> monster.name, monster)
    |> Map.ofList

let createByName name n =
    create (monsterKinds[name]) n
