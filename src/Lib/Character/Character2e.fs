module Domain.Character.ADND2nd
open DerivedTraits

type WorshipFocus =
    | Oghma
    | Lugh
    | Osiris
    | Isis
    | Odin
    | Thor
    | Idun
    with static member All = [Oghma; Lugh; Osiris; Isis; Odin; Thor; Idun]
type PsionicDiscipline =
    | Clairsentience
    | Psychokinesis
    | Psychometabolism
    | Psychoportation
    | Telepathy
    | Metapsionics
    with static member All = [Clairsentience; Psychokinesis; Psychometabolism; Psychoportation; Telepathy; Metapsionics]
type CharacterClass =
    | Fighter
    | Ranger
    | Paladin
    | Wizard
    | Cleric
    | Druid
    | Priest
    | Thief
    | Bard
    | Psionicist
type Race =
    | Human
    | Elf
    | Dwarf
    | HalfElf
    | Halfling
    | HalfGiant
    | ThriKreen
type Trait =
    | PC
    | StatMod of Stat * int
    | RaceOf of Race
    | SingleClass
    | SwordBowBonus of int
    | HDMultiplier of int
    | Level of CharacterClass * level:int
    | Worship of WorshipFocus
    | PrimaryDiscipline of PsionicDiscipline
    | LimitedRegeneration of minutes: int
    | WeaponSpecialist

type CharacterSheet = {
    id: int option // for saving and loading
    name: Name
    origin: Origin
    sex: Sex
    Str: int
    Dex: int
    Con: int
    Int: int
    Wis: int
    Cha: int
    exceptionalStrength: int option
    originalRolls: int array
    hp: (int * int) array
    ac: int // todo: replace with derived computation from equipment
    attacks: int
    toHitBonus: int
    damage: RollSpec
    xp: int<xp>
    levels: (CharacterClass * int) array
    // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
    traits: Setting<Trait, Trait Set>
    wealth: int<gp>
    }
type PreconditionContext = {
    preracialStats: Map<Stat, int>
    postracialStats: Map<Stat, int>
    }
let precondition pattern (head, options: Choice<_,_>) =
    head, { options with preconditions = Some(fun (trait1, ctx: (Trait Set * PreconditionContext)) -> pattern (trait1, ctx)) }
let preracialStatMin (prereqs: (Stat * int) list) (traits: Trait Set, ctx: PreconditionContext) =
    prereqs |> List.every (fun (stat, minimum) -> ctx.preracialStats |> Map.containsKey stat && ctx.preracialStats[stat] >= minimum)
let preracialStatRange (prereqs: (Stat * int * int) list) (traits: Trait Set, ctx: PreconditionContext) =
    prereqs |> List.every (fun (stat, minimum, maximum) -> ctx.preracialStats |> Map.containsKey stat && minimum <= ctx.preracialStats[stat] && ctx.preracialStats[stat] <= maximum)
let statMin (prereqs: (Stat * int) list) (traits: Trait Set, ctx: PreconditionContext) =
    prereqs |> List.every (fun (stat, minimum) -> ctx.postracialStats |> Map.containsKey stat && ctx.postracialStats[stat] >= minimum)
let statRange (prereqs: (Stat * int * int) list) (traits: Trait Set, ctx: PreconditionContext) =
    prereqs |> List.every (fun (stat, minimum, maximum) -> ctx.postracialStats |> Map.containsKey stat && minimum <= ctx.postracialStats[stat] && ctx.postracialStats[stat] <= maximum)
let hasTrait trait1 (traits: Trait Set, ctx: PreconditionContext) =
    traits |> Set.contains trait1
let hasATrait traits (ctxTraits: Trait Set, ctx: PreconditionContext) =
    traits |> Seq.exists (fun x -> ctxTraits |> Set.contains x)
let rules : DerivationRules<_,PreconditionContext> =
    [
        PC ==> ([Human;Elf;Dwarf;HalfElf;Halfling;HalfGiant;ThriKreen] |> List.map RaceOf)
            |> precondition (function
                        | RaceOf Elf, ctx -> preracialStatMin [Dex, 6; Con, 7; Int, 8; Cha, 8] ctx
                        | RaceOf HalfElf, ctx -> preracialStatMin [Dex, 6; Con, 6; Int, 4] ctx
                        | RaceOf Dwarf, ctx -> preracialStatRange [Str, 8, 18; Dex, 3, 17; Con, 11, 18; Cha, 3, 17] ctx
                        | RaceOf Halfling, ctx -> preracialStatRange [Str, 3, 18; Dex, 12, 20; Con, 3, 20; Wis, 7, 20] ctx
                        | RaceOf ThriKreen, ctx -> preracialStatRange [Str, 8, 20; Dex, 15, 20; Cha, 3, 17] ctx
                        | RaceOf HalfGiant, ctx -> preracialStatRange [Str, 17, 20; Dex, 3, 15; Con, 15, 20; Int, 3, 15; Wis, 3, 17; Cha, 3, 17] ctx
                        | _ -> true)
        (PC, { fresh [LimitedRegeneration 60; LimitedRegeneration 50; LimitedRegeneration 40; LimitedRegeneration 30; LimitedRegeneration 20; LimitedRegeneration 10] with autopick = true; elideFromDisplayAndSummary = true })
            |> precondition (function
                    | LimitedRegeneration 60, ctx -> statRange [Con, 20, 20] ctx
                    | LimitedRegeneration 50, ctx -> statRange [Con, 21, 21] ctx
                    | LimitedRegeneration 40, ctx -> statRange [Con, 22, 22] ctx
                    | LimitedRegeneration 30, ctx -> statRange [Con, 23, 23] ctx
                    | LimitedRegeneration 20, ctx -> statRange [Con, 24, 24] ctx
                    | LimitedRegeneration 10, ctx -> statRange [Con, 25, 25] ctx
                    | _ -> false)
        PC, { fresh [SingleClass] with elideFromDisplayAndSummary = true; autopick = true }
        confer (RaceOf Elf) [SwordBowBonus 1; StatMod(Dex, +1); StatMod(Con, -1)]
        confer (RaceOf HalfGiant) [HDMultiplier 2; StatMod(Str, +4); StatMod(Con, +2); StatMod(Int, -2); StatMod(Wis, -2); StatMod(Cha, -2)]
        confer (RaceOf Dwarf) [StatMod(Con, +1); StatMod(Cha, -1)]
        confer (RaceOf Halfling) [StatMod(Dex, +1); StatMod(Str, -1)]
        confer (RaceOf ThriKreen) [StatMod(Dex, +2); StatMod(Wis, +1); StatMod(Int, -1); StatMod(Cha, -2)]
        SingleClass ==> ([Fighter; Ranger; Paladin; Wizard; Cleric; Druid; Priest; Thief; Bard; Psionicist] |> List.map (fun x -> Level(x, 1)))
            |> precondition (function
                        | Level(Fighter, _), ctx -> statMin [Str, 9] ctx
                        | Level(Paladin, _), ctx -> statMin [Str, 12; Con, 9; Wis, 13; Cha, 17] ctx && hasTrait (RaceOf Human) ctx
                        | Level(Ranger, _), ctx -> statMin [Str, 13; Dex, 13; Con, 14; Wis, 14] ctx
                        | Level(Wizard, _), ctx -> statMin [Int, 9] ctx && hasATrait [RaceOf Human; RaceOf Elf; RaceOf HalfElf] ctx
                        | Level(Cleric, _), ctx -> statMin [Wis, 9] ctx
                        | Level(Priest, _), ctx -> statMin [Wis, 9] ctx
                        | Level(Druid, _), ctx -> statMin [Wis, 12; Cha, 15] ctx
                        | Level(Thief, _), ctx -> statMin [Dex, 9] ctx
                        | Level(Bard, _), ctx -> statMin [Dex, 12; Int, 13; Cha, 15] ctx && hasTrait (RaceOf Human) ctx
                        | Level(Psionicist, _), ctx -> statMin [Con, 11; Int, 12; Wis, 15] ctx
                        | _ -> true)
        Level(Priest,1) ==> (WorshipFocus.All |> List.map Worship)
        Level(Psionicist,1) ==> (PsionicDiscipline.All |> List.map PrimaryDiscipline)
        confer (Level(Fighter, 1)) [WeaponSpecialist]
        ]
    |> rulesOf
let strBonus = function
    | n, _ when n <= 1 -> -5, -4
    | 2, _ -> -3, -2
    | 3, _ -> -3, -1
    | (4|5), _ -> -2, -1
    | (6|7), _ -> -1, 0
    | 16, _ -> 0, +1
    | 17, _ -> +1, +1
    | 18, None -> +1, +2
    | 18, Some e when e <= 50 -> +1, +3
    | 18, Some e when e <= 75 -> +2, +3
    | 18, Some e when e <= 90 -> +2, +4
    | 18, Some e when e <= 99 -> +2, +5
    | 18, Some e -> +3, +6
    | 19, _ -> +3, +7
    | 20, _ -> +3, +8
    | 21, _ -> +4, +9
    | 22, _ -> +4, +10
    | 23, _ -> +5, +11
    | 24, _ -> +6, +12
    | 25, _ -> +7, +14
    | _ -> 0, 0
let dexACBonus = function
    | n when n <= 2 -> -5
    | 3 -> -4
    | 4 -> -3
    | 5 -> -2
    | 6 -> -1
    | 15 -> +1
    | 16 -> +2
    | 17 -> +3
    | 18 | 19 | 20 -> +4
    | 21 | 22 | 23 -> +5
    | 24 | 25 -> +6
    | _ -> 0
let conBonus isWarrior = function
    | n when n <= 1 -> -3
    | 2 | 3 -> -2
    | 4 | 5 | 6 -> -1
    | 15 -> +1
    | 16 -> +2
    | n when n > 16 && not isWarrior -> +2
    | 17 -> +3
    | 18 -> +4
    | 19 | 20 -> +5
    | 21 | 22 | 23 -> +6
    | 24 | 25 -> +7
    | _ -> 0

// hdMultiplier is so half-giants can have double HP on their rolled HP
let hpOf (con, isWarrior, hdMultiplier) lvl class' =
    let conBonus = conBonus isWarrior con
    let hpRoll dieSize =
        // high Con gives you certain minimums on HP rolls that you can't go below
        if con < 20 then rand dieSize
        elif con < 21 then max 2 (rand dieSize)
        elif con < 23 then max 3 (rand dieSize)
        else max 4 (rand dieSize)
    let d n = if hdMultiplier = 1 then rand n else List.init hdMultiplier (thunk1 hpRoll n) |> List.sum
    match class' with
    | Fighter | Paladin | Ranger -> if lvl <= 9 then d 10, conBonus else 3, 0
    | Cleric | Psionicist | Priest | Druid -> if lvl <= 9 then d 8, conBonus else 2, 0
    | Thief | Bard -> if lvl <= 10 then d 6, conBonus else 2, 0
    | Wizard -> if lvl <= 10 then d 4, conBonus else 1, 0
    // you should never have zero HP at first level or you'd be dead already
    |> function (hp,bonus) when hp + bonus <= 0 && lvl = 1 -> hp, (-hp + 1) | otherwise -> otherwise

let recompute (char:CharacterSheet) =
    let traits = char.traits.summary
    let classes = char.levels
    let has = traits.Contains
    let hp =
        match char.hp with
        | hp when hp.Length < classes.Length ->
            let isWarrior = classes |> Seq.exists (function (Fighter | Ranger | Paladin), _  -> true | _ -> false)
            let hdMultiplier = (traits |> Seq.tryPick (function HDMultiplier n -> Some n | _ -> None) |> Option.defaultValue 1)
            let additionalHp = classes |> Array.choose (function (cl, lvl) when lvl > hp.Length -> Some (hpOf (char.Con, isWarrior, hdMultiplier) lvl cl) | _ -> None)
            Array.append hp additionalHp
        | hp -> hp
    let ac =
        classes |> Seq.map (function
            | (Fighter | Ranger | Paladin), _ -> 3 // plate mail
            | Cleric, _ -> 3 // chain mail + shield
            | Bard, _ -> 5 // chain mail
            | (Priest | Druid | Psionicist), _ -> 4 // hide + shield
            | Thief, _ -> 8 // leather
            | Wizard, _ -> 10 // nothing
            )
            |> Seq.fold min 10
            |> fun ac -> ac - dexACBonus char.Dex
    let attacks =
        classes |> Seq.map (function
        | (Fighter | Ranger | Paladin), lvl ->
            if lvl >= 13 then 3 elif lvl >= 7 then 2 else 1
        | _ -> 1
        )
        |> Seq.fold max 1
        |> fun n -> if has WeaponSpecialist then n + 1 else n
    let weapon =
        classes |> Seq.map (function
        | (Fighter | Ranger | Paladin | Bard), _ -> {| name = "Greatsword"; isSword = true; damage = RollSpec.create(3, 6);  |}
        | (Cleric | Priest | Druid), _ -> {| name = "Morning star"; isSword = true; damage = RollSpec.create(1, 6, 1);  |}
        | Psionicist, _ -> {| name = "Scimitar"; isSword = true; damage = RollSpec.create(1, 8);  |}
        | Thief, _ -> {| name = "Longsword"; isSword = true; damage = RollSpec.create(1, 12);  |}
        | Wizard, _ -> {| name = "Quarterstaff"; isSword = true; damage = RollSpec.create(1, 6);  |}
        )
        |> Seq.maxBy (fun weapon -> weapon.damage)
    let toHitBonus =
        classes |> Seq.map (function
            | (Fighter | Ranger | Paladin), lvl -> lvl - 1
            | (Cleric | Priest | Druid), lvl -> 2 * ((lvl+2)/3 - 1)
            | (Psionicist | Bard | Thief), lvl -> (lvl+1)/2 - 1
            | Wizard, lvl -> (lvl+2)/3 - 1
            )
            |> Seq.fold max 0
            |> fun bonus -> bonus + (strBonus (char.Str, char.exceptionalStrength) |> fst)
                                    + if has WeaponSpecialist then 1 else 0
                                    + if weapon.isSword then (traits |> Seq.tryPick(function SwordBowBonus n -> Some n | _ -> None) |> Option.defaultValue 0) else 0
    let damage =
        classes |> Seq.map (function
            | (Fighter | Ranger | Paladin | Bard), _ -> RollSpec.create(3, 6) // greatsword
            | (Cleric | Priest | Druid), _ -> RollSpec.create(1, 6, 1) // morning star
            | Psionicist, _ -> RollSpec.create(1, 8) // scimitar
            | Thief, _ -> RollSpec.create(1,12) // longsword
            | Wizard, _ -> RollSpec.create(1,6) // quarterstaff
            )
            |> Seq.fold max (RollSpec.create(0,0))
            |> fun roll -> roll + (strBonus (char.Str, char.exceptionalStrength) |> snd)
                            + if has WeaponSpecialist then 2 else 0
    {
        char
        with
            hp = hp |> Array.ofSeq
            ac = ac
            attacks = attacks
            toHitBonus = toHitBonus
            damage = damage
        }
let xpRequired = function
    | Fighter, lvl ->
        match lvl with
        | n when n <= 1 -> 0
        | 2 -> 2000
        | 3 -> 4000
        | 4 -> 8000
        | 5 -> 16000
        | 6 -> 32000
        | 7 -> 64000
        | 8 -> 125000
        | 9 -> 250000
        | n -> (n-8)*250000
    | (Ranger|Paladin), lvl ->
        match lvl with
        | n when n <= 1 -> 0
        | 2 -> 2250
        | 3 -> 4500
        | 4 -> 9000
        | 5 -> 18000
        | 6 -> 36000
        | 7 -> 75000
        | 8 -> 150000
        | n -> (n-8)*300000
    | Wizard, lvl ->
        match lvl with
        | n when n <= 1 -> 0
        | 2 -> 2500
        | 3 -> 5000
        | 4 -> 10000
        | 5 -> 20000
        | 6 -> 40000
        | 7 -> 60000
        | 8 -> 90000
        | 9 -> 135000
        | 10 -> 250000
        | n -> (n-10)*375000
    | (Cleric|Druid|Priest), lvl ->
        match lvl with
        | n when n <= 1 -> 0
        | 2 -> 1500
        | 3 -> 3000
        | 4 -> 6000
        | 5 -> 13000
        | 6 -> 27500
        | 7 -> 55000
        | 8 -> 110000
        | n -> (n-8)*225000
    | (Thief|Bard), lvl ->
        match lvl with
        | n when n <= 1 -> 0
        | 2 -> 1250
        | 3 -> 2500
        | 4 -> 5000
        | 5 -> 10000
        | 6 -> 20000
        | 7 -> 40000
        | 8 -> 70000
        | 9 -> 110000
        | 10 -> 160000
        | n -> (n-10)*220000
    | Psionicist, lvl ->
        match lvl with
        | n when n <= 1 -> 0
        | 2 -> 2200
        | 3 -> 4400
        | 4 -> 88000
        | 5 -> 16500
        | 6 -> 30000
        | 7 -> 55000
        | 8 -> 100000
        | 9 -> 200000
        | 10 -> 400000
        | 11 -> 600000
        | 12 -> 800000
        | 13 -> 1000000
        | 14 -> 1200000
        | n -> 1200000 + (n-14)*300000

let levelUp (char: CharacterSheet) =
    match char.levels |> Array.tryLast with
    | None -> char
    | Some (mostRecentClass, level) ->
        let candidateLevel = mostRecentClass, (level+1)
        if char.xp >= (xpRequired candidateLevel * 1<xp>) then
            recompute { char with levels = Array.append char.levels [| candidateLevel |] }
        else char

let describeTrait = function
    | StatMod(stat, n) ->
        $"%+d{n} {stat}"
    | (RaceOf Halfling) -> "Athasian halfling"
    | (RaceOf HalfGiant) -> "Half-giant"
    | (RaceOf race) -> $"{race}" |> uncamel
    | HDMultiplier 2 -> "Double HP rolls"
    | LimitedRegeneration minutes -> $"Limited regeneration (1 HP every {minutes} minutes)"
    | SwordBowBonus n ->
        $"%+d{n} to hit with swords and bows"
    | Worship focus ->
        $"of {focus}"
    | PrimaryDiscipline discipline ->
        match discipline with
        | Clairsentience -> "Clairsentient"
        | Psychokinesis -> "Psychokineticist"
        | Psychometabolism -> "Psychometabolist"
        | Psychoportation -> "Psychoporter"
        | Telepathy -> "Telepath"
        | Metapsionics -> "Metapsionicist"
    | Level(class', 1) -> class'.ToString()
    | stat -> uncamel (stat.ToString())

