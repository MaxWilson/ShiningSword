module Domain.Character

open DerivedTraits

[<Measure>] type gp
[<Measure>] type xp

type Stat = Str | Dex | Con | Int | Wis | Cha
    with static member All = [Str;Dex;Con;Int;Wis;Cha]
type Sex = Male | Female | Neither
type Name = string
type Origin = { ruleSystem: string; nationalOrigin: string; startingLevel: int; statRollMethod: string }

type RollSpec = StaticBonus of int | RollSpec of n:int * d:int * rest: RollSpec option
    with
    member this.roll() =
        let rec loop = function
            | StaticBonus n -> n
            | RollSpec(n,d,rest) ->
                let sum = List.init (abs n) (thunk1 rand d) |> List.sum
                let sum = if n < 0 then -sum else sum
                match rest with | Some rest -> sum + loop rest | None -> sum
        loop this
    override this.ToString() =
        let rec loop needsOperator = function
            | StaticBonus n -> if needsOperator && n >= 0 then $"+{n}" else n.ToString()
            | RollSpec(n,d,rest) ->
                let prefix = if needsOperator && n >= 0 then $"+{n}d{d}" else $"{n}d{d}"
                match rest with
                | None | Some (StaticBonus 0) -> prefix
                | Some rest -> $"{prefix}{loop true rest}"
        loop false this
    static member create(bonus) = StaticBonus bonus
    static member create(n,d) = RollSpec(n,d,None)
    static member create(n,d,bonus) =
        if bonus <> 0 then RollSpec(n,d,Some (StaticBonus bonus))
        else RollSpec(n,d,None)
    static member create(n,d,rest) = RollSpec(n,d,Some (rest))
    static member (+)(lhs, rhs: int) =
        let rec addBonus (bonus: int) = function
            | StaticBonus n -> StaticBonus (n + bonus)
            | RollSpec(n, d, None) -> RollSpec(n, d, Some (StaticBonus bonus))
            | RollSpec(n, d, Some rest) -> RollSpec(n, d, addBonus bonus rest |> Some)
        addBonus rhs lhs
    static member (+)(lhs, rhs: RollSpec) =
        let rec addRhs = function
            | StaticBonus n -> (rhs + n)
            | RollSpec(n, d, None) -> RollSpec(n, d, Some rhs)
            | RollSpec(n, d, Some rest) -> RollSpec(n, d, Some (addRhs rest))
        addRhs lhs
    static member (-)(lhs, rhs: int) = lhs + (-rhs)
    static member (-)(lhs, rhs: RollSpec) =
        let rec invert = function
            | StaticBonus n -> StaticBonus -n
            | RollSpec(n, d, None) -> RollSpec(-n, d, None)
            | RollSpec(n, d, Some rest) -> RollSpec(-n, d, invert rest |> Some)
        lhs + invert rhs

// turn camel casing back into words with spaces, for display to user
let uncamel (str: string) =
    let caps = ['A'..'Z'] |> Set.ofSeq
    let lower = ['a'..'z'] |> Set.ofSeq
    let mutable spaceNeededBefore = []
    let mutable inWord = true
    for i in 1..str.Length-1 do
        match str[i] with
        | ' ' -> inWord <- false
        // When multiple caps are in a row, no spaces should be used, except before the last one if it's followed by a lowercase.
        // E.g. MySSNNumber => My SSN Number, but MySSN => My SSN not My SS N
        | letter when caps.Contains letter && inWord && ((caps.Contains str[i-1] |> not) || i+1 < str.Length && lower.Contains str[i+1])->
            spaceNeededBefore <- i::spaceNeededBefore
        | letter when System.Char.IsLetterOrDigit letter -> inWord <- true
        | _ -> ()
    let rec recur workingCopy spacesNeeded =
        match spacesNeeded with
        | [] -> workingCopy
        | index::rest ->
            recur $"{workingCopy[0..(index-1)]} {workingCopy[index..]}" rest
    recur str spaceNeededBefore

module ADND2nd =
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

module DND5e =
    type CharacterClass = Artificer | Bard | Barbarian | Cleric | Druid | Fighter | Monk | Paladin | Ranger | Rogue | Sorcerer | Warlock | Wizard
        with static member All = [Artificer; Bard; Barbarian; Cleric; Druid; Fighter; Monk; Paladin; Ranger; Rogue; Sorcerer; Warlock; Wizard]
    type CharacterSubclass =
        | AlchemistArtificer | ArtilleristArtificer | BattlesmithArtificer
        | LoreBard | ValorBard
        | BerserkerBarbarian | TotemBarbarian | ZealotBarbarian | StormsBarbarian
        | KnowledgeCleric | LifeCleric | LightCleric | NatureCleric | TempestCleric | TrickeryCleric | WarCleric | ForgeCleric
        | MoonDruid | LandDruid
        | ChampionFighter | BattlemasterFighter | EldritchKnightFighter
        | OpenHandMonk | ShadowMonk | ElementalMonk | LongDeathMonk | KenseiMonk | DrunkenMasterMonk
        | DevotionPaladin | AncientsPaladin | VengeancePaladin | ConquestPaladin
        | BeastmasterRanger | HunterRanger | GloomstalkerRanger
        | ThiefRogue | AssassinRogue | ArcaneTricksterRogue | SwashbucklerRogue | MastermindRogue | ScoutRogue
        | DraconicSorcerer | WildSorcerer | ShadowSorcerer | ClockworkSorcerer | AberrantMindSorcerer
        | FiendWarlock | ArchfeyWarlock | GreatOldOneWarlock | HexbladeWarlock | FathomlessWarlock | GenieWarlock
        | AbjurorWizard | ConjurorWizard | DivinerWizard | EnchanterWizard | EvokerWizard | IllusionistWizard | NecromancerWizard | TransmuterWizard

    type Trait =
        | PC | StartingClass | Race
        | Level of class':CharacterClass * level:int
        | Subclass of CharacterSubclass
        | Human
        | StandardHuman
        | VariantHuman
        | Elf
        | WoodElf
        | HighElf
        | DrowElf
        | Dwarf
        | HillDwarf
        | MountainDwarf
        | Goblin
        | StatMod of Stat * int
        | Feat
        | GreatWeaponMaster | PolearmMaster | Sharpshooter | CrossbowExpert | Tough | Lucky | Mobile | ModeratelyArmored
        | HeavyArmorMaster
        | BonusWizardCantrip
        | Cantrip of string
        | MaskOfTheWild
        | Faster of int
        | SunlightSensitivity
        | ImprovedDarkvision
        | ShieldProficiency
        | LightArmorProficiency
        | MediumArmorProficiency
        | HeavyArmorProficiency
        | MartialWeaponProficiency
        | ``Hexblade'sCurse``
        | NimbleEscape
        | ExtraHPPerLevel of int
        | ExtraAttack of extras: int
        | PackTactics
        | MartialAdvantage2d6
    let describeTrait = function
        | StatMod(stat, n) ->
            $"%+d{n} {stat}"
        | ExtraHPPerLevel(n) ->
            $"{n} extra HP per level"
        | Cantrip cantrip ->
            $"{cantrip}"
        | Faster n ->
            $"%+d{n}' speed"
        | Level(cl, 0) ->
            $"{cl}"
        | Subclass(subclass) ->
            let full = uncamel (subclass.ToString())
            full[..(full.LastIndexOf " " - 1)]
        | stat -> uncamel (stat.ToString())
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
        originalRolls: int array
        hp: (int * int) array
        ac: int
        toHit: int
        damage: RollSpec
        xp: int<xp>
        levels: (CharacterClass * int) array
        // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
        traits: Setting<Trait, Trait Set>
        wealth: int<gp>
        }

    let races = [Human; Elf; Dwarf; Goblin]
    let feats = [GreatWeaponMaster;PolearmMaster;Sharpshooter;CrossbowExpert;Tough;Lucky;Mobile;ModeratelyArmored;HeavyArmorMaster]
    type PreconditionContext = Map<Stat, int>
    let precondition pattern (head, options: Choice<_,_>) =
        head, { options with preconditions = Some(fun (trait1, (traits, ctx: PreconditionContext)) -> pattern (trait1, (traits, ctx))) }
    let statMin (prereqs: (Stat * int) list) (_, stats: PreconditionContext) =
        prereqs |> List.every (fun (stat, minimum) -> stats[stat] >= minimum)
    let statRange (prereqs: (Stat * int * int) list) (_, stats: PreconditionContext) =
        prereqs |> List.every (fun (stat, minimum, maximum) -> minimum <= stats[stat] && stats[stat] <= maximum)
    let hasTrait trait1 (traits, _ : PreconditionContext) =
        traits |> Set.contains trait1
    let hpOf lvl = function
        | Barbarian -> if lvl = 1 then 12 else 7
        | (Fighter | Paladin | Ranger) -> if lvl = 1 then 10 else 6
        | (Artificer | Bard | Monk | Cleric | Druid | Warlock | Rogue) -> if lvl = 1 then 8 else 5
        | (Sorcerer | Wizard) -> if lvl = 1 then 6 else 4
    // generic function for stat bonuses in 5E
    let statBonus statValue = ((statValue)/2)-5
    let proficiencyBonus (traits: Trait seq) =
        traits |> Seq.choose (function Level(class', lvl) -> Some (class', lvl) | _ -> None)
            |> Seq.groupBy fst
            |> Seq.map (snd >> Seq.map snd >> Seq.max)
            |> Seq.sum
            |> fun n -> 1+(n+3)/4

    let recompute (char: CharacterSheet) =
        let traits = char.traits.summary
        let str = char.Str
        let dex = char.Dex
        let con = char.Con
        let hp =
            let classLevels = char.levels
            match char.hp with
            | hp when hp.Length < classLevels.Length ->
                let extraHPPerLevel = traits |> Seq.tryPick(function ExtraHPPerLevel n -> Some n | _ -> None) |> Option.defaultValue 0
                let hp' = classLevels |> Array.skip hp.Length |> Array.map(function (cl,lvl) -> hpOf lvl cl, statBonus con + extraHPPerLevel)
                Array.append hp hp'
            | hp -> hp
        let ac, toHit, damage =
            let statBonus = statBonus
            let toHit = statBonus (max str dex) + proficiencyBonus traits
            let has trait1 = traits |> Set.contains trait1
            if has HeavyArmorProficiency && has MartialWeaponProficiency then
                // chain mail and greatsword
                if str >= dex then 16, toHit, RollSpec.create(2, 6, statBonus str)
                // chain mail, maybe a shield, and rapier
                else (if has ShieldProficiency then 18 else 16), toHit, RollSpec.create(1, 8, statBonus dex)
            elif has HeavyArmorProficiency && has ShieldProficiency then
                18, toHit, RollSpec.create(2, 6, statBonus str)
            elif has MediumArmorProficiency then
                let acBonus = min 2 (statBonus dex)
                if dex >= str then
                    14 + acBonus + (if has ShieldProficiency then +2 else 0), toHit, RollSpec.create(1, (if has MartialWeaponProficiency then 8 else 6), statBonus dex)
                else
                    let dmg = if not (has ShieldProficiency) then RollSpec.create(2,6,statBonus str)
                                else RollSpec.create(1, (if has MartialWeaponProficiency then 8 else 6), statBonus str)
                    14 + acBonus + (if has ShieldProficiency then +2 else 0), toHit, dmg
            else
                let ac = if has LightArmorProficiency then 12 + statBonus dex else 10 + statBonus dex
                if dex >= str then
                    ac, toHit, RollSpec.create(1, (if has MartialWeaponProficiency then 8 else 6), statBonus dex)
                else
                    let dmg = if not (has ShieldProficiency) then RollSpec.create(2,6,statBonus str)
                                else RollSpec.create(1, (if has MartialWeaponProficiency then 8 else 6), statBonus str)
                    ac, toHit, dmg
        {
            char
            with
                hp = hp |> Array.ofSeq
                toHit = toHit
                ac = ac
                damage = damage
            }

    let xpChart =
        [
            1, 0
            2, 300
            3, 900
            4, 2700
            5, 6500
            6, 14000
            7, 23000
            8, 34000
            9, 48000
            10, 64000
            11, 85000
            12, 100000
            13, 120000
            14, 140000
            15, 165000
            16, 195000
            17, 225000
            18, 265000
            19, 305000
            20, 355000
            ]
        |> Map.ofList

    let levelUp (char: CharacterSheet) =
        match char.levels |> Array.tryLast with
        | None -> char
        | Some (mostRecentClass, level) ->
            let candidateLevel = (level+1)
            match xpChart.TryFind candidateLevel with
            | Some xp when char.xp >= (1<xp> * xp) ->
                recompute { char with levels = Array.append char.levels [| mostRecentClass, candidateLevel |] }
            | _ -> char

    let rules: DerivationRules<_, PreconditionContext> =
        [
            PC, { fresh [Race; StartingClass] with elideFromDisplayAndSummary = true; autopick = true }
            Race ==> races
            StartingClass ==> [for cl in CharacterClass.All -> Level(cl, 0)]
            let stats = [Str;Dex;Con;Int;Wis;Cha]
            Human ==> [StandardHuman; VariantHuman]
            confer StandardHuman [for stat in Stat.All -> StatMod(stat, 1)]
            VariantHuman, { fresh (stats |> List.map (fun x -> StatMod (x,1))) with numberAllowed = 2; mustBeDistinct = true }
            invisiblyConfer VariantHuman [Feat]
            Feat ==> feats
                |> precondition (function
                | HeavyArmorMaster, ctx -> hasTrait HeavyArmorProficiency ctx
                | HeavyArmorProficiency, ctx -> hasTrait MediumArmorProficiency ctx
                | ModeratelyArmored, ctx -> hasTrait LightArmorProficiency ctx
                | _ -> true)
            Elf ==> [HighElf; WoodElf; DrowElf]
            confer Elf [StatMod (Dex, +2)]
            confer HighElf [BonusWizardCantrip; StatMod (Int, +1)]
            confer WoodElf [MaskOfTheWild; Faster 5; StatMod (Wis, +1)]
            confer DrowElf [ImprovedDarkvision; SunlightSensitivity; StatMod (Cha, +1)]
            BonusWizardCantrip ==> [Cantrip "Fire Bolt"; Cantrip "Minor Illusion"; Cantrip "Blade Ward"; Cantrip "Toll the Dead"]
            confer Dwarf [StatMod (Con, 2); Faster -5]
            Dwarf ==> [HillDwarf; MountainDwarf]
            confer HillDwarf [ExtraHPPerLevel 1; StatMod(Wis, 1)]
            confer MountainDwarf [MediumArmorProficiency; StatMod(Str, 2)]
            confer Goblin [NimbleEscape; StatMod(Dex, 2); StatMod(Con, 1)]
            confer Tough [ExtraHPPerLevel 2]
            confer HeavyArmorMaster [StatMod (Str, 1)]
            confer ModeratelyArmored [StatMod (Dex, 1); MediumArmorProficiency; ShieldProficiency]
            confer Mobile [Faster 10]
            let subclass lst = lst |> List.map Subclass
            Level(Artificer, 3) ==> subclass [AlchemistArtificer; ArtilleristArtificer; BattlesmithArtificer]
            Level(Bard, 3) ==> subclass [LoreBard; ValorBard]
            Level(Barbarian, 3) ==> subclass [BerserkerBarbarian; TotemBarbarian; ZealotBarbarian; StormsBarbarian]
            Level(Cleric, 1) ==> subclass [KnowledgeCleric; LifeCleric; LightCleric; NatureCleric; TempestCleric; TrickeryCleric; WarCleric; ForgeCleric]
            Level(Druid, 2) ==> subclass [MoonDruid; LandDruid]
            Level(Fighter, 3) ==> subclass [ChampionFighter; BattlemasterFighter; EldritchKnightFighter]
            Level(Monk, 3) ==> subclass [OpenHandMonk; ShadowMonk; ElementalMonk; LongDeathMonk; KenseiMonk; DrunkenMasterMonk]
            Level(Paladin, 3) ==> subclass [DevotionPaladin; AncientsPaladin; VengeancePaladin; ConquestPaladin]
            Level(Ranger, 3) ==> subclass [BeastmasterRanger; HunterRanger; GloomstalkerRanger]
            Level(Rogue, 3) ==> subclass [ThiefRogue; AssassinRogue; ArcaneTricksterRogue; SwashbucklerRogue; MastermindRogue; ScoutRogue]
            Level(Sorcerer, 1) ==> subclass [DraconicSorcerer; WildSorcerer; ShadowSorcerer; ClockworkSorcerer; AberrantMindSorcerer]
            Level(Warlock, 1) ==> subclass [FiendWarlock; ArchfeyWarlock; GreatOldOneWarlock; HexbladeWarlock; FathomlessWarlock; GenieWarlock]
            Level(Wizard, 2) ==> subclass [AbjurorWizard; ConjurorWizard; DivinerWizard; EnchanterWizard; EvokerWizard; IllusionistWizard; NecromancerWizard; TransmuterWizard]
            for class' in CharacterClass.All do
                // make sure they're prompted to choose a subclass if applicable at level 1
                invisiblyConfer (Level(class', 0)) [Level(class', 1)]
            for class' in [Fighter; Paladin] do
                confer (Level(class',0)) [HeavyArmorProficiency; ShieldProficiency]
            for class' in [Cleric;Ranger;Druid;Barbarian; Artificer] do
                confer (Level(class',0)) [MediumArmorProficiency; ShieldProficiency]
            for class' in [Rogue; Warlock; Bard] do
                confer (Level(class',0)) [LightArmorProficiency]
            for subclass in [LifeCleric; NatureCleric; TempestCleric; WarCleric; ForgeCleric] do
                confer (Subclass subclass) [HeavyArmorProficiency]
            confer (Subclass HexbladeWarlock) [MediumArmorProficiency;ShieldProficiency;MartialWeaponProficiency;``Hexblade'sCurse``]
            for class' in [Fighter; Paladin; Ranger; Barbarian] do
                confer (Level(class', 5)) [ExtraAttack 1]
            confer (Level(Fighter, 11)) [ExtraAttack 2]
            confer (Level(Fighter, 20)) [ExtraAttack 3]
            ]
        |> rulesOf


module Universal =
    type Detail<'t1, 't2> = Detail2e of 't1 | Detail5e of 't2
        with
        member this.isADND = match this with Detail2e _ -> true | Detail5e _ -> false
        member this.is5E = match this with Detail2e _ -> false | Detail5e _ -> true
        member this.map2e (f: ('t1 -> 't1)) =
            match this with
            | Detail2e instance -> Detail2e (f instance)
            | unchanged -> unchanged
        member this.map5e (f: ('t2 -> 't2)) =
            match this with
            | Detail5e instance -> Detail5e (f instance)
            | unchanged -> unchanged
        member this.map (f: ('t1 -> 't1)) =
            match this with
            | Detail2e instance -> Detail2e (f instance)
            | unchanged -> unchanged
        member this.map (f: ('t2 -> 't2)) =
            match this with
            | Detail5e instance -> Detail5e (f instance)
            | unchanged -> unchanged
        member this.converge (f2e, f5e) =
            match this with
            | Detail2e data -> f2e data
            | Detail5e data -> f5e data
        member this.raw =
            match this with
            | Detail2e data -> Detail2e()
            | Detail5e data -> Detail5e()

    let (|IsADND|_|) = function
    | Detail2e x -> Some x
    | _ -> None
    let (|Is5e|_|) = function
    | Detail5e x -> Some x
    | _ -> None

    type CharacterSheet2e = ADND2nd.CharacterSheet
    type CharacterSheet5e = DND5e.CharacterSheet
    type CharacterSheet = Detail<CharacterSheet2e, CharacterSheet5e>
    type Trait2e = ADND2nd.Trait
    type Trait5e = DND5e.Trait
    type Trait = Detail<Trait2e, Trait5e>
    type DerivationInstance2e = DerivationInstance<Trait2e>
    type DerivationInstance5e = DerivationInstance<Trait5e>
    type DerivationInstance = Detail<DerivationInstance2e, DerivationInstance5e>
    type PreconditionContext2e = ADND2nd.PreconditionContext
    type PreconditionContext5e = DND5e.PreconditionContext
    let rules2e: DerivationRules<Trait2e,PreconditionContext2e> = ADND2nd.rules
    let rules5e: DerivationRules<Trait5e,PreconditionContext5e> = DND5e.rules
    let (|GenericCharacterSheet|) = function
        | Detail2e (char: ADND2nd.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; origin = char.origin; sex = char.sex; exceptionalStrength = char.exceptionalStrength; hp = char.hp; ac = char.ac |}
        | Detail5e (char: DND5e.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; origin = char.origin; sex = char.sex; exceptionalStrength = None; hp = char.hp; ac = char.ac |}
    let recompute (char: CharacterSheet) =
        (char.map2e ADND2nd.recompute).map5e DND5e.recompute
    let levelUp (char: CharacterSheet) =
        (char.map2e ADND2nd.levelUp).map5e DND5e.levelUp

let rec makeName(sex: Sex) =
    let nationOfOrigin = chooseRandom ["Tir na n'Og"; "Abysia"; "Kailasa"; "Ermor"; "Undauntra"; "Arboria"; "Mordor"]
    let rec chooseFromLists =
        function
        | potentialCategory::rest ->
            match Onomastikon.nameLists |> Map.tryFind (nationOfOrigin, potentialCategory) with
            | Some nameList -> chooseRandom nameList
            | None -> chooseFromLists rest
        | [] -> "" // sometimes e.g. there is no last name for a given national origin
    let firstName = chooseFromLists [sex.ToString()]
    match firstName with
    | "" -> makeName(sex) // invalid nation/sex combination (e.g. no females in Mordor), try again
    | _ ->
        let lastName name =
            let surname = chooseFromLists [$"Last";$"Cognomen{sex}";$"Last{sex}";]
            $"{name} {surname}".Trim()
        let prefix name =
            let prefixes = ["Insanity"; "Black"; "Merciless"; "Gentle"; "Calamity"]
            $"{chooseRandom prefixes} {name}".Trim()
        let title name =
            let suffixes = ["Defender of Humanity"; "Last of the Dwarflords"; "the Accursed"; "Esquire"; "the Undying"]
            $"{name}, {chooseRandom suffixes}".Trim()
        let allThree = (prefix >> lastName >> title)
        nationOfOrigin, chooseRandomExponentialDecay 0.4 [lastName; (lastName >> title); prefix; allThree] firstName

