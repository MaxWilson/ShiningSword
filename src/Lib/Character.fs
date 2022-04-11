module Domain.Character

open DerivedTraits

type Stat = Str | Dex | Con | Int | Wis | Cha
    with static member All = [Str;Dex;Con;Int;Wis;Cha]
type Sex = Male | Female | Neither
type Name = string
type Origin = { ruleSystem: string; nationalOrigin: string; startingLevel: int; statRollMethod: string }
[<Measure>] type gp
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
        xp: int
        levels: (CharacterClass * int) array
        // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
        traits: Setting<Trait, Trait Set>
        wealth: int<gp>
        }
    type PreconditionContext = {
        preracialStats: Map<Stat, int>
        postracialStats: Map<Stat, int>
        }
    let precondition pattern (head, options) =
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
        let d n = if hdMultiplier = 1 then rand n else List.init hdMultiplier (thunk1 rand n) |> List.sum
        match class' with
        | Fighter | Paladin | Ranger -> if lvl <= 9 then d 10, conBonus else 3, 0
        | Cleric | Psionicist | Priest | Druid -> if lvl <= 9 then d 8, conBonus else 2, 0
        | Thief | Bard -> if lvl <= 10 then d 6, conBonus else 2, 0
        | Wizard -> if lvl <= 10 then d 4, conBonus else 1, 0

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
        xp: int
        levels: (CharacterClass * int) array
        // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
        traits: Setting<Trait, Trait Set>
        }

    let races = [Human; Elf; Dwarf; Goblin]
    let feats = [GreatWeaponMaster;PolearmMaster;Sharpshooter;CrossbowExpert;Tough;Lucky;Mobile;ModeratelyArmored;HeavyArmorMaster]
    type PreconditionContext = Map<Stat, int>
    let precondition pattern (head, options) =
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
                | HeavyArmorMaster, ctx -> statMin [Str, 15] ctx && hasTrait HeavyArmorProficiency ctx
                | HeavyArmorProficiency, ctx -> hasTrait MediumArmorProficiency ctx
                | ModeratelyArmored, ctx -> hasTrait LightArmorProficiency ctx
                | _ -> true)
            Elf ==> [HighElf; WoodElf; DrowElf]
            confer HighElf [BonusWizardCantrip]
            confer WoodElf [MaskOfTheWild; Faster 5]
            confer DrowElf [ImprovedDarkvision; SunlightSensitivity]
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
        | Detail2e (char: ADND2nd.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; origin = char.origin; sex = char.sex|}
        | Detail5e (char: DND5e.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; origin = char.origin; sex = char.sex|}

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

