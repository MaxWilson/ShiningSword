module Domain.Character.DND5e

open DerivedTraits

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

// XP needed before level up
let xpNeeded (char: CharacterSheet) =
    match char.levels |> Array.tryLast with
    | None -> 0<xp>
    | Some (mostRecentClass, level) ->
        let candidateLevel = (level+1)
        match xpChart.TryFind candidateLevel with
        | Some xp when char.xp < (1<xp> * xp) ->
            ((1<xp> * xp) - char.xp)
        | _ -> 0<xp>

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
