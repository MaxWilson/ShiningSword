module Domain.Character

open DerivedTraits

type Stat = Str | Dex | Con | Int | Wis | Cha
    with static member All = [Str;Dex;Con;Int;Wis;Cha]
type Sex = Male | Female | Neither
type Name = string

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
    type Trait =
        | PC
        | StatMod of Stat * int
        | Race
        | CharacterClass
        | Human
        | Elf
        | SwordBowBonus of int
        | Dwarf
        | HalfElf
        | Halfling
        | HalfGiant
        | ThriKreen
        | Level of CharacterClass * level:int
        | Worship of WorshipFocus
        | PrimaryDiscipline of PsionicDiscipline

    type CharacterSheet = {
        name: Name
        nationalOrigin: string
        sex: Sex
        Str: int
        Dex: int
        Con: int
        Int: int
        Wis: int
        Cha: int
        originalRolls: int array
        xp: int
        levels: (CharacterClass * int) array
        // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
        traits: Setting<Trait, Trait Set>
        }
    let rules =
        [
            PC, { fresh [Race;CharacterClass] with elideFromDisplayAndSummary = true; autopick = true }
            Race ==> [Human;Elf;Dwarf;HalfElf;Halfling;HalfGiant;ThriKreen]
            confer Elf [SwordBowBonus 1; StatMod(Dex, +1); StatMod(Con, -1)]
            confer HalfGiant [StatMod(Str, +4); StatMod(Con, +2); StatMod(Int, -2); StatMod(Wis, -2); StatMod(Cha, -2)]
            confer Dwarf [StatMod(Con, +1); StatMod(Cha, -1)]
            confer Halfling [StatMod(Dex, +1); StatMod(Str, -1)]
            confer ThriKreen [StatMod(Dex, +2); StatMod(Wis, +1); StatMod(Int, -1); StatMod(Cha, -2)]
            CharacterClass ==> ([Fighter; Ranger; Paladin; Wizard; Cleric; Druid; Priest; Thief; Bard; Psionicist] |> List.map (fun x -> Level(x, 1)))
            Level(Priest,1) ==> (WorshipFocus.All |> List.map Worship)
            Level(Psionicist,1) ==> (PsionicDiscipline.All |> List.map PrimaryDiscipline)
            ]
        |> rulesOf
    let describe = function
        | StatMod(stat, n) ->
            $"%+d{n} {stat}"
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
        | PC
        | Level of class':CharacterClass * level:int
        | Subclass of CharacterSubclass
        | Race
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
        | GreatWeaponMaster
        | Tough
        | Lucky
        | Mobile
        | HeavyArmorMaster
        | BonusWizardCantrip
        | Cantrip of string
        | MaskOfTheWild
        | Faster of int
        | SunlightSensitivity
        | ImprovedDarkvision
        | ShieldProficiency
        | LightArmorProficiency
        | ModeratelyArmored
        | MediumArmorProficiency
        | HeavyArmorProficiency
        | NimbleEscape
        | ExtraHPPerLevel of int
    let describe = function
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
        nationalOrigin: string
        sex: Sex
        Str: int
        Dex: int
        Con: int
        Int: int
        Wis: int
        Cha: int
        originalRolls: int array
        xp: int
        levels: (CharacterClass * int) array
        // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
        traits: Setting<Trait, Trait Set>
        }

    let feats = [GreatWeaponMaster;Tough;Lucky;Mobile;HeavyArmorMaster;ModeratelyArmored]
    let rules =
        [
            PC, { fresh [Race] with elideFromDisplayAndSummary = true; autopick = true }
            Race ==> [Human; Elf; Dwarf; Goblin]
            PC ==> [for cl in CharacterClass.All -> Level(cl, 0)]
            let stats = [Str;Dex;Con;Int;Wis;Cha]
            Human ==> [StandardHuman; VariantHuman]
            confer StandardHuman [for stat in Stat.All -> StatMod(stat, 1)]
            VariantHuman, { fresh (stats |> List.map (fun x -> StatMod (x,1))) with numberAllowed = 2; mustBeDistinct = true }
            invisiblyConfer VariantHuman [Feat]
            Feat ==> feats
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
            ]
        |> rulesOf

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
        nationOfOrigin, chooseRandom [prefix; lastName; (lastName >> title); allThree] firstName

module Universal =
    type Detail<'t1, 't2> = DetailADND of 't1 | Detail5e of 't2
        with
        member this.isADND = match this with DetailADND _ -> true | Detail5e _ -> false
        member this.is5E = match this with DetailADND _ -> false | Detail5e _ -> true
        member this.map2e (f: ('t1 -> 't1)) =
            match this with
            | DetailADND instance -> DetailADND (f instance)
            | unchanged -> unchanged
        member this.map5e (f: ('t2 -> 't2)) =
            match this with
            | Detail5e instance -> Detail5e (f instance)
            | unchanged -> unchanged
        member this.map (f: ('t1 -> 't1)) =
            match this with
            | DetailADND instance -> DetailADND (f instance)
            | unchanged -> unchanged
        member this.map (f: ('t2 -> 't2)) =
            match this with
            | Detail5e instance -> Detail5e (f instance)
            | unchanged -> unchanged
    let (|IsADND|_|) = function
    | DetailADND x -> Some x
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
    let rules2e: DerivationRules<Trait2e> = ADND2nd.rules
    let rules5e: DerivationRules<Trait5e> = DND5e.rules
    let (|GenericCharacterSheet|) = function
        | DetailADND (char: ADND2nd.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; nationalOrigin = char.nationalOrigin; sex = char.sex|}
        | Detail5e (char: DND5e.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; nationalOrigin = char.nationalOrigin; sex = char.sex|}
