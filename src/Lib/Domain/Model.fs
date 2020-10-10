module Domain.Model
#if INTERACTIVE
module Generator =
    type LensesAttribute() =
        inherit System.Attribute()
    type DuCasesAttribute() =
        inherit System.Attribute()
#else
open Myriadic
open Myriad.Plugins
#endif
open AutoWizard

module Character =
    type Stat = Str | Dex | Con | Int | Wis | Cha

    [<Generator.Lens>]
    type Stats = {
        str: int
        dex: int
        con: int
        int: int
        wis: int
        cha: int
        }
    [<Generator.DuCases>]
    type Sex = Male | Female | Neither
    [<Generator.DuCases>]
    type Feat = Sharpshooter | CrossbowExpert | HeavyArmorMaster | GreatWeaponMaster
    [<Generator.DuCases>]
    type Skill = Athletics | Stealth | Perception | Insight
    [<Generator.DuCases>]
    type ElfRace = High | Wood | Drow
    [<Generator.DuCases>]
    type DwarfRace = Mountain | Hill
    [<Generator.DuCases>]
    type HumanType = Standard | Variant of Skill * Feat * (Stat * Stat)
    [<Generator.DuCases>]
    type Race = Human of HumanType | Elf of ElfRace | Dwarf of DwarfRace | Halforc | Goblin

    [<Generator.DuCases>]
    type Class = Barbarian | Fighter | Monk | Rogue
    [<Generator.DuCases>]
    type FightingStyle = Dueling | Archery | Defense | GreatWeaponFighting
    [<Generator.DuCases>]
    type Subclass =
        | Champion
        | EldritchKnight
        | Samurai
        | Zealot
        | Swashbuckler
        | FourElements

    type ASIChoice = ASI of Stat * Stat | Feat of Feat

    [<Generator.DuCases>]
    type ClassAbility =
        | ASIChoice of ASIChoice
        | FightingStyle of FightingStyle
        | ExtraAttack of int
        | SecondWind of int
        | Indomitable of int
        | Subclass of Subclass

    [<Generator.Lens>]
    type CharacterSheet = {
        stats: Stats
        unmodifiedStats: Stats
        name: string
        sex: Sex
        race: Race
        xp: int
        allocatedLevels: Class list // advancement priorities, e.g. [Fighter; Fighter; Fighter; Fighter; Rogue; Fighter; Rogue]
        subclasses: Map<Class, Subclass>
        classAbilities: ClassAbility list
        }


open Character

[<Generator.Lens>]
type StatBlock = {
    stats: Stats
    hp: int
    ac: int
    }


[<Generator.Lens>]
type CharSheet = {
    statBlock: StatBlock
    xp: int
    yearOfBirth: int
    sex: Sex
    }


type StatSource = StatBlock of StatBlock | CharSheet of CharSheet

[<Generator.Lens>]
type Creature = {
    name: string
    stats: StatSource
    }

module Draft =
    [<Generator.Lens>]
    type DraftSheet = {
        unmodifiedStats: Stats
        explicitName: string option
        autoName: string
        sex: Setting<Sex>
        race: Setting<Race>
        xp: int
        allocatedLevels: Class list // advancement priorities, e.g. [Fighter; Fighter; Fighter; Fighter; Rogue; Fighter; Rogue]
        subclasses: Map<Class, Subclass>
        classAbilities: Setting<ClassAbility> list
        }
        with
        member this.name = defaultArg this.explicitName this.autoName
    [<Generator.DuCases>]
    type Trait =
        | Race of Race
        | Class of Class * Subclass option * int
        | Feat of Feat
        | ASI of Stat * int
        | Skill of Skill
