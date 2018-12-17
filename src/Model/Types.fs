module Model.Types

type Roll = { n: int; die: int; bonus: int }

type DamageType = Weapon | Fire | Cold | Poison

type Attack = {
    tohit: int
    damage: Roll * DamageType
    }

type Id = int
type Name = string
type Description = string
type Position = int * int
type Sex = Male | Female

type CharClass = Champion | Elemonk | PurpleDragonKnight | Battlerager
type SpellList = Wizard | Cleric
type Race =
    | Human
    | Elf
    | Dwarf
    | HalfElf
    | Other

type AbilityScore =
    | Str | Dex | Con | Int | Wis | Cha

type Feature =
    | Race of Race
    | Subrace of string
    | ClassLevel of CharClass * int
    | HeavyArmorMaster
    | ASI of AbilityScore * int
    | GreatWeaponMaster
    | Sharpshooter
    | DefensiveDuelist
    | HeavyArmorDamageResistance of N:int
    | Faster of int
    | CharmResist
    | NoSleep
    | ExtraCantrip of SpellList
    | Feat
    | ExtraHP of int
    | PoisonResist
    | MediumArmorProficiency
    | Darkvision

type Equipment = Item of string | LimitedUseItem of string * int

type CharSheet = {
    originalRolls: int*int*int*int*int*int
    name: Name
    sex: Sex
    isNPC: bool
    race: Race
    classLevels: CharClass list
    str: int
    dex: int
    con: int
    int: int
    wis: int
    cha: int
    features: Feature list // all non-spell choice points derived from race/class: feats, fighting styles, etc.
    xp: int
    equipment: Equipment list
    }

type Usages = Map<string, int>

type Condition =
    | Prone
    | Grappled
    | Stunned
    | Unconscious

type StatBlock = {
    name: Name
    sex: Sex
    hp: int
    xp: int
    str: int
    dex: int
    con: int
    int: int
    wis: int
    cha: int
    resistances: Set<DamageType>
    immunities: Set<DamageType>
    damageResistance: Map<DamageType, int>
    conditionExemptions: Set<Condition>
    attacks: Attack list
    features: Feature list
    }

type Status = {
    conditions: Condition list
    }

type TerrainFeature = | Rubble | Undergrowth | Wall | Combatant of Id

type TerrainMap = Map<Position, TerrainFeature> // todo: is map the right data structure to support the right queries?

// In-combat character or monster info
type Combatant = {
    id: Id
    team: Id
    stats: StatBlock
    usages: Usages
    status: Status
    position: Position
    }

// Persistent, per-adventure, between-encounters character info. Stuff that is more temporary chan a CharSheet.
type CharInfo = {
    src: CharSheet // the CharSheet from which this CharInfo is derived
    usages: Usages
    status: Status
    hp: int
    thp: int
    sp: int
    }

type Battle = {
    map: TerrainMap
    combatants: Map<Id, Combatant>
    }

type RosterEntry = {
    original: StatBlock
    current: StatBlock
    team: int
    id: Id
    position: Position
    }

type Roster = Map<Id, RosterEntry>
type Intention = Move of Position | Attack of Id
type Declarations = (Id * Intention) list

module Log =
    type Data = string list * string list list
    let empty = [], []
    let log msg (log:Data) : Data =
        match log with
        | buffer, log -> msg::buffer, log
    let flush (log:Data) : Data =
        match log with
        | buffer, (h::rest) -> [], (h@(List.rev buffer))::rest
        | buffer, [] -> [], [List.rev buffer]
    let advance (log:Data) : Data =
        match flush log with
        | _, rest -> [], []::rest
    let extract = flush >> snd >> List.rev

type GameState = {
    pcs: CharInfo list
    parEarned: int
    gateNumber: int
    towerNumber: int
    randomNumber: int
    timeElapsed: int // seconds
    gp: int
    log: Log.Data
    roster: Roster option
    }

type Query =
    | Freetext of string
    | Number of string
    | Confirm of string
    | Select of prompt: string * choices: string[]
    | Alert of string
