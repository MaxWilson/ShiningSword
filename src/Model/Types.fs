module Model.Types


module RollModule =
    open System.Numerics
    type Predicate = AtLeast of int | AtMost of int | Natural of min: int * max: int | Else
    type Transform = Div of int | Times of int
    type Aggregation = Sum | Max | Min
    type Request =
        | Dice of n:int * die:int
        | StaticValue of n:int
        | Combine of Aggregation * AggregateRequest
        | Branch of baseRollPlusMods: (Request * Request) * branches: (Predicate * Request) list
        | Transform of Request * transform: Transform
    and AggregateRequest =
        | Aggregate of Request list
        | Repeat of n: int * Request
        | Best of n:int * AggregateRequest
    type ResultValue = int
    type Result =
        { value: ResultValue; source: Request; sublog: Result list }
    and AggregateResult =
        { value: Result list; source: AggregateRequest; sublog: Result list }
    type DistributionResult =
        DistributionResult of Map<ResultValue, BigInteger>
type FractionalResult = float

type RollType = RollModule.Request

type DamageType = Weapon | Fire | Cold | Poison

type Attack = {
    tohit: int
    damage: RollType * DamageType
    }

type Id = int
type Name = string
type Description = string
type Position = int * int
type Sex = Male | Female

type CharClass = Champion | Elemonk | PurpleDragonKnight | Battlerager | Samurai
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
    | DuelingStyle
    | ArcheryStyle
    | DefenseStyle
    | Mobile

type Equipment = Item of string | LimitedUseItem of string * int

type CharTemplate = {
    name: string
    statPriorities: int*int*int*int*int*int
    description: string
    race: Race option
    advancementPriorities: CharClass list
    featurePriorities: Feature list
    homeRegion: string list option
    }

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
    template: CharTemplate option
    homeRegion: string option
    description: string
    }

type Usages = Map<string, int>

type Condition =
    | Prone
    | Grappled
    | Stunned
    | Unconscious

type StatBlock = {
    name: Name
    typeName: Name option // Name of species/class/etc. Used for UI display
    sex: Sex
    hp: int
    xp: int
    str: int
    dex: int
    con: int
    int: int
    wis: int
    cha: int
    ac: int
    resistances: Set<DamageType>
    immunities: Set<DamageType>
    damageResistance: Map<DamageType, int>
    conditionExemptions: Set<Condition>
    attacks: Attack list
    features: Feature list
    }

type Status = Status of conditions: Condition list

type TerrainFeature = | Rubble | Undergrowth | Wall | Combatant of Id

type TerrainMap = Map<Position, TerrainFeature> // todo: is map the right data structure to support the right queries?

type TeamId = Blue | White | Red | Green | Yellow | Black

// In-combat character or monster info
type Combatant = {
    id: Id
    team: TeamId
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

type Roster = Map<Id, Combatant>
type Stakes = Stakes of parEarned: int * xpReward: int * gp: int
type Battle = {
    map: TerrainMap
    combatants: Map<Id, Combatant>
    stakes: Stakes option
    }

module Battle2 =
    type Expression = Roll of RollType | Repeat
    type Command = Log of string | Quit | Roll of Expression
    type Battle = {
        log: string list
        lastCommand: (string * Command) option
        }

type Intention = Move of Position | Attack of Id
type Declarations = (Id * Intention) list

module Log =
    type Data = string list * string list list
    let empty = [], []
    let log msg (log:Data) : Data =
        match log with
        | buffer, log -> msg::buffer, log
    let logMany msgs (log:Data) =
        match log with
        | buffer, log -> msgs@buffer, log
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
    battle: Battle option
    }

type Query =
    | Freetext of string
    | Number of string
    | Confirm of string
    | Select of prompt: string * choices: string[]
    | Alert of string
    | Character of CharInfo
