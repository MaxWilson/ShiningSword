module Model.Types

type Roll = { n: int; die: int; bonus: int }

type DamageType = Weapon | Fire | Cold

type Attack = {
    tohit: int
    damage: Roll * DamageType
    }

type Id = int
type Name = string
type Description = string
type Position = int * int

type StatBlock = {
    name: Name
    hp: int
    xp: int
    }
    with
    member this.id = this.name

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
    pcs: StatBlock list
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
