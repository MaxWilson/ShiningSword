module Model.Types

type Roll = { n: int; die: int; bonus: int }

type DamageType = Weapon | Fire | Cold

type Attack = {
    tohit: int
    damage: Roll * DamageType
    }

type Id = int
type Position = int * int

type StatBlock = {
    name: string
    hp: int
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
    type Data = string

type GameState = Roster * Log.Data
