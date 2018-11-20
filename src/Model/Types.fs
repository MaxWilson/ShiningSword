module Model.Types

type Roll = { n: int; die: int; bonus: int }

type DamageType = Weapon | Fire | Cold

type Attack = {
    tohit: int
    damage: Roll * DamageType
    }

type StatBlock = {
    name: string
    hp: int
    }

type Id = int

type RosterEntry = {
    original: StatBlock
    current: StatBlock
    team: int
    id: Id
    }

type Roster = Map<Id, RosterEntry>
type Position = int * int
type Intention = Move of Position | Attack of Id
type Declarations = (Id * Intention) list

module Queries =
    // A query context has context about what question is being asked.
    // Think of it as a generalized input to Console.WriteLine. A query
    // context pairs with a recognizer to become a prompt.
    type IntentionQuery = Query of Id
    type StatQuery<'t> = Query of Id * string
    type FreeformQuery = Query of Id * string

module Recognizer =
    open Wilson.Packrat
    let (|Roster|_|) = ExternalContextOf<Roster>
    let inRoster pred (roster: Roster) = 
        roster |> Map.tryFindKey (fun id v -> pred v)
    let nameMatches name r = r.original.name = name && r.current.hp > 0
    let (|Name|_|) = function
        | Word (name, ctx) & Roster(roster)
            when inRoster (nameMatches name) roster |> Option.isSome ->
            Some((inRoster (nameMatches name) roster |> Option.get), ctx)
        | _ -> None
    let (|Intention|_|) = function
        | Str "move" (Int (x, (Int (y, ctx)))) -> Some(Move(x,y), ctx)
        | Str "attack" (Name(id, ctx)) -> Some(Attack(id), ctx)
        | _ -> None
    let (|Number|_|) = (|Int|_|)
    let (|Bool|_|) = function
        | Word(AnyCase("y" | "yes" | "true" | "t"), ctx) -> Some(true, ctx)
        | Word(AnyCase("n" | "no" | "false" | "f"), ctx) -> Some(false, ctx)
        | _ -> None
    let (|FreeformText|_|) = function
        | Any(words, ctx) -> Some(words, ctx)
        
