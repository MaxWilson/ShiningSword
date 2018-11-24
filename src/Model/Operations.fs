module Model.Operations
open Model.Types
open Common

module Queries =
    // A query context has context about what question is being asked.
    // Think of it as a generalized input to Console.WriteLine. A query
    // context pairs with a recognizer to become a prompt.
    type IntentionQuery = Query of Id
    type StatQuery<'t> = Query of Id * string
    type FreeformQuery = Query of Id * string

module Recognizer =
    open Wilson.Packrat
    let (|Roster|_|) = ExternalContextOf<GameState> >> Option.map fst
    let inRoster pred (roster: Roster) =
        roster |> Map.tryFindKey (fun id v -> pred v)
    let nameMatches name r = r.original.name = name && r.current.hp > 0
    let (|Name|_|) = function
        | Word (name, ctx) & Roster(roster)
            when inRoster (nameMatches name) roster |> Option.isSome ->
            Some((inRoster (nameMatches name) roster |> Option.get), ctx)
        | _ -> None
    let (|Intention|_|) = function
        | Str "move" (Int (x, OWS(Str "," (Int (y, ctx))))) -> Some(Move(x,y), ctx)
        | Str "attack" (Name(id, ctx)) -> Some(Attack(id), ctx)
        | _ -> None
    let (|Number|_|) = (|Int|_|)
    let (|Bool|_|) = function
        | Word(AnyCase("y" | "yes" | "true" | "t"), ctx) -> Some(true, ctx)
        | Word(AnyCase("n" | "no" | "false" | "f"), ctx) -> Some(false, ctx)
        | _ -> None
    let (|FreeformText|_|) = function
        | Any(words, ctx) -> Some(words, ctx)

module Interact =
    open Queries
    type Interact<'result> =
        | Intention of IntentionQuery * (Intention -> 'result)
        | StatNumber of StatQuery<int> * (int -> 'result)
        | StatText of StatQuery<string> * (string -> 'result)
        | Confirmation of string * (bool -> 'result)
        | Continuation of Interact<'result>
    type Interaction<'result> =
        | Interact of Interact<'result>
        | Immediate of 'result
    let tryUnlock (g:GameState) (interact: Interact<_>) (input:string) =
        match interact, Wilson.Packrat.ParseArgs.Init(input, g) with
        | Intention(query, continuation), Recognizer.Intention(intent, Wilson.Packrat.End) ->
            Some (continuation intent)
        | StatNumber(query, continuation), Recognizer.Number(answer, Wilson.Packrat.End) ->
            Some (continuation answer)
        | StatText(query, continuation), Recognizer.FreeformText(answer, Wilson.Packrat.End) ->
            Some (continuation answer)
        | Confirmation(query, continuation), Recognizer.Bool(answer, Wilson.Packrat.End) ->
            Some (continuation answer)
        | _ -> None

module Log =
    let log msg log =
        log + "\n" + msg
    let empty = ""

module Creature =
    let map f (c: RosterEntry) =
        { c with current = f c.current }

module GameState =
    let mapLog f (g:GameState) =
        match g with
        | roster, log -> roster, f log
    let mapRoster f (g:GameState) =
        match g with
        | roster, log -> f roster, log
    let mapCreatureId id msg f (g:GameState) =
        match g with
        | roster, log ->
            roster |> Map.add id (Creature.map f roster.[id]), log |> Log.log msg

// executes action declarations in listed order
let execute (d: Declarations) (g:GameState) : GameState =
    let execute ((r,log):GameState as g) = function
        | id, Move(x, y) ->
            let msg = sprintf "%s moves to %A" r.[id].current.name (x,y)
            r |> Map.add id { r.[id] with position = x, y }, log |> Log.log msg
        | id, Attack(targetId) ->
            let actor = r.[id]
            let target = r.[targetId]
            let roll = rand 20
            if roll + 4 > 15 then
                let dmg = rand 8 + 2
                g |> GameState.mapCreatureId targetId
                    (sprintf "%s rolls %d and hits %s for %d points of damage!" actor.current.name roll target.current.name dmg)
                    (fun s -> { s with hp = s.hp - dmg })
            else
                r, log |> Log.log (sprintf "%s rolls %d and misses %s." actor.current.name roll target.current.name)
    d |> List.fold execute g
