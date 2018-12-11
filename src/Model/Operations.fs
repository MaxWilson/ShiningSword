module Model.Operations
open Model.Types
open Common

module Recognizer =
    open Packrat
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

module Query =
    open Packrat

    let tryParse recognizer arg =
        match ParseArgs.Init arg |> recognizer with
        | Some(v, End) -> Some v
        | _ -> None
    let text txt =
        Query.Freetext txt, Some
    let confirm txt =
        Query.Confirm txt, (tryParse Recognizer.``|Bool|_|``)
    let number txt =
        Query.Freetext txt, (tryParse Recognizer.``|Number|_|``)
    let choose prompt choices =
        let tryChoose arg =
            choices |> Seq.tryFind (fun choice -> arg = choice.ToString())
        Query.Select (prompt, choices |> Seq.map (fun v -> v.ToString()) |> Array.ofSeq), tryChoose

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
