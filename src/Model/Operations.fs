module Model.Operations
open Model.Types
open Common

module Recognizer =
    open Packrat
    let (|Roster|_|) = ExternalContextOf<GameState> >> Option.bind (fun st -> st.roster)
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
    let text state txt =
        (Query.Freetext txt, state), Some
    let confirm state txt =
        (Query.Confirm txt, state), (tryParse Recognizer.``|Bool|_|``)
    let number state txt =
        (Query.Number txt, state), (tryParse Recognizer.``|Number|_|``)
    let choose state prompt choices =
        let tryChoose arg =
            choices |> Seq.tryFind (fun choice -> arg = choice.ToString())
        (Query.Select (prompt, choices |> Seq.map (fun v -> v.ToString()) |> Array.ofSeq), state), tryChoose
    let alert state txt =
        (Query.Alert txt, state), thunk (Some ())

module Creature =
    let map f (c: RosterEntry) =
        { c with current = f c.current }

module GameState =
    let mapLog f (g:GameState) =
        { g with log = f g.log }
    let mapRoster f (g:GameState) =
        { g with roster = Option.map f g.roster }
    let mapCreatureId id msg f (g:GameState) =
        match g.roster with
        | Some roster ->
            { g with
                roster = roster |> Map.add id (Creature.map f roster.[id]) |> Some;
                log = g.log |> Log.log msg }
        | None -> g
    let empty = { pcs = []; parEarned = 0; gateNumber = 1; towerNumber = 1; randomNumber = 1; timeElapsed = 0; gp = 0; log = Log.empty; roster = None }

// executes action declarations in listed order
let execute (d: Declarations) (g:GameState) : GameState =
    let execute (g:GameState) decl =
        match g.roster with
        | None -> g
        | Some r ->
            match decl with
            | id, Move(x, y) ->
                let msg = sprintf "%s moves to %A" r.[id].current.name (x,y)
                { g with roster = r |> Map.add id { r.[id] with position = x, y } |> Some}
                    |> GameState.mapLog (Log.log msg)
            | id, Attack(targetId) ->
                let actor = r.[id]
                let target = r.[targetId]
                let roll = rand 20
                if roll + 4 > 15 then
                    let dmg = rand 8 + 2
                    let msg = (sprintf "%s rolls %d and hits %s for %d points of damage!" actor.current.name roll target.current.name dmg)
                    g |> GameState.mapCreatureId targetId msg (fun (s: StatBlock) -> { s with hp = s.hp - dmg })
                      |> GameState.mapLog (Log.log msg)
                else
                    g |> GameState.mapLog (Log.log (sprintf "%s rolls %d and misses %s." actor.current.name roll target.current.name))

    d |> List.fold execute g

module PC =
    open Model.Tables
    let computeLevel xp =
        (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
    let combatBonus stat = (stat/2) - 5 // based on 5E tables
    type Class = Fighter | Wizard
    let computeHP con classList =
        let bonus = combatBonus con
        let dieSize characterClass = match characterClass with Fighter -> 10 | Wizard -> 6
        classList |> Seq.mapi (fun l cl -> if l = 0 then (dieSize cl) + bonus else (dieSize cl)/2 + 1 + bonus) |> Seq.sum
    let create name =
        let xp = 0
        { name = name; xp = xp; hp = List.init (computeLevel xp) (thunk Fighter) |> computeHP 12
          str = 12; dex = 12; con = 12; int = 12; wis = 12; cha = 12 }
