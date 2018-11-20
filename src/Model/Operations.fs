module Model.Operations
open Model.Types

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
        
module Interact =
    open Queries
    type Interact<'result> =
        | Intention of IntentionQuery * (Intention -> 'result)
        | StatNumber of StatQuery<int> * (int -> 'result)
        | StatText of StatQuery<string> * (string -> 'result)
        | Confirmation of string * (bool -> 'result)
    let trampoline (interact: Interact<_>) (input:string) =
        match interact, Wilson.Packrat.ParseArgs.Init input with
        | Intention(query, continuation), Recognizer.Intention(intent, Wilson.Packrat.End) ->
            Some (fun () -> continuation intent)
        | StatNumber(query, continuation), Recognizer.Number(answer, Wilson.Packrat.End) ->
            Some (fun () -> continuation answer)
        | StatText(query, continuation), Recognizer.FreeformText(answer, Wilson.Packrat.End) ->
            Some (fun () -> continuation answer)
        | Confirmation(query, continuation), Recognizer.Bool(answer, Wilson.Packrat.End) ->
            Some (fun () -> continuation answer)
        | _ -> None

// executes action declarations in listed order
let execute (r:Roster) (d: Declarations) : Roster =
    r
