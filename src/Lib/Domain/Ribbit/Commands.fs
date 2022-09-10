// Design intent is that Operations will have the low-level operations using stateChange, etc., and Commands will
// have abstractions that translate text into those operations by way of a parser + update function.
// In short, commands are part of the LANGUAGE Ribbit, not just the implementation the way RibbitMsgs are.
module Domain.Ribbit.Commands

let helpText = """
Example commands:
define Beholder
define Grue
add Beholder
add Grue, Grue, Grue
add Bob, Lara, Harry
"""

open Domain
open Domain.Ribbit
open Domain.Ribbit.DataStore
open Domain.Ribbit.Core
open Domain.Ribbit.Operations

type Command =
    | Define of Name
    | Add of Name
    | Declare of Name * (Id -> Ribbit -> Ribbit) // this is how we generalize Declare(name, property)--by transforming property.Set(name, <expression>) into (fun id ribbit -> myProperty.Set(id, <evaluate expression>))
    | RemoveIndividuals of Name list
    | RenameIndividual of Name * newName:Name
    | AddLogEntry of scopeIndexes:int list * txt:string // scopeIndexes = [] means append at the top level; scopeIndexes = [0] means append to the first root level entry, [-1] means the last, [0;-1] means the last child of the first root entry, etc.

let executeCommand msg (ribbit:Ribbit) =
    match msg with
    | Define name ->
        ribbit.transform(stateChange { do! Domain.Ribbit.Operations.addKind name (fun _ rbt -> (), rbt) })
    | Add (name) ->
        let add =
            stateChange {
                let name = name
                let! isMonsterKind = Ribbit.GetM(fun d -> d.data.kindsOfMonsters.ContainsKey name)
                do! if isMonsterKind then Operations.addMonster name (fun _ r -> (), r) >> ignoreM
                    else Operations.addCharacterToRoster name >> ignoreM
                }
        ribbit.transform add
    | Declare(name, setExpressionById) ->
        match ribbit.data.roster |> Map.tryFind name |> Option.orElse (ribbit.data.kindsOfMonsters |> Map.tryFind name) with
        | Some id ->
            ribbit |> setExpressionById id
        | None -> shouldntHappen()
    | RemoveIndividuals names ->
        names |> List.fold (fun model name -> (RemoveRosterEntry name |> flip Ribbit.Update model)) ribbit
    | RenameIndividual(name, newName) ->
        match ribbit.data.roster |> Map.tryFind name |> Option.orElse (ribbit.data.kindsOfMonsters |> Map.tryFind name) with
        | Some id ->
            ribbit |> personalNameP.Set(id, newName) |> Ribbit.Update (RenameRosterEntry(name, newName))
        | None -> shouldntHappen()
    | AddLogEntry(ixs, txt) ->
            ribbit.update (RibbitMsg.AddLogEntry(ixs, txt))

open Packrat

#nowarn "40" // we're not going anything crazy with recursion like calling a pass-in method as part of a ctor. Just regular pattenr-matching.

let nameChars = alphanumeric + whitespace + Set.ofList ['#']
let numericWithPlusOrMinus = Set.ofList(['0'..'9']@['+';'-'])

let (|NewName|_|) = function
    | OWS(Chars nameChars (name, ctx)) -> Some(name.Trim(), ctx)
    | _ -> None
let rec (|NewNames|_|) = pack <| function
    | NewName(name, OWSStr "," (NewNames(rest, ctx))) -> Some(name::rest, ctx)
    | NewName(name, ctx) -> Some([name], ctx)
    | _ -> None


let (|IntMod|_|) = pack <| function
    | OWS(Chars numericWithPlusOrMinus (v, OWS(rest))) ->
        match System.Int32.TryParse(v) with
        | true, v -> Some(v, rest)
        | _ -> None
    | _ -> None

let (|GameContext|_|) = ExternalContextOf<Ribbit>
let (|Name|_|) = pack <| function
    | OWS(GameContext(ribbit) & (args, ix)) ->
        let substring = args.input.Substring(ix)
        let startsWith s = substring.StartsWith(s, System.StringComparison.InvariantCultureIgnoreCase)
        let candidates = ribbit.data.roster |> Map.keys |> Seq.append (ribbit.data.kindsOfMonsters |> Map.keys) |> Seq.distinct |> Seq.sortByDescending (fun n -> n.Length)
        // allow leaving off # sign
        match candidates |> Seq.tryFind(startsWith) with
        | Some (name) ->
            let l = name.Length
            Some(name, (args, ix+l))
        | None -> None
    | _ -> None
// Individual Name = NOT a monsterkind. E.g. doesn't make sense for all dragons to hit all giants.
let (|IndividualName|_|) = pack <| function
    | OWS(GameContext(ribbit) & (args, ix)) ->
        let substring = args.input.Substring(ix)
        let startsWith s = substring.StartsWith(s, System.StringComparison.InvariantCultureIgnoreCase)
        let candidates = ribbit.data.roster |> Map.keys |> Seq.distinct |> Seq.sortByDescending (fun n -> n.Length)
        // allow leaving off # sign
        match candidates |> Seq.tryFind(startsWith) |> Option.orElseWith(fun () -> candidates |> Seq.tryFind(fun name -> startsWith (name.Replace("#", "")))) with
        | Some (name) ->
            let l = if substring.StartsWith (name.Replace("#", "")) then (name.Replace("#", "").Length) else name.Length
            Some(name, (args, ix+l))
        | None -> None
    | _ -> None
let rec (|IndividualNames|_|) = pack <| function
    | IndividualName(name, Str "," (IndividualNames(rest, ctx))) -> Some(name::rest, ctx)
    | IndividualName(name, ctx) -> Some([name], ctx)
    | _ -> None
let rec (|Names|_|) = pack <| function
    | Name(name, Str "," (Names(rest, ctx))) -> Some(name::rest, ctx)
    | Name(name, ctx) -> Some([name], ctx)
    | _ -> None
let (|Commands|_|) = pack <| function    
    | Str "add" (NewNames(names, ctx)) ->
        Some(names |> List.map Add, ctx)
    | Str "define" (NewNames(names, ctx)) ->
        Some(names |> List.map Define, ctx)
    | Str "remove" (IndividualNames(names, ctx)) ->
        Some([RemoveIndividuals names], ctx)
    | Str "rename" (IndividualName(name, NewName(newName, ctx))) ->
        Some([RenameIndividual (name, newName)], ctx)
    | _ -> None

// usage example: use via parsing active pattern
// let parse (ribbit:Ribbit) txt =
//    match ParseArgs.Init(txt, ribbit) with
//    | Commands (cmds, End) -> Some cmds
//    | _ -> None
