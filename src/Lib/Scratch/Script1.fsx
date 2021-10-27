#I __SOURCE_DIRECTORY__
#I ".."
#load @"Optics.fs"
#load @"Common.fs"
#load "AutoWizard.fs"
#load @"Ribbit\Model.fs"
#load "Parsing.fs"

open System
open Packrat

(* delete this before checkin

"""
define getShieldBonus:
    if self.sp at least 2
      self.sp loses 2
      return 5
    else return 0

add bob
let ac = bob.AC + getShieldBonus with self = bob
bob.AC = 18
bob.sp = 5
ac
"""

let! bob = transform1 (Game.add "Bob")
do! transform (Game.define "getShieldBonus" [
    If(BinaryOp(Dereference(IndirectDataRef("self", "sp")), Const(Number(2)), AtLeast),
        Sequence [
            Assign(IndirectDataRef("self", "sp"),
                        BinaryOp(Dereference(IndirectDataRef("self", "sp")), Const(Number(2)), Minus))
            Return (Const (Number 5))
        ],
        Some(Return (Const (Number 0))))
])
let! eventId =
    transform1 (
      Game.start
        [
            Assign(LocalRef "__event_1", StartEvent("getShieldBonus", ["self", Const (Id bob)]))
            Assign(LocalRef "ac", BinaryOp(Dereference(DataRef(bob, "AC")), Dereference(IndirectEventRef("__event_1")), Plus))
            Return (Dereference (LocalRef "ac"))
        ] [] |> compile)
let api: Ribbit.Api<_> = { dereference = Game.dereference; defer = Game.defer;
             resume = Game.resume; supply = Game.supply; start = Game.startByName }
do! transform (supply api (bob, "AC") (Number 18) >> supply api (bob, "sp") (Number 5))

*)
open Domain.Model
open Domain.Model.Ribbit

let (|EndLine|_|) =
    let chars = Set.ofList [' '; '\t']
    function
    | Chars chars (_, Char('\n', ctx)) -> Some ctx
    | _ -> None

let (|Roster|_|) = Packrat.ExternalContextOf<Map<string, AgentId list>>
let (|ValidName|_|) =
    // in this case we need to do something a little tricky: detect names by prefix
    pack <| function
    | Roster(roster) as ctx ->
        let recognized: string -> bool =
            let names = roster.Keys
            fun name -> names |> Seq.exists (fun n -> n.StartsWith(name, StringComparison.InvariantCultureIgnoreCase))
        match ctx with
        | LongestSubstringWhere recognized 30 (name, ctx) ->
            match roster |> Map.tryFind name with
            | Some [id] -> Some(id, ctx) // try for an exact match. Don't want to disregard "skeleton 1" just because "skeleton 10" exists.
            | Some ids -> None // ambiguous, try by Id instead
            | None ->
                match roster.Keys |> Seq.filter (fun rosterName -> rosterName.StartsWith(name, StringComparison.InvariantCultureIgnoreCase)) |> List.ofSeq with
                | [name] when roster.[name].Length = 1 -> Some(roster.[name].Head, ctx) // allow shortened names like El for Eladriel, as long as they are unambiguous and unique (only one match)
                | _ -> None
        | _ -> None
    | _ -> None

let (|Literal|_|) : ParseRule<Domain.Model.Ribbit.RuntimeValue> = function
    | Int(n, ctx) -> Some(RuntimeValue.Number n, ctx)
    | _ -> None

let (|EventDefinition|_|) : ParseRule<Domain.Model.Ribbit.Command> = function
    | Word(word, ctx) -> None

let (|SupplyValue|_|) : ParseRule<Domain.Model.Ribbit.Command> = function
    | ValidName(id, Str "." (Word(propName, OWSStr "=" (Literal(v, Roster(roster) & ctx))))) ->
        Some(Supply(VariableReference.DataRef(id, propName), v), ctx)
    | _ -> None

let (|StatementsExecute|_|) : ParseRule<Domain.Model.Ribbit.Command> = function
    | Word(word, ctx) -> None

let (|AddEntity|_|) : ParseRule<Domain.Model.Ribbit.Command> = function
    | Word("add", Word(name, ctx)) ->
        Some(AddToRoster(name), ctx)
    | _ -> None

let (|Command|_|) : ParseRule<Domain.Model.Ribbit.Command> = function
    | EventDefinition(cmd, ctx)
    | SupplyValue(cmd, ctx)
    | StatementsExecute(cmd, ctx) -> Some(cmd, ctx)
    | AddEntity(cmd, ctx) -> Some(cmd, ctx)
    | _ -> None

let roster = Map.ofSeq ["BOB", [1]]
parserWithExternalContext (|Command|_|) roster "bob.AC = 18"

let rec (|Program|_|) : ParseRule<Domain.Model.Ribbit.Command list> =
    pack <| function
    | Command(cmd, EndLine (Program(cmds, ctx))) -> Some(cmd::cmds, ctx)
    | Command(cmd, ctx) -> Some([cmd], ctx)

parser (|Program|_|) "add Bob"
