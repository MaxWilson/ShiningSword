module Domain.Parse.Ribbit

open Packrat

type RosterAdaptor = {
    isValidNamePrefix: (string -> bool)
    tryNamePrefix: string -> Id list
    tryId: Id -> string option
    tryName: string -> Id option
}

let (|Roster|_|) =
    Packrat.ExternalContextOf<RosterAdaptor>

let (|ValidName|_|) =
    // in this case we need to do something a little tricky: detect names by prefix
    pack <| function
    | Roster(roster) as ctx ->
        match ctx with
        | LongestSubstringWhere (roster.isValidNamePrefix) 30 (name, ctx) ->
            match roster.tryNamePrefix name with
            | [id] -> Some (id, ctx) // allow shortened names like El for Eladriel, as long as they are unambiguous (only one match)
            | matches ->
                // if there are multiple matches, try for an exact match. Don't want to disregard "skeleton 1" just because "skeleton 10" exists.
                matches |> List.tryPick(fun id -> match roster.tryId id with Some v when String.equalsIgnoreCase v name -> Some(id, ctx) | _ -> None)
        | _ -> None
    | _ -> None

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
let (|EventDefinition|_|) : ParseRule<Domain.Model.Ribbit0.Command> = function
| Word(word, ctx) -> None
let (|SupplyValue|_|) : ParseRule<Domain.Model.Ribbit0.Command> = function
| Word(word, ctx) -> None
let (|StatementsExecute|_|) : ParseRule<Domain.Model.Ribbit0.Command> = function
| Word(word, ctx) -> None

let (|Command|_|) : ParseRule<Domain.Model.Ribbit0.Command> = function
    | EventDefinition(cmd, ctx)
    | SupplyValue(cmd, ctx)
    | StatementsExecute(cmd, ctx) -> Some(cmd, ctx)
    | _ -> None

let rec (|Program|_|) : ParseRule<Domain.Model.Ribbit0.Command list> =
    pack <| function
    | Command(cmd, Program(cmds, ctx)) -> Some(cmd::cmds, ctx)
    | Command(cmd, ctx) -> Some([cmd], ctx)
