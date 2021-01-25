// POC for ribbit, from scratch and without generics

#I __SOURCE_DIRECTORY__
#I ".."
#load "GameLoop.fsx"

// do we need HOAS in this case? Only if we want eval to have typed output, I think, like eval<int> vs. eval<int -> int>.
// for now let's do FOAS and see how far we get.

(*

Features of POC:

1.) Lazy data entry (done)
2.) Expressions vs. Events. Expressions not logged directly,
    do not directly modify state but only request modifications
    (like event spawning or data requests), can return revised
    expression (event definition vs. event awaiting)).
    Initially just one event: Log(expr). (done)
3.) Event Spawning
4.) Parameters on spawned events
5.) Implicit spawning/event triggering
6.) Filter expressions on event triggers
7.) Variable scoping with inheritance
8.) Variable defaults (support primitives/external function calls)
9.) Delta values ("subtract 2 from inherited value")
10.) Temporal scopes ("this round", "until spell XYZ expires", etc.)
11.) Show log hierarchy (interactive explore)
12.) AI, Behaviors

Scenario:
3 orcs vs. orog
Needs rules for HP, damage, attacks, initiative

*)

type VariableName = string
type Label = Label of string
type Requirements = VariableName list
type Result<'expr, 'result> =
    | Yield of 'result
    | Require of revised: 'expr * Requirements

type State = {
    data: Map<string, int>
    pending: (Label * VariableName list * Event) list
    log: string list
    }
    with
    static member ofList vars = { data = Map.ofList vars; pending = []; log = [] }
and Expr =
    | Const of int
    | Add of Expr * Expr
    | Ref of name: VariableName
    | Prim of op: (State -> Expr)
and Event =
    Log of Expr

let rec evaluate state = function
    | Const n -> Yield n
    | Add(lhs, rhs) ->
        let (|ExprOf|) = function
            | Require (expr, req) -> expr, req
            | Yield v -> Const v, []
        match evaluate state lhs, evaluate state rhs with
        | Yield l, Yield r -> Yield (l + r)
        | ExprOf(lexpr, lreq), ExprOf(rexpr, rreq) ->
            Require(Add(lexpr, rexpr),
                match lreq, rreq with
                | [], lst | lst, [] -> lst
                | _ -> lreq@rreq |> List.distinct)
    | Ref name as expr ->
        match state.data |> Map.tryFind name with
        | Some v -> Yield v
        | None -> Require(expr, [name])
    | Prim(op) ->
        let expr = op state
        evaluate state expr

type Msg =
    | Supply of VariableName * int
    | Eval of label: string * Expr

let update msg state =
    let exec label state event =
        match event with
        | Log expr ->
            match evaluate state expr with
            | Yield v -> { state with log = state.log @ [$"{label}: {v}"] }
            | Require(expr', requirements) -> { state with pending = (Label label, requirements, Log expr')::state.pending }
    match msg with
    | Eval(label, expr) ->
        exec label state (Log expr)
    | Supply(name, v) ->
        let unblock var v ((label, vars, expr): (Label * VariableName list * Event) as pendingRow) =
            if vars |> List.exists ((=) var) then
                label, (vars |> List.filter ((<>) var)), expr // no longer waiting for var because it's here
            else
                pendingRow        
        let pending = state.pending |> List.map (unblock name v)
        // now there might be unblocked rows
        let unblocked, blocked = pending |> List.partition (fun (_, vars, _) -> List.isEmpty vars)
        unblocked |> List.fold (fun state (Label label, _, event) -> exec label state event) { state with pending = blocked; data = state.data |> Map.add name v }

let view state =
    for entry in state.log do
        printfn "%s" entry
    let mutable state = state
    for (Label label, vars, _) in state.pending do
        for v in vars do
            printfn $"[{label}]: enter a value for {v}"
let mutable game = State.ofList ["x", 10]
let reset() = game <- State.ofList []
let exec msg = GameLoop.gameIter &game view (update msg)
let eval label expr = exec (Eval(label, expr))
let supply name v = exec (Supply(name, v))
let d6 = Prim(fun state -> Const (rand 6))

eval "3+7-5+x" (Add(Add(Add(Const 3, Const 7), Const -5), Ref "x"))
eval "3+y-5+x" (Add(Add(Add(Const 3, Ref "y"), Const -5), Ref "x"))
supply "y" -10
supply "x" 12
reset()
eval "2d6+y" (Add(Add(d6, d6), Ref "y"))
supply "y" 3
