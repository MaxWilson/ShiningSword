module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck

type Key = int * string
type Logic<'state, 'result> = 'state -> 'state * LogicOutput<'state, 'result>
and LogicOutput<'state, 'result> = Ready of 'result | Awaiting of Logic<'state, 'result>

module Queue =
    type 't d = 't list
    let append item queue = queue@[item]
    let empty = []

type State = {
    data: Map<Key, obj>
    blocked: Map<Key, Logic>
    workQueue: Logic Queue.d
    log: string list
    }
    with
    static member fresh = {
        blocked = Map.empty
        data = Map.empty
        workQueue = Queue.empty
        log = [] }
and Logic = Logic<State, string>

type Prop<'t> = { name: string }
let read id (prop: Prop<'t>) (state: State) =
    match state.data |> Map.tryFind (id, prop.name) with
    | Some (:? 't as v) -> Some v
    | _ -> None

let readThen read demand (id: int, prop: Prop<'t>) (thenLogic: 't -> Logic<State, 't>) : Logic<State, 't> =
    let rec logic state =
        match read id prop state with
        | Some v ->
            thenLogic v state
        | None ->
            state |> demand (id, prop.name) logic
    logic

let demand (id, propName) logic state =
    { state with blocked = state.blocked |> Map.add (id, propName) logic }

let fulfill (id, prop: Prop<'t>) (value: 't) state =
    state

let spawn log logic state =
    state

let addToQueue logic state =
    state

let untilFixedPoint state =
    state

type LogicBuilder() =
    member _.Return x =
        fun state -> state, Ready x

let logic = LogicBuilder()

[<Tests>]
let tests = testList "ribbit.unit" [
    let verifyLog msg state =
        Expect.contains "Missing message" msg state.log
    testCase "stub" <| fun _ ->
        Expect.equal "Placeholder" 42 (RuleEngine.placeholder 40 + 2)
    testCase "Scenario1" <| fun _ ->
        State.fresh
        |> spawn id (logic {
                return "abc"
            })
        |> untilFixedPoint
        |> verifyLog "abc"
    ]
