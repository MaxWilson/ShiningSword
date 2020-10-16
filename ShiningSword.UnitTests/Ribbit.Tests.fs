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
    blocked: Map<Key, Logic<unit>>
    workQueue: Logic<unit> Queue.d
    log: string list
    }
    with
    static member fresh = {
        blocked = Map.empty
        data = Map.empty
        workQueue = Queue.empty
        log = [] }
and Logic<'t> = Logic<State, 't>

type Prop<'t> = { name: string }
module Logic =
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

    let addToQueue logic state =
        { state with workQueue = Queue.append logic state.workQueue }

    // Like Task.Continue or Async.Map
    let continueWith f logic =
        let rec continueWith logic state =
            match logic state with
            | state, Ready v ->
                f v state
            | state, Awaiting logic ->
                state, Awaiting (logic |> continueWith)
        continueWith logic

    let andLog logic =
        logic |> continueWith (fun msg state -> { state with log = state.log @ [msg] }, Ready ())

    let spawn (logic: Logic<string>) state =
        match (logic |> andLog) state with
        | state, Ready () ->
            state
        | state, Awaiting logic ->
            state |> addToQueue logic

    let untilFixedPoint state =
        let queue = state.workQueue
        let state = { state with workQueue = Queue.empty }
        let processLogic state logic =
            match logic state with
            | state, Ready() -> state
            | state, Awaiting logic -> state |> addToQueue logic
        queue |> List.fold processLogic state

    type LogicBuilder() =
        member _.Return x =
            fun state -> state, Ready x
        member _.ReturnFrom logic = logic

    let logic = LogicBuilder()
open Logic

[<Tests>]
let tests = testList "ribbit.scenario" [
    let verifyLog msg state =
        Expect.contains (sprintf "Missing message: '%s' was not in %A.\nState: %A" msg state.log state) msg state.log
    testCase "stub" <| fun _ ->
        Expect.equal "Placeholder" 42 (RuleEngine.placeholder 40 + 2)
    testCase "Scenario 1: spawn, basic logging" <| fun _ ->
        State.fresh
        |> spawn (logic {
                return "abc"
            })
        |> verifyLog "abc"
    testCase "Scenario 2: workQueue and fixed points" <| fun _ ->
        let rest state = state, Awaiting(fun state -> state, Ready "xyz")
        State.fresh
        |> spawn (logic {
                return! rest
            })
        |> untilFixedPoint
        |> verifyLog "xyz"
    ptestCase "Scenario 3: demands and fulfillment" <| fun _ ->
        ()
    ]
