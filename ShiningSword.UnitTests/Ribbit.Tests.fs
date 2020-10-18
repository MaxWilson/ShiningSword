module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck

type Key = int * string
type Logic<'state, 'demand, 'result> = 'state -> 'state * LogicOutput<'state, 'demand, 'result>
and LogicOutput<'state, 'demand, 'result> = Ready of 'result | Awaiting of demand:'demand * followup:Logic<'state, 'demand, 'result>

module Queue =
    type 't d = 't list
    let append item queue = queue@[item]
    let empty = []

type State = {
    data: Map<Key, obj>
    blocked: Map<Key, Logic<unit> list>
    workQueue: Logic<unit> Queue.d
    log: string list
    }
    with
    static member fresh = {
        blocked = Map.empty
        data = Map.empty
        workQueue = Queue.empty
        log = [] }
and Demand = Key option
and Logic<'t> = Logic<State, Demand, 't>

type Prop<'t> = { name: string }
module Logic =
    let read id (prop: Prop<'t>) (state: State) =
        match state.data |> Map.tryFind (id, prop.name) with
        | Some (:? 't as v) -> Some v
        | _ -> None

    let demand (id, propName as key) logic state =
        match state.blocked |> Map.tryFind key with
        | None ->
            { state with blocked = state.blocked |> Map.add (id, propName) [logic] }
        | Some current ->
            { state with blocked = state.blocked |> Map.add (id, propName) (current@[logic]) }

    let addToQueue logic state =
        { state with workQueue = Queue.append logic state.workQueue }

    let fulfill (id, prop: Prop<'t>) (value: 't) state =
        let state = { state with data = state.data |> Map.add (id, prop.name) (box value) }
        match state.blocked |> Map.tryFind (id, prop.name) with
        | None | Some [] -> state
        | Some unblocked ->
            unblocked |> List.fold (flip addToQueue) ({ state with blocked = state.blocked |> Map.remove (id, prop.name) })

    // Like Task.Continue or Async.Map
    let continueWith f logic =
        let rec continueWith logic state =
            match logic state with
            | state, Ready v ->
                f v state
            | state, Awaiting(demand, logic) ->
                state, Awaiting (demand, logic |> continueWith)
        continueWith logic

    let andLog logic =
        logic |> continueWith (fun msg state -> { state with log = state.log @ [msg] }, Ready ())

    let processLogic = function
        | state, Ready () ->
            state
        | state, Awaiting(Some demand', logic) ->
            state |> demand demand' logic
        | state, Awaiting(None, logic) ->
            state |> addToQueue logic

    let spawn (logic: Logic<string>) state =
        (logic |> andLog) state |> processLogic

    let rec untilFixedPoint state =
        let queue = state.workQueue
        let state = { state with workQueue = Queue.empty }
        match queue |> List.fold (fun state logic -> logic state |> processLogic) state with
        | { workQueue = [] } as state -> state
        | state -> state |> untilFixedPoint

module LogicBuilder =
    type Builder() =
        member _.Return x =
            fun state -> state, Ready x
        member _.ReturnFrom logic = logic
        member _.Bind (logic: Logic<'t>, rhs: 't -> Logic<'r>) : Logic<'r> =
            Logic.continueWith rhs logic


    let logic = Builder()
    let read id prop state =
        match Logic.read id prop state with
        | Some v -> state, Ready v
        | None ->
            let actualRead state =
                let value = Logic.read id prop state |> Option.get
                state, Ready value
            state, Awaiting(Some (id, prop.name), actualRead)

open Logic
open LogicBuilder

[<Tests>]
let tests = testList "ribbit.scenario" [
    let verifyLog msg state =
        Expect.contains (sprintf "Missing message: '%s' was not in %A.\nState: %A" msg state.log state) msg state.log
    let verify f state =
        match f state with
        | Some msg ->
            Tests.failtest msg
        | None -> state
    testCase "stub" <| fun _ ->
        Expect.equal "Placeholder" 42 (RuleEngine.placeholder 40 + 2)
    testCase "Scenario 1: spawn, basic logging" <| fun _ ->
        State.fresh
        |> spawn (logic {
                return "abc"
            })
        |> verifyLog "abc"
    testCase "Scenario 2: workQueue and fixed points" <| fun _ ->
        let rest state = state, Awaiting(None, fun state -> state, Awaiting(None, fun state -> state, Ready "xyz"))
        State.fresh
        |> spawn (logic {
                return! rest
            })
        |> untilFixedPoint
        |> verifyLog "xyz"
    testCase "Scenario 3: demands and fulfillment" <| fun _ ->
        let HP : Prop<int> = { name = "HP" }
        State.fresh
        |> spawn (logic
            {
                let! hp = read 2 HP
                return sprintf "Bob has %d HP" hp
            })
        |> verify (fun state ->
            if state.log.IsEmpty && state.blocked |> Map.containsKey (2, "HP") then None
            else Some "Nothing should complete until 2's HP are available"
            )
        |> fulfill (2, HP) 27
        |> verify (function
            | { log = _::_  } -> Some "Nothing should complete until 2's HP are available"
            | state when not state.blocked.IsEmpty -> Some "Should be unblocked now"
            | _ -> None
            )
        |> untilFixedPoint
        |> verifyLog "Bob has 27 HP"
    ]
