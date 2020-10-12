module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck

type Key = int * string
type Logic<'state, 'result> = 'state -> 'state * LogicOutput<'state, 'result>
and LogicOutput<'state, 'result> = Ready of 'result | Awaiting of Logic<'state, 'result>

type State = {
    data: Map<Key, obj>
    blocked: Map<Key, Logic<State, string>>
    }
    with static member fresh = { blocked = Map.empty; data = Map.empty }

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

[<Tests>]
let tests = testList "ribbit.unit" [
    testCase "stub" <| fun _ ->
        Expect.equal "Placeholder" 42 (RuleEngine.placeholder 40 + 2)
    ]
