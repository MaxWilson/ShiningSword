module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck
open Domain.RuleEngine
open Domain.RuleEngine.Logic
open Domain.Model
open Domain.Model.Ribbit
open Domain.RuleEngine
open Domain.RuleEngine.Logic
open Domain.RuleEngine.Logic.Builder

[<Tests>]
let tests = testList "ribbit.scenario" [
    let verifyLog msg state =
        let msgs = state.log |> Queue.read |> List.map (fun id -> state.settled.[id])
        Expect.contains (sprintf "Missing message: '%s' was not in %A.\nState: %A" msg msgs state) msg msgs
    let verify f state =
        match f state with
        | Some msg ->
            Tests.failtest msg
        | None -> state
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
        let HP = intProp "HP"
        State.fresh
        |> spawn (logic
            {
                let! hp = read 2 HP
                return sprintf "Bob has %d HP" hp
            })
        |> verify (fun state ->
            if state.log.IsEmpty && state.outstandingQueries |> Map.containsKey (2, "HP") then None
            else Some "Nothing should complete until 2's HP are available"
            )
        |> fulfill (2, HP) 27
        |> verify (function
            | { log = _::_  } -> Some "Nothing should complete until 2's HP are available"
            | state when not state.outstandingQueries.IsEmpty -> Some "Should be unblocked now"
            | _ -> None
            )
        |> untilFixedPoint
        |> verifyLog "Bob has 27 HP"
    ]
