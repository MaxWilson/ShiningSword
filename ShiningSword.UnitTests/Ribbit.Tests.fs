module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck

type Demand = int * string
type State = Map<int * string, obj>
type Eventual<'state, 't> = Eventual of ('state -> 'state * Result<'state, 't>)
and Result<'state, 't> =
    | Awaiting of Demand list * Eventual<'state, 't>
    | Ready of 't
let execute (Eventual(f)) state =
    f state

[<Tests>]
let tests = testList "ribbit.unit" [
    testCase "stub" <| fun _ ->
        Expect.equal "Placeholder" 42 (RuleEngine.placeholder 40 + 2)
    ]
