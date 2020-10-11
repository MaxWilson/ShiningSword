module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck

[<Tests>]
let tests = testList "ribbit.unit" [
    testCase "stub" <| fun _ ->
        Expect.equal "Placeholder" 42 (RuleEngine.placeholder 40 + 2)
    ]
