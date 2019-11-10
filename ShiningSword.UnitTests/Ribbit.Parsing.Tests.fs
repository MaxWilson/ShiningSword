module Ribbit

open Expecto

[<Tests>]
let tests = testList "parsing" [
    testCase "My first test" <| fun _ ->
        Expect.isTrue true "I broke reality"
    ]
