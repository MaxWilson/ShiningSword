module Ribbit

open Expecto

let t = testList "parsing" [
    testCase "max" <| fun _ ->
        Expect.isTrue true "Truth"
    ]

[<Tests>]
let tests = testList "parsing" [
    testCase "My first test" <| fun _ ->
        Expect.isTrue true "I broke reality"
    testProperty "Reverse of reverse of a list is the original list" <|
      fun (xs:list<int>) -> List.rev (List.rev xs) = xs
    testProperty "Addition is commutative" <| fun (x:int) (y:int) -> x + y = y + x
    t
    ]
