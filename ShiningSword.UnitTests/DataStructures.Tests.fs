module DataStructures
open Expecto
open Expecto.Flip

[<Tests>]
let tests = testList "Basic data structures" [
    testCase "FastList should behave sanely, not lose data, remember order. We don't verify that it's fast though, that will be obvious enough from reading source code. We just verify correctness." <| fun _ ->
        let lst = FastList.ofSeq [1;3;2]
        Expect.equal "Should have three entries initially" ([1;3;2],3) (lst.inOrder(), lst.Length)
        Expect.equal "Adding to the list should give us an index that we can use to refer back to what we just added" 7 (let ix, lst = lst.AddM 7 in lst[ix])
    ]
