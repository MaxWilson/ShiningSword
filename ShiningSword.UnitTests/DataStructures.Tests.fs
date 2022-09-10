module DataStructures
open Expecto
open Expecto.Flip

type TestTrie =
    | Leaf of label: string
    | Branch of label: string * TestTrie list

[<Tests>]
let tests = testList "Basic.DataStructures" [
    // Basic data structures
    testCase "FastList" <| fun _ ->
        //  should behave sanely, not lose data, remember order. We don't verify that it's fast though, that will be obvious enough from reading source code. We just verify correctness.
        let lst = FastList.ofSeq [1;3;2]
        Expect.equal "Should have three entries initially" ([1;3;2],3) (lst.inOrder(), lst.Length)
        Expect.equal "Adding to the list should give us an index that we can use to refer back to what we just added" 7 (let ix, lst = lst.AddM 7 in lst[ix])
    let t =
        let Branch label interior = Branch(label, interior)
        Branch "Root" [
            Branch "A" [
                Branch "A1" []
                Branch "A2" [Leaf "A2X"; Leaf "A2Y"; Leaf "A2Z"]
                ]
            Branch "B" [
                Leaf "B1"
                ]
            Branch "C" [
                Leaf "C1"
                Branch "C2" [Leaf "C2X"]
                ]
            Leaf "D"
            ]
    testList "Trie" [
        testCase $"Trie" <| fun _ ->
            ()
        //// Trie functions should append and retrieve data correctly
        let read = Trie.read (function Leaf l -> l, None | Branch(l, children) -> l, Some children)
        for ix, nodeLabel in [ [], "Root"; [0],"A"; [2],"C"; [-1], "D"; [0;1;2], "A2Z"; [2;-1;-1], "C2X"; [4], "Root"; [2;-10], "C" ] do
            testCase $"Try to read {ix}" <| fun _ ->
                Expect.equal $"{ix} should return {nodeLabel}" nodeLabel (read ix t)
        ]
    ]
