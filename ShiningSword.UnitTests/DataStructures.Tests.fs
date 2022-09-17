module DataStructures
open Expecto
open Expecto.Flip

let test msg quotation =
    try
        Swensen.Unquote.Assertions.test quotation
    with :? Expecto.AssertException as err->
        failtest $"{msg} {err.Message}"

type TestTrie =
    | Leaf of label: string
    | Branch of label: string * TestTrie list

[<Tests>]
let tests = testList "Basic.DataStructures" [
    // Basic data structures
    testCase "FastList" <| fun _ ->
        //  should behave sanely, not lose data, remember order. We don't verify that it's fast though, that will be obvious enough from reading source code. We just verify correctness.
        let lst = FastList.ofSeq [1;3;2]
        test "Should have three entries initially" <@ ([1;3;2],3) = (lst.inOrder(), lst.Length) @>
        test "Adding to the list should give us an index that we can use to refer back to what we just added" <@ 7 = (let ix, lst = lst.AddM 7 in lst[ix]) @>
    let trie =
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
        //// Trie functions should append and retrieve data correctly
        let nav = function Leaf l -> l, None | Branch(l, children) -> l, Some children
        let updateNode (ix, updatedChild) = function
            | Branch(parentValue, children) -> Branch(parentValue, children |> List.mapi (fun j child -> if ix = j then updatedChild else child))
            | Leaf _ -> shouldntHappen()
        let updateValue v = function (Leaf l) -> Leaf v | Branch(l, children) -> Branch(v, children)
        let appendTo newValue = function (Leaf parentValue) -> Branch(parentValue, [Leaf newValue]) | Branch(parentValue, children) -> Branch(parentValue, children@[Leaf newValue])
        for ix, nodeLabel in [ [], "Root"; [0],"A"; [2],"C"; [-1], "D"; [0;1;2], "A2Z"; [2;-1;-1], "C2X"; [4], "Invalid"; [2;-10], "Invalid" ] do
            testCase $"Try to read {ix}" <| fun _ ->
                test $"{ix} should return {nodeLabel}" <@ nodeLabel = (Trie.read nav ix trie |> Option.defaultValue "Invalid") @>
            testCase $"Try to write {ix}" <| fun _ ->
                let g = System.Guid.NewGuid().ToString()
                let trie = Trie.replace nav updateNode (updateValue g) ix trie
                let expectedValue = match nodeLabel with "Invalid" -> "Invalid" | _ -> g
                test $"After update, {ix} should return {g}" <@ expectedValue = (Trie.read nav ix trie |> Option.defaultValue "Invalid") @>
            testCase $"Try to append to {ix}" <| fun _ ->
                let g = System.Guid.NewGuid().ToString()
                let trie = Trie.replace nav updateNode (appendTo g) ix trie
                let expectedValue = match nodeLabel with "Invalid" -> "Invalid" | _ -> g
                test $"After append, {ix} should still return {nodeLabel}" <@ nodeLabel = (Trie.read nav ix trie |> Option.defaultValue "Invalid") @>
                test $"""After append, {ix@[-1] |> sprintf "%A"} should return {g}""" <@ expectedValue = (Trie.read nav (ix@[-1]) trie |> Option.defaultValue "Invalid") @>
        ]
    ]
