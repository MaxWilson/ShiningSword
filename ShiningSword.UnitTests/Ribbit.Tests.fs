module Ribbit.Tests

open Common
open Expecto
open FsCheck
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Swensen.Unquote

let trimFront (input: string) =
    let newLine = "\n" // can't use System.Environment.Newline for F# compile-time strings, which are just \n apparently
    let lines = input.Split(newLine) |> Array.skipWhile System.String.IsNullOrWhiteSpace
    if lines.Length = 0 then System.String.Empty
    else
        let countWhitespace (line:string) =
            let rec loop i =
                if i >= line.Length || line.[i] |> System.Char.IsWhiteSpace |> not then i
                else loop (i+1)
            loop 0
        let skipWhitespace N (line:string) =
            let rec loop i =
                if i >= line.Length then System.String.Empty
                elif line.[i] |> System.Char.IsWhiteSpace |> not || i >= N then line.Substring(i).TrimEnd()
                else loop (i+1)
            loop 0
        let prefixN = lines.[0] |> countWhitespace
        lines |> Array.map (skipWhitespace prefixN) |> fun x -> System.String.Join(newLine, x).TrimEnd()

let basicAttack = testCase "Basic attack definition can be parsed" <| fun _ ->
    let ribbitDef = """
        define attack as
            costs 1 attack
            set attackRoll = roll 1d20
    """
    ()

[<Tests>]
let tests = testList "ribbit.scenario" [

    testCase "Lemma 1" <| fun _ ->
        let x = """
            abc
            def
               hij
               k
            ab
             a
            """
        test <@ "abc\ndef\n   hij\n   k\nab\n a" = trimFront x @>
        ("abc\ndef\n   hij\n   k\nab\n a", "Should trim first line and prefixes evenly")
        ||> Expect.equal (trimFront x)

    ]

