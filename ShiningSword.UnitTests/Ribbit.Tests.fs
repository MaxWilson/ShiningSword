module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Swensen.Unquote
open Domain.Ribbit

let define (rules: string) (ribbit:RibbitData) = notImpl ribbit
let execute (commands: string) (ribbit:RibbitData) = notImpl ribbit
let withRolls rollPlugin (ribbit:RibbitData) = notImpl ribbit
let addCreatures team creatureList src (ribbit:RibbitData) = notImpl ribbit
type Unknown() =
    do
        notImpl()

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
        let skipWhitespace n (line:string) =
            let rec loop i =
                if i >= line.Length then System.String.Empty
                elif line.[i] |> System.Char.IsWhiteSpace |> not || i >= n then line.Substring(i).TrimEnd()
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

module Expect =
    let fail msg = Expect.equal msg true false

[<Tests>]
let tests = testList "Ribbit.scenario" [

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
        Expect.equal "Should trim first line and prefixes evenly" "abc\ndef\n   hij\n   k\nab\n a" (trimFront x)

    ]

