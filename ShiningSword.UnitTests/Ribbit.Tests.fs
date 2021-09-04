module Ribbit.Tests

open Common
open Expecto
open FsCheck
open Domain.Model
open Domain.Model.Ribbit
open Domain.Engine.Ribbit
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

let executeScript script _ : RuntimeValue list = notImpl()

[<Tests>]
let tests = testList "ribbit.scenario" [
    testCase "Scenario 1" <| fun _ ->

        let ac, sp =
            withState Game.fresh (state {
                let! bob = transform1 (Game.add "Bob")
                do! transform (Game.define "getShieldBonus" [
                    If(BinaryOp(Dereference(IndirectDataRef("self", "sp")), Const(Number(2)), AtLeast),
                        Sequence [
                            Assign(IndirectDataRef("self", "sp"),
                                        BinaryOp(Dereference(IndirectDataRef("self", "sp")), Const(Number(2)), Minus))
                            Return (Const (Number 5))
                        ],
                        Some(Return (Const (Number 0))))
                ])
                let! eventId =
                    transform1 (
                      Game.start
                        [
                            Assign(LocalRef "__event_1", StartEvent("getShieldBonus", ["self", Const (Id bob)]))
                            Assign(LocalRef "ac", BinaryOp(Dereference(DataRef(bob, "AC")), Dereference(IndirectEventRef("__event_1")), Plus))
                            Return (Dereference (LocalRef "ac"))
                        ] [] |> compile)
                let api: Ribbit.Api<_> = { dereference = Game.dereference; defer = Game.defer;
                             resume = Game.resume; supply = Game.supply; start = Game.startByName }
                do! transform (supply api (bob, "AC") (Number 18) >> supply api (bob, "sp") (Number 5))
                let! (g: Game) = get()
                return ((g.events.[eventId]), g.data.[bob].properties.["sp"])
            })
        Expect.equal ac (EventResult (Number 23)) "Bob's AC should compute as 23 since Bob has 5 SP remaining"
        Expect.equal sp (Number 3) "Bob should have expended 2 of his remaining 5 SP"
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

    testCase "Scenario 2" <| fun _ ->
        let g = Game.fresh |> executeScript """
            define getShieldBonus:
                if self.sp at least 2
                  self.sp loses 2
                  return 5
                else return 0

            add bob
            let ac = bob.AC + getShieldBonus with self=bob
            bob.AC = 18
            bob.sp = 5
            ac
            """
        test <@ g |> List.last = Number 23 @>
        ()
    ]

