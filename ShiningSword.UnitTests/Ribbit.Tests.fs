module Ribbit.Tests

open Common
open Expecto
open FsCheck
open Domain.Model
open Domain.Model.Ribbit
open Domain.Engine.Ribbit

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
    ]
