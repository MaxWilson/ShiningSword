module Data

open Expecto
open Data
open Optics

[<Tests>]
let tests = testList "Data structures." [
    testCase "Queue.queue" <| fun _ ->
        let q = Queue.create |> Queue.add 1 |> Queue.add 2 |> Queue.add 3
        let v, q = q |> Queue.(|Pop|) |> Option.get
        Expect.equal v 1 "1 should be the first thing off the queue"
        Expect.equal (q |> Queue.normalize |> fun x -> x.frontList) [2;3] "2 and 3 should be left on the queue"
        let q = q |> Queue.add 4 |> Queue.add 5
        let vs = q |> Queue.normalize |> fun x -> x.frontList
        Expect.equal vs [2;3;4;5] "Wrong order"
    testCase "Lens.tuples" <| fun _ ->
        let data = (1,2)
        let lfst f = lens (fst) (fun v (_, snd) -> v, snd) f
        let compose = (lfst => lens snd (fun v (fst, _) -> fst,v))
        Expect.equal (read lfst data) 1 "Unexpected read result"
        Expect.equal (write lfst 99 data) (99, 2) "Unexpected write result"
        Expect.equal (over lfst ((*)2) data) (2, 2) "Unexpected over result"
        Expect.equal (over compose ((*)11) ((5,6),7)) ((5,66),7) "Unexpected over result"
        ()
    ]
