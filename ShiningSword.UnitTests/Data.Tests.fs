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
    testCase "Optics.lenses" <| fun _ ->
        let data = (1,2)
        let fst_ f = lens (fst) (fun v (_, snd) -> v, snd) f
        let compose = (fst_ => lens snd (fun v (fst, _) -> fst,v))
        Expect.equal (read fst_ data) 1 "Unexpected read result"
        Expect.equal (write fst_ 99 data) (99, 2) "Unexpected write result"
        Expect.equal (over fst_ ((*)2) data) (2, 2) "Unexpected over result"
        Expect.equal (over compose ((*)11) ((5,6),7)) ((5,66),7) "Unexpected over result"
        ()
    testCase "Optics.prisms" <| fun _ ->
        let data1 = ["a";"b"]
        let data2 = Ok "ok"
        let list_ ix = lens (List.item ix) (fun v l -> l.[0..(ix-1)]@v::l.[ix+1..])
        Expect.equal (read (list_ 0) data1) "a" "Unexpected read result"
        Expect.equal (write (list_ 0) "99" data1) ["99"; "b"] "Unexpected write result"
        Expect.equal (over (list_ 0) (fun x -> x + x) data1) ["aa";"b"] "Unexpected over result"
        ()
    ]