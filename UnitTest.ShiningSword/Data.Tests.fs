module Data

open Expecto
open Data
open Optics
open Optics.Operations

[<Tests>]
let tests = testList "Data structures" [
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
        let compose = (fst_ => lens snd (fun v (fst, _) -> fst,v))
        Expect.equal (data |> read fst_) 1 "Unexpected read result"
        Expect.equal (data |> write fst_ 99) (99, 2) "Unexpected write result"
        Expect.equal (data |> over fst_ ((*)2)) (2, 2) "Unexpected over result"
        Expect.equal (over (inv fst_ => snd_) ((*)11) ((5,6),7)) ((5,66),7) "Unexpected over result"
        Expect.equal (over compose ((*)11) ((5,6),7)) ((5,66),7) "Unexpected over result"
        ()
    testCase "Optics.verify short-circuiting on read" <| fun _ ->
        let data = (1,2)
        Expect.equal (read (lens fst (fun v d -> failwith "shouldn't err")) data) 1 "Should be able to read efficiently without writing"
    testCase "Optics.prisms" <| fun _ ->
        let data1 = ["a";"b"]
        let data2 = Ok "ok"
        let list_ ix = prism (List.tryItem ix) (fun v l -> l.[0..(ix-1)]@v::l.[ix+1..])
        Expect.equal (read (list_ 0) data1) (Some "a") "Unexpected read result"
        Expect.equal (write (list_ 0) "99" data1) ["99"; "b"] "Unexpected write result"
        Expect.equal (over (list_ 0) (fun x -> x + x) data1) ["aa";"b"] "Unexpected over result"
        Expect.equal (read (list_ 2) data1) None "Unexpected read result"
        Expect.equal (write (list_ 2) "99" data1) ["a"; "b"] "Unexpected write result"
        Expect.equal (over (list_ 2) (fun x -> x + x) data1) ["a";"b"] "Unexpected over result"
        let ok_ = prism (function Ok v -> Some v | _ -> None) (fun v _ -> Ok v)
        let err_ = prism (function Error v -> Some v | _ -> None) (fun v _ -> Error v)
        Expect.equal (read ok_ data2) (Some "ok") "Unexpected read result"
        Expect.equal (read err_ data2) None "Unexpected read result"
        Expect.equal (write ok_ "99" data2) (Ok "99") "Unexpected write result"
        Expect.equal (write err_ "99" data2) (Ok "ok") "Unexpected write result"
        Expect.equal (over ok_ (fun x -> x + x) data2) (Ok "okok") "Unexpected over result"
        Expect.equal (over err_ (fun x -> x + x) data2) (Ok "ok") "Unexpected over result"
        ()
    testCase "Optics.compose prisms and lenses" <| fun _ ->
        let list_ ix = prism (List.tryItem ix) (fun v l -> l.[0..(ix-1)]@v::l.[ix+1..])
        let fst_ () = lens fst (fun v d -> v, snd d)
        let twice v = v + v
        Expect.equal (over (list_ 2 => fst_) twice [1,"a"; 2, "b"; 3, "c"]) [1,"a"; 2, "b"; 6, "c"] "Lens didn't compose with prism correctly"
        Expect.equal (over (list_ 3 => fst_) twice [1,"a"; 2, "b"; 3, "c"]) [1,"a"; 2, "b"; 3, "c"] "Lens didn't compose with prism correctly"
        Expect.equal (over (inv fst_ => list_ 2) twice ([1;2;3], "abc")) ([1;2;6], "abc") "Lens didn't compose with prism correctly"
        Expect.equal (over (inv fst_ => list_ 3) twice ([1;2;3], "abc")) ([1;2;3], "abc") "Lens didn't compose with prism correctly"
        Expect.equal (over (inv fst_ => list_ 3 => list_ 2) twice ([[];[];[5;5;];[1;2;3]], "abc")) ([[];[];[5;5];[1;2;6]], "abc") "Lens didn't compose with prism correctly"
    ]
