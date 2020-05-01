module Impl.Tests
open Optics
open Optics.Operations
open Arch
open Impl

open Expecto
open FsCheck
open Swensen.Unquote

[<Tests>]
let t = testList "Impl.Definitions" [
    testProperty "Id: An Id is something that is always unique within a given context" <| fun start size someStr ->
        size >= 0 ==> lazy(
            let state = ((start : Id), (someStr: string))
            let collect = Seq.unfold (generateId (fst_()) >> (fun (a,b) -> Some(b,a))) state |> Seq.take size
            test <@ collect |> Seq.distinct |> Seq.length = size @>
            )
    ]

