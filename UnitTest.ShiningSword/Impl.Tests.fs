module Impl.Tests
open Optics
open Optics.Operations
open Arch
open Impl

open Expecto
open FsCheck
open Swensen.Unquote

type Creature = { name: string; xp: int }
type State = { id: int; creatures: Map<Id, Creature> }

[<Tests>]
let t = testList "Impl.Definitions" [
    testProperty "Id: An Id is something that is always unique within a given context" <| fun start size someStr ->
        size >= 0 ==> lazy(
            let state = ((start : Id), (someStr: string))
            let collect = Seq.unfold (generateId (fst_()) >> Some) state |> Seq.take size
            test <@ collect |> Seq.distinct |> Seq.length = size @>
            )
    testProperty "Inserting a thing inserts that thing" <| fun name xp ->
        let state = { id = 0; creatures = Map.ofSeq [] }
        let creature = { name = (name: string); xp = (xp : int) }
        let id_ = lens (fun st -> st.id) (fun v st -> { st with id = v })
        let creatures_ = lens (fun st -> st.creatures) (fun v st -> { st with creatures = v })
        let id, state = insert id_ creatures_ creature state
        test <@ state.creatures.[id] = creature @>
    ]

