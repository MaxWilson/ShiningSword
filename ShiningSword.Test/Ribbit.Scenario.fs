module Ribbit

open Xunit
open Common
open DataEngine
open DataEngine.Functions

[<Fact>]
let EmptyModel() =
    let state = init()
    let m = state.data
    Assert.Equal(Log.empty, m.log)
    Assert.Equal([], m.properties)
    Assert.Equal(Roster.empty, m.roster)
    Assert.Equal(emptyView, state.view)
    state

type FakeStorage() =
    interface IDataStorage with
        member this.Save (label:Label) data callback =
            Common.notImpl()
        member this.Load label callback =
            Common.notImpl()
let storage = FakeStorage()

let execute statement m =
    let mutable state' = m
    execute storage m statement (fun m' -> state' <- m')
    state'

[<Fact>]
let RollDice() =
    let m = EmptyModel() |> execute "3d6" 
    match m.view.lastOutput |> List.exactlyOne |> snd with
    | Hierarchy.Nested(txt, [Hierarchy.Leaf txt2]) ->
       Assert.StartsWith("3d6: ", txt)
       let rest = txt.Substring("3d6: ".Length)
       Assert.Equal(rest, txt2)
       let v = int rest
       Assert.True(betweenInclusive 3 18 v)
    | _ ->
        failwith "A regular 3d6 roll is not interesting enough to require more than a summary node and a simple leaf node"
    m
