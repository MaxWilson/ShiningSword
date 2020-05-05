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
type MvuLite<'model, 'msg, 'view> = {
    init: unit -> 'model * CommandLite<'msg> list;
    view: 'model -> ('msg -> unit) -> 'view;
    update: 'model -> 'msg -> 'model * CommandLite<'msg> list
    }
    with
    static member IterateToFixedPoint(terminate: ('model -> 'msg -> bool), mvu: MvuLite<'model, 'msg, 'view>) =
        let mutable msgs = []
        let queueMsg msg = msgs <- msg :: msgs
        let mutable model, initCmds = mvu.init()
        initCmds |> List.iter (fun cmd -> cmd queueMsg)
        let mutable isTerminated = false
        let update msg =
            if isTerminated then ()
            elif terminate model msg then isTerminated <- true
            else
                let model', cmds = mvu.update model msg
                cmds |> List.iter (fun cmd -> cmd queueMsg)
                model <- model'
        let drain() =
            let msgsToExecute = msgs |> List.rev
            msgs <- []
            msgsToExecute |> List.iter update
        drain()
        while not isTerminated do
            mvu.view model queueMsg |> ignore
            drain()
        model
and CommandLite<'msg> = ('msg -> unit) -> unit

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
    testProperty "MVU eventually removes everything from the list" <| fun (model: int list) ->
        let lst = model |> List.map (fun n -> abs (n % 10)) // construct smaller input
        let r = System.Random()
        let mvu = {
            init = fun () -> (lst, []), []
            view = fun (remaining, guessHistory) dispatch ->
                dispatch (r.Next 10)
            update = fun model msg ->
                let remaining, history = model
                let remaining =
                    match remaining |> List.tryFindIndex ((=)msg) with
                    | Some ix -> remaining.[..(ix-1)] @ remaining.[ix+1..]
                    | _ -> remaining
                (remaining, msg::history), []
            }
        // it takes at least as many guesses to finish everything as there was originally stuff in the model
        MvuLite.IterateToFixedPoint ((fun (remaining, _) _ -> remaining = []), mvu)
        |> snd
        |> List.length
        >= model.Length
    ]

