// Learn more about F# at http://fsharp.org

open System
open Model
open Operations
open Interact
open Model.Types

type InteractionBuilder() =
    member this.Bind(q: Queries.IntentionQuery, continuation) =
        Intention(q, continuation)
    member this.Return(x) = x
    member this.ReturnFrom(x) = Immediate x

let interaction = InteractionBuilder()

[<EntryPoint>]
let main argv =
    let add id teamId vals roster =
        roster |> Map.add id { RosterEntry.current = vals; RosterEntry.original = vals; id = id; team = teamId; position = 0,0 }
    let roster = Map.empty |> add 1 1 { StatBlock.name = "Bob"; StatBlock.hp = 30 }
                           |> add 2 2 { StatBlock.name = "Fred"; StatBlock.hp = 30 }
    let consoleInteraction (errMsg: string option) (g:GameState) interaction =
        match errMsg with
        | Some msg -> printfn "%s" msg
        | None -> ()
        match interaction with
        | Intention(Queries.IntentionQuery.Query(id),_) ->
            let r,log = g
            printfn "What does %s want to do?" (r.[id].current.name)
        | _ -> failwithf "Not implemented: consoleInteraction cannot render interaction %A" interaction
        Operations.Interact.trampoline g interaction (Console.ReadLine())
    let executeOneRound g =
        // an event loop which resolves an interaction before continuing. Analagous to Async.RunSynchronously or the browser event loop.
        let rec resolveInteraction errMsg interaction =
            match consoleInteraction errMsg g interaction with
            | Some f ->
                f
            | None ->
                resolveInteraction (Some "Sorry, I couldn't understand that.") interaction
        let declareAndExecuteImmediately id g = interaction {
            let! intention = Queries.IntentionQuery.Query id
            return Operations.execute [id, intention] g
            }
        let declare id = interaction {
            let! intention = Queries.IntentionQuery.Query id
            return id, intention
            }
        //let rec declareAll ids : Interact<(Id * Intention) list> = interaction {
        //        match ids with
        //        | [] -> return! []
        //        | h::t ->
        //            let! (list : Id * Intention) = declareAll t
        //            let! (d : Id * Intention) = declare h
        //            return d::rest
        //    }
        //g |> Operations.execute (declareAll [1;2])
        g |> Operations.execute ([declare 1; declare 2] |> List.map (resolveInteraction None))

    let mutable state = (roster, Log.empty)
    while (fst state) |> Seq.exists (function KeyValue(_, c) -> c.current.hp <= 0) |> not do
        let (outcome, log) = executeOneRound state
        state <- outcome, log
        printfn "%s" log
        for KeyValue(_, creature) in outcome do
            printfn "%s: %d out of %d HP left\n\tPosition: %A" creature.current.name creature.current.hp creature.original.hp creature.position
    0 // return an integer exit code
