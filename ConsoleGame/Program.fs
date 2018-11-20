// Learn more about F# at http://fsharp.org

open System
open Model
open Operations
open Interact
open Model.Types

[<EntryPoint>]
let main argv =
    let add id teamId vals roster =
        roster |> Map.add id { RosterEntry.current = vals; RosterEntry.original = vals; id = id; team = teamId; position = 0,0 }
    let roster = Map.empty |> add 1 1 { StatBlock.name = "Bob"; StatBlock.hp = 30 }
                           |> add 2 2 { StatBlock.name = "Fred"; StatBlock.hp = 30 }
    let exec g =
        let next = Intention(Queries.IntentionQuery.Query 1, fun i -> Operations.execute g [1, i])
        let rec loop next =
            match next with
            | Intention(Queries.IntentionQuery.Query(id),_) ->
                let r,log = g
                printfn "What does %s want to do?" (r.[id].current.name)
            | _ -> failwithf "Not implemented: No match for %A" next
            match Operations.Interact.trampoline g next (Console.ReadLine()) with
            | Some f ->
                f()
            | None ->
                loop next
        loop next
    let (outcome, log) = exec (roster, Log.empty)
    printfn "%s" log
    for KeyValue(_, creature) in outcome do
        printfn "%s: %d out of %d HP left" creature.current.name creature.original.hp creature.current.hp
    0 // return an integer exit code
