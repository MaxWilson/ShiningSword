// Learn more about F# at http://fsharp.org

open System
open Model
open Operations
open Interact

[<EntryPoint>]
let main argv =
    let exec r =
        let next = Intention(Queries.IntentionQuery.Query 22, fun i -> Operations.execute r [22, i])
        let rec loop next =
            match next with
            | Intention(Queries.IntentionQuery.Query(id),_) -> printfn "What does %d want to do?" id
            | _ -> failwithf "Not implemented: No match for %A" next
            match Operations.Interact.trampoline next (Console.ReadLine()) with
            | Some f ->
                f()
            | None ->
                loop next
        loop next
    printfn "Hello World from F#!"
    0 // return an integer exit code
