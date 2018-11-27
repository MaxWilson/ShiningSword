// Learn more about F# at http://fsharp.org

open System
open Model
open Operations
open Interact
open Model.Types

type StateFunc<'a, 'result> =
    StateFunc of 'result * next:('a -> StateFunc<'a, 'result>)
module StateFuncM =
    let apply arg (StateFunc(_, next)) = next arg
    let get (StateFunc(v, _)) = v
    let rec fold f state v =
        let v = f state v
        StateFunc(v, fold f v)

type Eventual<'state, 'result> =
    | Final of 'result
    | Intermediate of ('state -> 'state * Eventual<'state, 'result>)
module Eventual =
    let reduce s = function
        | Final v as m -> s, m
        | Intermediate f -> f s
    let bind m f =
        let rec progress m a =
            let s, m = reduce a m
            match m with
            | Final v -> f(s,v)
            | Intermediate _ -> s, Intermediate (progress m)
        Intermediate (progress m)
    let rec resolve s = function
        | Final v -> v
        | m ->
            let s, m = reduce s m
            resolve s m
let thunk v _ = v
let increment = StateFuncM.fold (fun state v -> state + v)
increment "This is " "bob" |> StateFuncM.apply "?" |> StateFuncM.get

let rec loop i arg =
    let (next:StateFunc<string, string>),(name:string) = arg
    if i > 1 then
        let StateFunc(v:string, _) as next = next |> StateFuncM.apply name
        v, Intermediate(loop (i-1))
    else
        (i, f), Final(i, f)
let logic (StateFunc(_lastValue, next), name) : string * Eventual<_,_> =
    loop name i next
let rec e = Eventual.bind (Final "Bob": Eventual<int * StateFunc<string, string>, _>) (function (state, name) ->
    let StateFunc(i, next) = state
    let rec loop (name:string) i (f:StateFunc<string, string>) =
        if i > 1 then
            let StateFunc(v: string, next) = StateFuncM.apply name f
            v, Intermediate(loop name (i-1) next)
        else
            (i, f), Final(i, f)
    loop name i next
    )
let v = e |> Eventual.resolve (10, fun i name -> "This is" + name)
printfn "%A" v

type InteractionBuilder() =
    member this.Bind(q: Queries.IntentionQuery, continuation) =
        Intention(q, continuation)
    member this.Bind(interaction: Interaction<'a>, continuation: 'a -> Interaction<'b>) : Interaction<'b> =
        match interaction with
        | Immediate v ->
            (continuation v)
        | Interact q ->
            let i =
                match q with
                | Intention(q, f) ->
                    Intention(q, f >> continuation)
                | StatNumber(q, f) ->
                    StatNumber(q, f >> continuation)
                | StatText(q, f) ->
                    StatText(q, f >> continuation)
                | Confirmation(q, f) ->
                    Confirmation(q, f >> continuation)
            interaction
    member this.Return(x) = Immediate x
    member this.ReturnFrom(x) = x

let interaction = InteractionBuilder()

[<EntryPoint>]
let main argv =
    let add id teamId vals roster =
        roster |> Map.add id { RosterEntry.current = vals; RosterEntry.original = vals; id = id; team = teamId; position = 0,0 }
    let roster = Map.empty |> add 1 1 { StatBlock.name = "Bob"; StatBlock.hp = 30 }
                           |> add 2 2 { StatBlock.name = "Fred"; StatBlock.hp = 30 }
    let consoleInteraction (errMsg: string option) (g: GameState) interaction =
        match errMsg with
        | Some msg -> printfn "%s" msg
        | None -> ()
        match interaction with
        | Intention(Queries.IntentionQuery.Query(id),_) ->
            let r,log = g
            printfn "What does %s want to do?" (r.[id].current.name)
        | _ -> failwithf "Not implemented: consoleInteraction cannot render interaction %A" interaction
        Console.ReadLine()
    let executeOneRound g =
        // an event loop which resolves an interaction before continuing. Analagous to Async.RunSynchronously or the browser event loop.
        let rec unlock errMsg interaction =
            match Operations.Interact.tryUnlock g interaction (consoleInteraction errMsg g interaction) with
            | Some f ->
                f
            | None ->
                unlock (Some "Sorry, I couldn't understand that.") interaction
        let resolve interaction =
            match interaction with
            | Immediate v -> v
            | Interact i ->
                unlock None i
        let declareAndExecuteImmediately id g = interaction {
            let! intention = Queries.IntentionQuery.Query id
            return Operations.execute [id, intention] g
            }
        let declare id = interaction {
            let! intention = Queries.IntentionQuery.Query id
            return id, intention
            }
        let rec declareAll ids : Interaction<Declarations> = interaction {
                match ids with
                | [] -> return []
                | h::t -> return []
                    //let decl = declareAll t
                    //return! interaction.Bind(declareAll t, fun rest -> interaction.Return [])
                    //return! interaction.Bind(decl, id)
                    //let! rest = declareAll t
                    //return []
                    //return! interaction.Bind(declareAll t, id)
                    //let decl = declareAll t
                    //let! (d : Id * Intention) = declare h
                    //let! (rest : (Id * Intention) list) = decl
                    //return d::rest
            }
        let y = interaction.Return ([]: Declarations)
        let x = interaction.Bind(declareAll [1;2], fun rest -> y)

        g |> Operations.execute (declareAll [1;2] |> resolve)

    let mutable state = (roster, Log.empty)
    while (fst state) |> Seq.exists (function KeyValue(_, c) -> c.current.hp <= 0) |> not do
        let (outcome, log) = executeOneRound state
        state <- outcome, log
        printfn "%s" log
        for KeyValue(_, creature) in outcome do
            printfn "%s: %d out of %d HP left\n\tPosition: %A" creature.current.name creature.current.hp creature.original.hp creature.position
    0 // return an integer exit code
