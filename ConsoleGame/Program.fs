// Learn more about F# at http://fsharp.org
#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#r @"..\src\bin\Debug\netstandard2.0\ShiningSword.dll"
#endif

open System
open Model
open Operations
open Interact
open Model.Types
open Model.Operations.Queries
open Wilson.Packrat

/// Note: Eventual.Final._stateTypeAdapter is just a type kludge (placeholder type) used at monad construction time.
/// You should not necessary expect it to always be called during evaluation--your logic should be written not to
/// break even if another function with the same type signature is substituted, e.g. to update an accumulator or do logging.
type Eventual<'arg, 'intermediate, 'result> =
    | Final of 'result * _stateTypeAdapter: ('arg -> 'intermediate)
    | Intermediate of ('arg -> (Eventual<'arg, 'intermediate, 'result>) * 'intermediate)
module Eventual =
    let bind m f =
        let reduce s = function
            | Final(_v, typeAdapt) as m -> m, typeAdapt s
            | Intermediate f -> f s
        let rec progress m a =
            let m, s = reduce a m
            match m with
            | Final(v,_) -> f s v
            | Intermediate _ -> Intermediate (progress m), s
        Intermediate (progress m)
    /// Trampoline until Final state is reached, resolving queries back
    /// to answers using the fResolveQuery. E.g. fResolveQuery might
    /// turn an Interact<'t> into a 't by calling Console.WriteLine + Readline()
    let resolve (fResolveQuery: 'intermediate -> 'arg) (fRequery: 'arg -> 'intermediate) =
        let reduce fRequery s = function
            | Final(_) as m -> m, fRequery s
            | Intermediate f -> f s
        let rec resolve s monad =
            match monad with
            | Final(v,_) -> v
            | m ->
                let m, s = reduce fRequery s m
                resolve (fResolveQuery s) m
        resolve

for x in 1..10 do
    Eventual.bind (Final ("Bob", id): Eventual<string, string, string>)
        (fun prefix name ->
            let rec loop i (accum:string) : Eventual<string, _, _> =
                if i > 0 then
                    Intermediate(fun arg ->
                                    loop (i-1) (accum + i.ToString() + arg), arg)
                else
                    Final(accum, id)
            (loop x prefix), name)
    |> Eventual.resolve id id "Hi my name is "
    |> printfn "%A"

type InteractionQuery =
    | Intention of IntentionQuery
    | StatNumber of StatQuery<int>
    | StatText of StatQuery<string>
    | Confirmation of string

type Interact<'result> =
    | Intention of IntentionQuery * (Eventual<Intention, InteractionQuery, 'result>)
    | StatNumber of StatQuery<int> * (Eventual<int, InteractionQuery, 'result>)
    | StatText of StatQuery<string> * (Eventual<string, InteractionQuery, 'result>)
    | Confirmation of string * (Eventual<bool, InteractionQuery, 'result>)

type Interactive<'result> = Eventual<string, InteractionQuery, 'result>

type InteractionBuilder<'a, 'b>(typeAdapter: 'a -> 'b) =
    let wrap q continuation recognizer =        
        let rec this =
            Intermediate(fun arg ->
                match arg |> recognizer with
                | Some arg -> 
                    continuation arg, q
                | _ -> this, q)
        this
    // todo: make more generic, with inputs other than string        
    member this.Bind((q:InteractionQuery, recognizer: string -> 'arg option), continuation: ('arg -> Interactive<_>)): Interactive<_> =
        wrap q continuation recognizer
    member this.Bind(interaction: Interactive<'a>, continuation: 'a -> Interactive<'b>) : Interactive<'b> =
        Eventual.bind interaction (fun state x -> continuation x, state)
    member this.Return(x) = Final (x, typeAdapter)
    member this.ReturnFrom(x) = x

module Query =
    let tryParse recognizer arg =
        match ParseArgs.Init arg |> recognizer with
        | Some(v, End) -> Some v
        | _ -> None

    let intention txt =
        InteractionQuery.Intention(IntentionQuery.Query txt), (tryParse Recognizer.``|Intention|_|``)
    let statNumber id statName =
        InteractionQuery.StatNumber(StatQuery.Query(id, statName)), (tryParse Recognizer.``|Number|_|``)
    let statText id statName =
        InteractionQuery.StatText(StatQuery.Query(id, statName)), (tryParse Recognizer.``|FreeformText|_|``)
    let confirm txt =
        InteractionQuery.Confirmation txt, (tryParse Recognizer.``|Bool|_|``)

let interaction = InteractionBuilder(fun (s:string) -> InteractionQuery.Confirmation(sprintf "Did you mean to type '%s'?" s))
let z : Eventual<string, InteractionQuery, string> =
    interaction {
        let! x = Query.confirm "Do you want fries with that?"
        if x then
            return "That will be $2.00"
        else
            return "That will be $1.00"
    }
let resolve =
    let rec resolve s monad =
        match monad with
        | Final(v,_) -> v
        | Intermediate(f) as m ->
            printfn "%A?" s
            let answer = Console.ReadLine()
            let m, s = f answer
            resolve s m
    resolve
z |> resolve (Unchecked.defaultof<_>) |> printfn "Result: %s"

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
        let rec declareAll ids : Interactive<Declarations> = interaction {
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
