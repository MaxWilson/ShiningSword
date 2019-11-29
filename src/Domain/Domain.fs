[<AutoOpen>]
module Domain.Domain
open System
open Common
open Data
open Data.Functor
open Packrat
open Domain.Prelude
open Domain.Properties
open Domain.Dice
open Domain.Commands

type Event = Evaluation<Executable>
type Model = {
        properties: Property list
        roster: SymmetricMap.Data<Id, string>
        data: Map<Key, Value>
        blocking: SymmetricRelation.Data<Reference, Reference> // data needed --> data waiting.
        blockedThreads: {| eventId: Id |} list
        eventLog: FastList<Event>
    }
let fresh = { properties = []; roster = SymmetricMap.empty(); data = Map.empty; blocking = SymmetricRelation.empty; blockedThreads = []; eventLog = FastList.fresh() }
module Lens =
    let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
    let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
    let blocking = Lens.lens (fun d -> d.blocking) (fun v d -> { d with blocking = v})
    let blockedThreads = Lens.lens (fun d -> d.blockedThreads) (fun v d -> { d with blockedThreads = v})
    let eventLog = Lens.lens (fun d -> d.eventLog) (fun v d -> { d with eventLog = v})

let tryParseCommand (model: Model) txt =
    let startsWith txt prefix =
        (txt:string).StartsWith prefix
    let adaptor : RosterAdaptor = {
            isValidNamePrefix = fun prefix -> model.roster |> Data.SymmetricMap.values |> Seq.exists (fun n -> startsWith n prefix)
            tryNamePrefix = fun prefix ->
                model.roster |> Data.SymmetricMap.toSeq |> Seq.choose (fun (id, name) -> if startsWith name prefix then Some id else None) |> List.ofSeq
            tryId = flip Data.SymmetricMap.tryFind model.roster
            tryName = flip Data.SymmetricMap.tryFindValue model.roster
            }
    match ParseArgs.Init(txt, adaptor) with
    | Domain.Commands.Parse.DomainCommand(cmd, End) ->
        Some cmd
    | v -> None

let rec eval model expr =
    match expr with
    | Literal v -> Ready v
    | Ref (PropertyRef key) -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting ([PropertyRef key], expr)
    | Ref (EventRef id) ->
        match model.eventLog |> tryFind id with
        | Some (Ready v) -> Ready v
        | Some(Awaiting _) ->
            Awaiting(model.blocking.backward |> Map.find (EventRef id), expr)
        | None -> Awaiting ([EventRef id], expr) // this branch might never happen
    | BinaryOperation(lhs, op, rhs) ->
        match eval model lhs, eval model rhs with
        | Ready l, Ready r ->
            Ready(if op = Plus then l + r else l - r)
        | Awaiting (key, _), _ | _, Awaiting (key,_) -> Awaiting (key, expr)
    | BestN(n, exprs) ->
        match exprs |> List.tryMapFold(fun vs e -> match eval model e with Ready v -> Ok(v::vs) | Awaiting (k,_) -> Error k) [] with
        | Ok vs -> Ready (vs |> List.sortDescending |> List.take (min vs.Length n) |> List.sum)
        | Error k -> Awaiting (k, expr)
    | Negate e -> match eval model e with
                    | Ready v ->
                        match v with
                        | Number n -> Number (-n)
                        | Err _ -> v
                        | _ -> Err (sprintf "Cannot negate '%A'" v)
                        |> Ready
                    | v -> v

let addName name model =
    if (model: Model).roster |> SymmetricMap.tryFindValue name |> Option.isSome then // idempotence
        model
    else
        let id = 1 + (if model.roster |> SymmetricMap.isEmpty then 0 else model.roster |> SymmetricMap.toSeq |> Seq.map fst |> Seq.max)
        model |> Lens.over Lens.creatureIds (SymmetricMap.add id name)
let addProperty propertyName model =
    if (model:Model).properties |> List.exists (fun p -> p.name = propertyName) then
        model
    else
        { model with properties = model.properties @ [{name = propertyName}] }

let progress exec (completed: Set<Reference>) (model:Model): Model =
    // interacts with: model.blocking, model.eventLog, model.data (properties)
    // post-invariant: new model has reached a fixed point w/ regard to blocking/eventLog/data
    let rec iterate (completed: Set<Reference>) (model:Model): Model =
        // look up all the events which are directly or undirectly unblocked by this reference and progress them
        let progressing = completed |> Set.map (fun ref -> model.blocking.forward.[ref])
                                    |> Seq.collect id
                                    |> Seq.distinct
                                    |> Array.ofSeq
        let blocking' = progressing |> Array.fold (fun blocking ref -> blocking |> SymmetricRelation.removeAllForward ref) model.blocking
        // might still be blocked by other dependencies
        let unblocked = progressing |> Array.filter(fun ref -> blocking'.backward.ContainsKey ref |> not)
        let model = { model with blocking = blocking' }
        let reallyExecute ((completed, model: Model) as input) ref : Set<Reference> * Model =
            match ref with
            | PropertyRef(key) ->
                match eval model (Ref ref) with
                | Ready _ -> (completed |> Set.add ref), model // this property is "complete" now in this model, signal for further chaining
                | Awaiting _ -> input // this property is not completed yet
            | EventRef(eventId) ->
                match model.eventLog.rows.[eventId] with
                | Ready v -> input // shouldn't happen probably
                | Awaiting(executable) ->
                    let model = exec model eventId executable
                    match model.eventLog |> find eventId with
                    | (Ready v) ->
                        (completed |> Set.add (EventRef eventId)), model
                    | Awaiting(executions) ->
                        completed, model
        match unblocked |> Array.fold reallyExecute (Set.empty, model) with
        | c, model when c.IsEmpty -> model
        | completed, model -> iterate completed model
    iterate completed model

let private resolve eventId value model =
    model |> Lens.over Lens.eventLog (transform eventId (thunk (Ready value)))

let await eventId refs model =
    let addKeys eventId blocking =
        refs
        |> List.fold (fun data key -> data |> SymmetricRelation.add key (EventRef eventId)) blocking
    { model with blockedThreads = {| eventId = eventId |} :: model.blockedThreads }
            |> Lens.over Lens.blocking (addKeys eventId)

let rec private executeHelper model eventId executable =
    match executable with
    | Evaluate expr ->
        match eval model expr with
        | Ready v -> resolve eventId v model
        | Awaiting(refs, executables) ->
            await eventId refs model
    | AddRow name -> addName name model |> resolve eventId Nothing
    | SetProperty (keys, expr) ->
        let setProperty v model key =
            model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key v)
        match eval model expr with
        | Ready v ->
            keys
            |> List.fold (setProperty v) model
            |> progress executeHelper (Set.ofList [EventRef eventId])
            |> resolve eventId Nothing
        | _ -> model
    | ChangeProperty (keys, expr) ->
        let setProperty v model key =
            let changeVal (data:Map<Key,Value>) =
                match data |> Map.tryFind key with
                | Some v0 -> data |> Map.add key (v0 + v)
                | None -> data |> Map.add key v
            model |> addProperty (snd key) |> Lens.over Lens.data changeVal
        let blocked = keys |> List.filter (fun key -> model.data.ContainsKey key |> not) |> List.map PropertyRef
        match blocked with
        | [] ->
            match eval model expr with
            | Ready v ->
                keys
                |> List.fold (setProperty v) model
                |> progress executeHelper (Set.ofList [EventRef eventId])
                |> resolve eventId Nothing
            // todo: if the expression isn't ready yet, how to defer? should have a way to create an eval event and wait for it
        | _ -> await eventId blocked model

let setProperty key (value:Value) model =
    model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key value)
        |> progress executeHelper (Set.ofList [PropertyRef key])

let execute model cmd =
    let model = model |> Lens.over Lens.eventLog (add (Awaiting cmd))
    let eventId = model.eventLog.lastId.Value
    eventId, executeHelper model eventId cmd
