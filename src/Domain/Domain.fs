[<AutoOpen>]
module Domain.Domain
open System
open Common
open Data
open Data.Functor
open Packrat
open Domain.Properties
open Domain.Dice
open Domain.Commands

type EventId = Id
/// causedBy = parent, causes = children
type Event = {
    status: Evaluation<Executable>;
    originalExecutable: Executable;
    causedBy: EventId option;
    causes: EventId list
    }
    with static member Status (e:Event) = e.status
let (|AwaitingRoll|_|) = function
    | Awaiting(Evaluate(Roll d) | SetProperty(_, Roll d) | ChangeProperty(_, Roll d)) -> Some d
    | _ -> None

type Model = {
        properties: Property list
        roster: SymmetricMap<Id, string>
        data: Map<Key, Value>
        blocking: SymmetricRelation.Data<Reference, Reference> // data needed --> data waiting.
        blockedThreads: EventId list
        eventLog: FastList<Event>
    }

/// model that is not at a fixed point
type ModelIntermediateState = {
    model: Model
    processingQueue: Queue.Data<Reference>
}
    with
    static member create(model) = { model = model; processingQueue = Queue.create }
    static member create(model, refs) = { model = model; processingQueue = Queue.ofList refs }
    static member create ref = fun model -> { model = model; processingQueue = Queue.create |> Queue.add ref }
    static member extract = function
        | { model = model; processingQueue = q } when Queue.isEmpty q ->
            model
        | m -> failwithf "Tried to extract model from a non-fixedpoint: %A" m
    member this.isFixedPoint = this.processingQueue |> Queue.isEmpty
    member this.await(ref) = { this with processingQueue = this.processingQueue |> Queue.add ref }
    member this.map(f) = { this with model = f this.model }

let fresh = {
        properties = []
        roster = SymmetricMap.empty()
        data = Map.empty; blocking = SymmetricRelation.empty
        blockedThreads = []
        eventLog = FastList.fresh()
        }
module Lens =
    let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
    let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
    let blocking = Lens.lens (fun d -> d.blocking) (fun v d -> { d with blocking = v})
    let blockedThreads = Lens.lens (fun d -> d.blockedThreads) (fun v d -> { d with blockedThreads = v})
    let eventLog = Lens.lens (fun d -> d.eventLog) (fun v d -> { d with eventLog = v})

let addEvent e model =
    let m = model |> Lens.over Lens.eventLog (add e)
    m.eventLog.lastId.Value, m

let blockOn eventId refs model =
    let addKeys eventId blocking =
        refs
        |> List.fold (fun data key -> data |> SymmetricRelation.add key (EventRef eventId)) blocking
    { model with blockedThreads = eventId :: model.blockedThreads }
            |> Lens.over Lens.blocking (addKeys eventId)

let addBlockingEvent parentId e model =
    let eventId, model =
        model |> addEvent ({ status = Awaiting e; originalExecutable = e; causedBy = Some parentId; causes = []})
    let model = { model with eventLog = model.eventLog |> transform parentId (fun e -> { e with causes = eventId::e.causes }) }
    eventId, model |> blockOn eventId [EventRef eventId]

let adaptorOf (model: Model) =
    let startsWith txt prefix =
        (txt:string).StartsWith prefix
    {
    isValidNamePrefix = fun prefix -> model.roster |> Data.SymmetricMap.values |> Seq.exists (fun n -> startsWith n prefix)
    tryNamePrefix = fun prefix ->
        model.roster |> Data.SymmetricMap.toSeq |> Seq.choose (fun (id, name) -> if startsWith name prefix then Some id else None) |> List.ofSeq
    tryId = flip tryFind model.roster
    tryName = flip tryFindValue model.roster
    }

let tryParseExecutable (model: Model) txt =
    match ParseArgs.Init(txt, adaptorOf model) with
    | Domain.Commands.Parse.Executable(cmd, End) ->
        Some cmd
    | v -> None

let tryParseCommand (model: Model) txt =
    match ParseArgs.Init(txt, adaptorOf model) with
    | Domain.Commands.Parse.ConsoleCommand(cmd, End) ->
        Some cmd
    | v -> None

let eval eventId model expression =
    let rec eval model = function
        | Literal v -> Ready v
        | Ref (PropertyRef key) as expr -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting ([PropertyRef key], (expr, model))
        | Ref (EventRef id) as expr ->
            match model.eventLog |> tryFind id |> Option.map Event.Status with
            | Some (Ready v) -> Ready v
            | Some(Awaiting _executable) ->
                Awaiting(model.blocking.backward |> Map.find (EventRef id), (expr, model))
            | None -> Awaiting ([EventRef id], (expr, model)) // this branch might never happen
        | BinaryOperation(lhs, op, rhs) ->
            match eval model lhs with
            | Ready l ->
                match eval model rhs with
                | Ready r ->
                    Ready(if op = Plus then l + r else l - r)
                | Awaiting(key, (rhsExpr, model)) as awaiting -> Awaiting(key, (BinaryOperation(lhs, op, rhsExpr), model))
            | Awaiting (key, (lhsExpr, model)) ->
                Awaiting(key, (BinaryOperation(lhsExpr, op, rhs), model))
        | BestN(n, exprs) ->
            match exprs |> List.tryMapFold(fun vs e -> match eval model e with Ready v -> Ok(v::vs) | Awaiting await -> Error await) [] with
            | Ok vs -> Ready (vs |> List.sortDescending |> List.take (min vs.Length n) |> List.sum)
            | Error (key, (expr, model)) -> Awaiting (key, (expr, model))
        | Negate e -> match eval model e with
                        | Ready v ->
                            match v with
                            | Number n -> Number (-n)
                            | Err _ -> v
                            | _ -> Err (sprintf "Cannot negate '%A'" v)
                            |> Ready
                        | Awaiting(key, (expr, model)) -> Awaiting(key, (Negate expr, model))
        | Roll _ as expr ->
            let parentId = eventId
            // rewrite the expression to be awaiting a child event which is the actual roll
            let childEventId, model = model |> addBlockingEvent parentId (Evaluate expr)
            Awaiting([EventRef childEventId], (Ref(EventRef childEventId), model))
    eval model expression

let addName name model =
    if (model: Model).roster |> tryFindValue name |> Option.isSome then // idempotence
        model
    else
        let id = 1 + (if model.roster |> SymmetricMap.isEmpty then 0 else model.roster |> SymmetricMap.toSeq |> Seq.map fst |> Seq.max)
        model |> Lens.over Lens.creatureIds (add (id, name))

// define a property
let addProperty propertyName model =
    if (model:Model).properties |> List.exists (fun p -> p.name = propertyName) then
        model
    else
        { model with properties = model.properties @ [{name = propertyName}] }

let unblock ref (model': ModelIntermediateState) =
    let progressing =
        match model'.model.blocking.forward |> Map.tryFind ref with Some refs -> refs | None -> []
        |> Seq.distinct
        |> Array.ofSeq
    let blocking' = SymmetricRelation.removeAllForward ref model'.model.blocking
    // might still be blocked by other dependencies
    let unblocked = progressing |> Array.filter(fun ref -> blocking'.backward.ContainsKey ref |> not)
    let q = unblocked |> Array.fold (fun state ref -> Queue.add ref state) model'.processingQueue
    { model' with model = { model'.model with blocking = blocking' }; processingQueue = q }

let private resolve eventId value (model': ModelIntermediateState) =
    model'.map(fun model ->
        model |> Lens.over Lens.eventLog (transform eventId (fun e -> { e with status = (Ready value) }))
        )
    |> unblock (EventRef eventId)
let private await (eventId, executable) refs model =
    model
    |> Lens.over Lens.eventLog (transform eventId (fun e -> { e with status = (Awaiting executable) }))
    |> blockOn eventId refs

let private executeHelper (model':ModelIntermediateState) eventId executable : ModelIntermediateState =
    let setProperty v (model': ModelIntermediateState) key =
        model'.map(fun model ->
            model
            |> addProperty (snd key)
            |> Lens.over Lens.data (Map.add key v))
        |> unblock (PropertyRef key)
    match executable with
    | Evaluate expr ->
        match eval eventId model'.model expr with
        | Ready v -> resolve eventId v model'
        | Awaiting(refs, (expr, model)) ->
            { model' with model = await (eventId, Evaluate expr) refs model }
    | AddRow name -> model'.map(fun model -> addName name model) |> resolve eventId Nothing
    | SetProperty (keys, expr) ->
        match eval eventId model'.model expr with
        | Ready v ->
            keys |> List.fold (setProperty v) model' |> resolve eventId Nothing
        | Awaiting(refs, (expr, model)) ->
            { model' with model = await (eventId, SetProperty(keys, expr)) refs model }
    | ChangeProperty (keys, expr) ->
        let blocked = keys |> List.filter (fun key -> model'.model.data.ContainsKey key |> not) |> List.map PropertyRef
        match blocked with
        | [] ->
            match eval eventId model'.model expr with
            | Ready v ->
                let model = keys |> List.fold (setProperty v) model'

                model
                |> resolve eventId Nothing
            | Awaiting(refs, (expr, model)) ->
                { model' with model = await (eventId, ChangeProperty(keys, expr)) refs model }
        | _ -> { model' with model = await (eventId, executable) blocked model'.model }

let progress (model:ModelIntermediateState): ModelIntermediateState =
    // interacts with: model.blocking, model.eventLog, model.data (properties)
    let iterate (completed: List<Reference>) (model':ModelIntermediateState): ModelIntermediateState =
        // look up all the events which are directly or undirectly unblocked by this reference and progress them
        let model = model'.model
        let progressing = completed |> Seq.collect (fun ref -> match model.blocking.forward |> Map.tryFind ref with Some refs -> refs | None -> [])
                                    |> Seq.distinct
                                    |> Array.ofSeq
        let blocking' = completed |> Seq.fold (fun blocking ref -> blocking |> SymmetricRelation.removeAllForward ref) model.blocking
        // might still be blocked by other dependencies
        let unblocked = progressing |> Array.filter(fun ref -> blocking'.backward.ContainsKey ref |> not)
        let model = { model with blocking = blocking' }
        let model' = { model' with model = model }
        let reallyExecute (input: ModelIntermediateState) ref : ModelIntermediateState =
            let model = input.model
            let completed = input.processingQueue |> Queue.eject
            match ref with
            | PropertyRef(key) ->
                match model.data.TryFind key with
                | Some v ->
                    // this property is "complete" now in this model, signal for further chaining
                    input.await(ref)
                | None -> input // this property is not completed yet
            | EventRef(eventId) ->
                match model.eventLog.rows.[eventId].status with
                | Ready v -> input // shouldn't happen probably
                | Awaiting(executable) ->
                    let model': ModelIntermediateState = executeHelper input eventId executable
                    match model'.model.eventLog |> find eventId |> Event.Status with
                    | Ready v ->
                        model'.await(ref)
                    | Awaiting(executions) ->
                        model'
        unblocked |> Array.fold reallyExecute model'
    match model.processingQueue |> Queue.eject with
    | [] -> model
    | queue ->
        iterate queue model


let setProperty key (value:Value) model =
    model
    |> addProperty (snd key) |> Lens.over Lens.data (Map.add key value)
    |> ModelIntermediateState.create (PropertyRef key)

let fulfillRoll eventId n model =
    model
    |> Lens.over Lens.eventLog (transform eventId (fun e -> { e with status = Ready (Number n) }))
    |> ModelIntermediateState.create (EventRef eventId)

let rec progressToFixedPoint = function
    | (m: ModelIntermediateState) when m.isFixedPoint -> m.model
    | m ->
        m |> progress |> progressToFixedPoint

let execute model cmd =
    let eventId, model = model |> addEvent { status = Awaiting cmd; originalExecutable = cmd; causedBy = None; causes = [] }
    eventId, (executeHelper (model |> ModelIntermediateState.create) eventId cmd) |> progressToFixedPoint
