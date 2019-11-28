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

type Event = Evaluation<Executable[]>
type Model = {
        properties: Property list
        roster: SymmetricMap.Data<Id, string>
        data: Map<Key, Value>
        blocking: SymmetricRelation.Data<Key, Reference> // data which needs to be in data to proceed
        blockedThreads: {| eventId: Id; stack: Executable |} list
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
    | Ref (PropertyRef key) -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting (PropertyRef key, expr)
    | Ref (EventRef id) ->
        match model.eventLog |> tryFind id with
        | Some (event:Event) ->
            match event with
            | Awaiting(ref, _) -> Awaiting(ref, expr)
            | Ready v -> Ready v
        | None -> Awaiting (EventRef id, expr)
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
let unblock exec (model: Model) =
    let keys = model.blocking.forward |> Map.filter (fun key _ -> model.data.ContainsKey key)
    // execute any unblocked threads
    let unblock key (model: Model) =
        match model.blocking.forward |> Map.tryFind key with
        | None | Some [] ->
            model
        | Some unblocked ->
            // re-process the unblocked threads from the beginning (TODO: store continuations instead of whole cmds)
            unblocked
                |> List.map (function
                                | EventRef eventId -> eventId, (model.blockedThreads |> List.find (fun t -> t.eventId = eventId)).stack
                                | v -> matchfail v) // don't yet have an implementation for unblocking data references
                |> List.fold exec { model with blocking = SymmetricRelation.removeAllForward key model.blocking }
    keys |> Map.fold (fun model key _ -> unblock key model) model
let private resolve eventId value model =
    model |> Lens.over Lens.eventLog (transform eventId (thunk value))
let await (eventId, cmd) keys model =
    let addKeys data =
        keys |> List.fold (fun data key -> data |> SymmetricRelation.add key (EventRef eventId)) data
    { model with blockedThreads = {| eventId = eventId; stack = cmd; |} :: model.blockedThreads }
                |> Lens.over Lens.blocking addKeys
let rec private executeHelper model (eventId, cmd) =
    match cmd with
    | Evaluate expr ->
        match eval model expr with
        | Ready v -> resolve eventId (Ready v) model
        | Awaiting(key, executables) ->
            await (eventId, cmd) [key] model
    | AddRow name -> addName name model |> resolve eventId (Ready Nothing)
    | SetProperty (keys, expr) ->
        let setProperty v model key =
            model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key v)
        match eval model expr with
        | Ready v ->
            keys
            |> List.fold (setProperty v) model
            |> unblock executeHelper
            |> resolve eventId (Ready Nothing)
        | _ -> model
    | ChangeProperty (keys, expr) ->
        let setProperty v model key =
            let changeVal (data:Map<Key,Value>) =
                match data |> Map.tryFind key with
                | Some v0 -> data |> Map.add key (v0 + v)
                | None -> data |> Map.add key v
            model |> addProperty (snd key) |> Lens.over Lens.data changeVal
        let blocked = keys |> List.filter (fun key -> model.data.ContainsKey key |> not)
        match blocked with
        | [] ->
            match eval model expr with
            | Ready v ->
                keys
                |> List.fold (setProperty v) model
                |> unblock executeHelper
                |> resolve eventId (Ready Nothing)
            // todo: if the expression isn't ready yet, how to defer? should have a way to create an eval event and wait for it
        | _ -> await (eventId, cmd) blocked model

let setProperty key (value:Value) model =
    model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key value)
        |> unblock executeHelper

let execute model cmd =
    let model = model |> Lens.over Lens.eventLog (add { status = Blocked; cmd = cmd })
    let eventId = model.eventLog.lastId.Value
    eventId, executeHelper model (eventId, cmd)
