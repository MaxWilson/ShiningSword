[<AutoOpen>]
module Domain.Domain
open System
open Common
open Data
open Data.Functor
open Domain.Prelude
open Domain.Properties
open Domain.Dice
open Domain.Commands

type EventStatus = Blocked | Resolved of output: string option
type Event = { status: EventStatus; cmd: Command; cmdText: string }
type Model = {
        properties: Property list
        roster: SymmetricMap.Data<Id, string>
        data: Map<Key, int>
        blocking: SymmetricRelation.Data<Key, Reference> // data which needs to be in data to proceed
        blockedThreads: {| eventId: Id; stack: Command |} list
        eventLog: FastList<Event>
    }
let fresh = { properties = []; roster = SymmetricMap.empty(); data = Map.empty; blocking = SymmetricRelation.empty; blockedThreads = []; eventLog = FastList.fresh() }
module Lens =
    let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
    let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
    let blocking = Lens.lens (fun d -> d.blocking) (fun v d -> { d with blocking = v})
    let blockedThreads = Lens.lens (fun d -> d.blockedThreads) (fun v d -> { d with blockedThreads = v})
    let eventLog = Lens.lens (fun d -> d.eventLog) (fun v d -> { d with eventLog = v})

let rec tryParseExpression model (cmd: string) =
    match Int32.TryParse cmd with
    | true, v -> Number v |> Literal |> Some
    | _ when String.IsNullOrWhiteSpace cmd -> None
    | _ ->
        // REALLY crude parsing because that's not the point right now
        if cmd.Contains "+" || cmd.Contains "-" then
            let op = if (cmd.Contains "+") then Plus else Minus
            let cmds = cmd.Split('+', '-') |> Array.map (tryParseExpression model >> FSharp.Core.Option.get)
            match cmds with
            | [|lhs; rhs|] -> BinaryOperation(lhs, op, rhs) |> Some
            | _ -> None
        else
            match cmd.Split('.') with
            | [|name;prop|] ->
                match model.roster |> SymmetricMap.tryFindValue name with
                | Some id ->
                    Ref(PropertyRef (id, prop.Trim())) |> Some
                | _ -> None
            | _ -> Ref(PropertyRef(0, cmd)) |> Some
let rec tryParseCommand model (cmd: string) =
    if cmd.StartsWith ("add ") then
        Some (AddRow <| cmd.Replace("add ", "").Trim())
    elif cmd.Contains("=") then
        match cmd.Split('=') with
        | [|lhs; value|] ->
            match tryParseExpression model value with
            | Some expr ->
                match lhs.Split('.') with
                | [|name;prop|] ->
                    match model.roster |> SymmetricMap.tryFindValue name with
                    | Some id ->
                        SetProperty([id, prop.Trim()], expr) |> Some
                    | _ -> None
                | _ -> SetProperty([0, cmd.Trim()], expr) |> Some
            | _ -> None
        | _ -> None
    else
        match tryParseExpression model cmd with
        | Some e -> Some (Evaluate e)
        | _ -> None
let rec eval model = function
    | Literal (Number v) -> Ready v
    | Ref (PropertyRef key) -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting key
    | BinaryOperation(lhs, op, rhs) ->
        match eval model lhs, eval model rhs with
        | Awaiting key, _ | _, Awaiting key -> Awaiting key
        | Ready l, Ready r ->
            Ready(if op = Plus then l + r else l - r)
    | BestN(n, exprs) ->
        match exprs |> List.tryMapFold(fun vs e -> match eval model e with Ready v -> Ok(v::vs) | Awaiting k -> Error k) [] with
        | Ok vs -> Ready (vs |> List.sortDescending |> List.take (min vs.Length n) |> List.sum)
        | Error k -> Awaiting k

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
let execute model cmdText cmd =
    let model = model |> Lens.over Lens.eventLog (add { status = Blocked; cmd = cmd; cmdText = cmdText })
    let eventId = model.eventLog.lastId.Value
    let resolve eventId msg model =
        model |> Lens.over Lens.eventLog (transform eventId (fun e -> { e with status = Resolved msg }))
    let rec help model (eventId, cmd) =
        match cmd with
        | Evaluate expr as cmd ->
            match eval model expr with
            | Ready v -> resolve eventId (v.ToString() |> Some) model
            | Awaiting key ->
                { model with blockedThreads = {| eventId = eventId; stack = cmd; |} :: model.blockedThreads }
                    |> Lens.over Lens.blocking (SymmetricRelation.add key (EventRef eventId))
        | AddRow name -> addName name model |> resolve eventId None
        | SetProperty (keys, expr) ->
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
                        |> List.fold help { model with blocking = SymmetricRelation.removeAllForward key model.blocking }
            let setProperty v model key =
                model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key v)
            match eval model expr with
            | Ready v ->
                    let model = keys |> List.fold (setProperty v) model
                    keys
                        |> List.fold (fun model key -> unblock key model) model
                        |> resolve eventId None
            | _ -> model
    eventId, help model (eventId, cmd)
