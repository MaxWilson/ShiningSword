module App
open System
open Elmish
open Elmish.Browser.Navigation
open Model.Types
open Model.Operations
open Common

open Elmish.React
open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
importAll "../../sass/main.sass"

type FastList1<'t> = { rows: Map<int, 't>; lastId: int option }
    with
    member data.add (row: 't) =
        let id = (defaultArg data.lastId 0) + 1
        { data with rows = data.rows |> Map.add id row; lastId = Some id }
    member data.transform id f =
        let row = data.rows.[id]
        { data with rows = data.rows |> Map.add id (f row) }
    member data.replace id row =
        { data with rows = data.rows |> Map.add id row }
    member data.toSeq() =
        seq { for i in 1..(defaultArg data.lastId 0) -> data.rows.[i] }
module FastList1 =
    let inline (|HasRows|) x =
        fun () -> (^a : (member rows: Map<int, 't>) x)
    let inline rows (HasRows f) = f()
    let inline (|HasLastId|) x =
        fun () -> (^a : (member lastId: int option) x)
    let inline lastId (HasLastId f) = f()
    let inline add row (data) =
        let id = (defaultArg (lastId data) 0) + 1
        { data with rows = (rows data) |> Map.add id row; lastId = Some id }
module FastList2 =
    let inline (|HasAdd|) x =
        fun arg -> (^a : (member add: 'b -> 'a) (x,arg))
    let inline add row (HasAdd add) =
        add row


module FastList =
    type Data<'t> = { rows: Map<int, 't>; lastId: int option }
    let fresh = { rows = Map.empty; lastId = None }
    let add (row: 't) (data:Data<'t>) =
        let id = (defaultArg data.lastId 0) + 1
        { data with rows = data.rows |> Map.add id row; lastId = Some id }
    let transform id f data =
        let row = data.rows.[id]
        { data with rows = data.rows |> Map.add id (f row) }
    let replace id row data =
        { data with rows = data.rows |> Map.add id row }
    let toSeq data =
        seq { for i in 1..(defaultArg data.lastId 0) -> data.rows.[i] }

module SymmetricMap =
    type Data<'key, 'v when 'key: comparison and 'v: comparison> = Map<'key, 'v> * Map<'v, 'key>
    let inline empty() = Map.empty, Map.empty
    let find k (d: Data<_,_>) = (fst d) |> Map.find k
    let findValue k (d: Data<_,_>) = (snd d) |> Map.find k
    let tryFind k (d: Data<_,_>) = (fst d) |> Map.tryFind k
    let tryFindValue k (d: Data<_,_>) = (snd d) |> Map.tryFind k
    let add k v (m1, m2) = m1 |> Map.add k v, m2 |> Map.add v k
    let toSeq (d: Data<_,_>) = fst d |> Map.toSeq
    let isEmpty (d: Data<_,_>) = fst d |> Map.isEmpty
    let keys (d: Data<_,_>) = (d |> fst) |> Map.toSeq |> Seq.map fst
    let values (d: Data<_,_>) = (d |> fst) |> Map.toSeq |> Seq.map snd

module SymmetricRelation =
    type Data<'t1, 't2 when 't1: comparison and 't2: comparison> = {
        forward: Map<'t1, 't2 list>
        backward: Map<'t2, 't1 list>
    }
    let empty = { forward = Map.empty; backward = Map.empty }
    let add v1 v2 (d:Data<_,_>) =
        let createOrExtend key v map =
            match map |> Map.tryFind key with
            | Some lst -> map |> Map.add key (v::lst)
            | None -> Map.add key [v] map
        {
            forward = createOrExtend v1 v2 d.forward
            backward = createOrExtend v2 v1 d.backward
        }
    let removeAllForward v1 (d:Data<_,_>) =
        match d.forward |> Map.tryFind v1 with
        | None -> d // nothing to remove
        | Some v2s ->
            let deleteFrom (map:Map<_,_>) key =
                match map.[key] |> List.filter ((<>) v1) with
                | [] -> map |> Map.remove key
                | vs -> map |> Map.add key vs // replace with a new, shortened list
            let backward' = v2s |> List.fold deleteFrom d.backward
            { d with backward = backward'; forward = d.forward |> Map.remove v1 }
    let removeAllBackward v2 (d:Data<_,_>) =
        match d.backward |> Map.tryFind v2 with
        | None -> d // nothing to remove
        | Some v1s ->
            let deleteFrom (map:Map<_,_>) key =
                match map.[key] |> List.filter ((<>) v2) with
                | [] -> map |> Map.remove key
                | vs -> map |> Map.add key vs // replace with a new, shortened list
            let forward' = v1s |> List.fold deleteFrom d.forward
            { d with forward = forward'; backward = d.backward |> Map.remove v2 }

module Domain =
    type Id = int
    type PropertyName = string
    type Key = Id * PropertyName
    type Op = Plus | Minus
    type Expression =
        | Number of int
        | Ref of Key
        | BinaryOperation of Expression * Op * Expression
    type Cmd =
        | Eval of originalText:string * ast:Expression
        | AddRow of name:string
        | SetData of Key * Expression
    type Property = { name: string }
    type Evaluation = Ready of int | Awaiting of Key
    type Reference = EventRef of Id | PropertyRef of Key
    type EventStatus = Blocked | Resolved of output: string option
    type Event = { status: EventStatus; cmd: Cmd; cmdText: string }
    type Model = {
            properties: Property list
            roster: SymmetricMap.Data<Id, string>
            data: Map<Key, int>
            blocking: SymmetricRelation.Data<Key, Reference> // data which needs to be in data to proceed
            blockedThreads: {| eventId: Id; stack: Cmd |} list
            eventLog: FastList.Data<Event>
        }
    let fresh = { properties = []; roster = SymmetricMap.empty(); data = Map.empty; blocking = SymmetricRelation.empty; blockedThreads = []; eventLog = FastList.fresh }
    module Lens =
        let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
        let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
        let blocking = Lens.lens (fun d -> d.blocking) (fun v d -> { d with blocking = v})
        let blockedThreads = Lens.lens (fun d -> d.blockedThreads) (fun v d -> { d with blockedThreads = v})
        let eventLog = Lens.lens (fun d -> d.eventLog) (fun v d -> { d with eventLog = v})

    let rec tryParseExpression model (cmd: string) =
        match Int32.TryParse cmd with
        | true, v -> Number v |> Some
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
                        Ref(id, prop.Trim()) |> Some
                    | _ -> None
                | _ -> Ref(0, cmd) |> Some
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
                            SetData((id, prop.Trim()), expr) |> Some
                        | _ -> None
                    | _ -> SetData((0, cmd), expr) |> Some
                | _ -> None
            | _ -> None
        else
            match tryParseExpression model cmd with
            | Some e -> Some (Eval (cmd, e))
            | _ -> None
    let rec eval model = function
        | Number v -> Ready v
        | Ref key -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting key
        | BinaryOperation(lhs, op, rhs) ->
            match eval model lhs, eval model rhs with
            | Awaiting key, _ | _, Awaiting key -> Awaiting key
            | Ready l, Ready r ->
                Ready(if op = Plus then l + r else l - r)
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
        let model = model |> Lens.over Lens.eventLog (FastList.add { status = Blocked; cmd = cmd; cmdText = cmdText })
        let eventId = model.eventLog.lastId.Value
        let resolve eventId msg model =
            { model with eventLog = model.eventLog |> FastList.transform eventId (fun e -> { e with status = Resolved msg }) }
        let rec help model (eventId, cmd) =
            match cmd with
            | Eval (txt, expr) as cmd ->
                match eval model expr with
                | Ready v -> resolve eventId (v.ToString() |> Some) model
                | Awaiting key ->
                    { model with blockedThreads = {| eventId = eventId; stack = cmd; |} :: model.blockedThreads }
                        |> Lens.over Lens.blocking (SymmetricRelation.add key (EventRef eventId))
            | AddRow name -> addName name model |> resolve eventId None
            | SetData (key, expr) ->
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
                match eval model expr with
                | Ready v ->
                    model |> addProperty (snd key) |> Lens.over Lens.data (Map.add key v) |> unblock key |> resolve eventId None
                | _ -> model
        eventId, help model (eventId, cmd)

module View =
    type Id = int
    type ConsoleLog = Resolved of string | Blocked of Id
    type Model = { currentInput: string; console: ConsoleLog list; domainModel: Domain.Model }
    module Lens =
        let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
    type Cmd = NewValue of string | ENTER | RESET | Error of string
    let summaryOf (m:Domain.Model) =
        let data = m.data
        [
            yield tr[][
                yield th[][str "Name"]
                for p in m.properties do yield th[][str p.name]
            ]
            for (id, name) in m.roster |> SymmetricMap.toSeq do
                yield tr[][
                    yield td[][str name]
                    for p in m.properties ->
                        match data |> Map.tryFind (id, p.name) with
                        | Some v -> td[][str (v.ToString())]
                        | None -> td[][str "???"]
                ]
            ]
    let view m dispatch =
        let log = [
            for e in m.console ->
                match e with
                | Resolved s -> li[ClassName "resolved"][str s]
                | Blocked id ->
                    let d = m.domainModel
                    let event = d.eventLog.rows.[id]
                    let dependencies = d.blocking.backward.[Domain.EventRef id] |> List.map (fun (id, prop) -> if id > 0 then (d.roster |> SymmetricMap.find id) + "." + prop else prop)
                                        |> String.join ", "
                    li[ClassName "blocked"][str <| sprintf "%s (needs %s)" event.cmdText dependencies]
            ]

        div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
            yield p[ClassName "summaryPane"][
                    table [] (summaryOf m.domainModel)
                ]
            yield p[ClassName "queryPane"] [
                h2[][str "Enter a command:"]
                br[]
                form [OnSubmit (fun e -> dispatch ENTER; e.preventDefault())] [
                        input [OnChange (fun e -> e.Value |> NewValue |> dispatch); HTMLAttr.Value m.currentInput; HTMLAttr.AutoFocus true]
                        button[Type "submit"][str "OK"]
                    ]
                br[]
                ]
            if log <> [] then
                yield div[ClassName "sidebar"][
                    ul[] log
                    button[OnClick (fun _ -> dispatch RESET)][str "Clear"]
                ]
        ]

    let init _ = { currentInput = ""; console = []; domainModel = Domain.fresh }, Cmd.Empty
    let update msg model =
        try
            match msg with
            | NewValue s -> { model with currentInput = s; }, Cmd.Empty
            | ENTER ->
                match Domain.tryParseCommand model.domainModel model.currentInput with
                | Some expr ->
                    let eventId, domain' = Domain.execute model.domainModel model.currentInput expr
                    let logEntry eventId =
                        let event = domain'.eventLog.rows.[eventId]
                        match event.status with
                        | Domain.Blocked -> Blocked eventId
                        | Domain.Resolved None -> Resolved (event.cmdText)
                        | Domain.Resolved (Some output) -> sprintf "%s: %s" event.cmdText output |> Resolved
                    let resolve = function Blocked id -> logEntry id | v -> v
                    { model with currentInput = ""; domainModel = domain'; console = (model.console@[logEntry eventId] |> List.map resolve) }, Cmd.Empty
                | _ -> model, Cmd.Empty
            | RESET -> init()
            | Error err ->
                { model with console = model.console@[Resolved err] }, Cmd.Empty
        with err ->
            { model with console = model.console@[Resolved err.Message] }, Cmd.Empty


// App
Program.mkProgram View.init View.update View.view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun d ->
    Fable.Import.Browser.window.onerror <-
    fun msg _src _lineNo _colNo err ->
        if msg.Contains "SocketProtocolError" = false then
            d (View.Error (sprintf "Error: %A" msg))
        ))
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
