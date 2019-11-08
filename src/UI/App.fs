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
    let find k d = (fst d) |> Map.find k
    let findValue k d = (snd d) |> Map.find k
    let tryFind k d = (fst d) |> Map.tryFind k
    let tryFindValue k d = (snd d) |> Map.tryFind k
    let add k v (m1, m2) = m1 |> Map.add k v, m2 |> Map.add v k
    let toSeq d = fst d |> Map.toSeq
    let isEmpty x = fst x |> Map.isEmpty

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
    type Dependency = | WaitingForEvent of Id | WaitingForProperty of Key
    type EventStatus = Blocked | Resolved of output: string option
    type Event = { status: EventStatus; cmd: Cmd; cmdText: string }
    type ThreadBlockage = { eventId: Id; awaiting: Dependency; stack: Cmd } // this is probably not the right name for the second half of the awaiting model. OpenRequest should be the thing it's waiting for
    type Model = {
            properties: Property list
            roster: SymmetricMap.Data<Id, string>
            data: Map<Key, int>
            blockedThreads: ThreadBlockage list // data which needs to be in data to proceed
            eventLog: FastList.Data<Event>
        }
    let fresh = { properties = []; roster = SymmetricMap.empty(); data = Map.empty; blockedThreads = []; eventLog = FastList.fresh }
    module Lens =
        let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
        let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
        let blockedThreads = Lens.lens (fun d -> d.blockedThreads) (fun v d -> { d with blockedThreads = v})
        let eventLog = Lens.lens (fun d -> d.eventLog) (fun v d -> { d with eventLog = v})

    let rec tryParseExpression model (cmd: string) =
        match Int32.TryParse cmd with
        | true, v -> Number v |> Some
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
                    model |> Lens.over Lens.blockedThreads (fun l -> l @ [{ eventId = eventId; stack = cmd; awaiting = Dependency.WaitingForProperty key }])
            | AddRow name -> addName name model |> resolve eventId None
            | SetData (key, expr) ->
                // execute any unblocked threads
                let unblock key (model: Model) =
                    let unblocked, stillBlocked = model.blockedThreads |> List.partition (function { awaiting = Dependency.WaitingForProperty k } when k = key -> true | _ -> false)
                    if unblocked = [] then
                        model
                    else
                        // re-process the unblocked threads from the beginning (TODO: store continuations instead of whole cmds)
                        unblocked
                            |> List.map (fun x -> x.eventId, x.stack)
                            |> List.fold help { model with blockedThreads = stillBlocked }
                match eval model expr with
                | Ready v ->
                    model |> Lens.over Lens.data (Map.add key v) |> unblock key |> resolve eventId None
                | _ -> model
        eventId, help model (eventId, cmd)

module View =
    type Id = int
    type ConsoleLog = Resolved of string | Blocked of Id
    type Model = { currentInput: string; console: ConsoleLog list; domainModel: Domain.Model }
    module Lens =
        let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
    type Cmd = NewValue of string | ENTER | RESET | Error of string
    let view m dispatch =
        let log = [
            for e in m.console ->
                match e with
                | Resolved s -> li[ClassName "resolved"][str s]
                | Blocked id -> li[ClassName "blocked"][str (m.domainModel.eventLog.rows.[id].cmdText)]
            ]

        div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
            yield p[ClassName "summaryPane"][str ""]
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
