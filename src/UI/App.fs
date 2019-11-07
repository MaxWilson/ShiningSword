module App
open System
open Elmish
open Elmish.Browser.Navigation
open Model.Types
open Model.Operations
open Common
open Fable.Import.Browser

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
    type Data<'t> = { rows: Map<int, 't>; nextId: int }
    let fresh = { rows = Map.empty; nextId = 1 }
    module Lens =
        let rows = Lens.lens (fun d -> d.rows) (fun v d -> { d with rows = v})
        let nextId = Lens.lens (fun d -> d.nextId) (fun v d -> { d with nextId = v})
    let add row data =
        let id = data.nextId
        data |> Lens.over Lens.rows (Map.add id row) |> Lens.over Lens.nextId ((+) 1)
    let toSeq data =
        seq { for i in 1..(data.nextId-1) -> data.rows.[i] }

module SymmetricMap =
    type Data<'key, 'v when 'key: comparison and 'v: comparison> = Map<'key, 'v> * Map<'v, 'key>
    let empty = Map.empty, Map.empty
    let find k d = (fst d) |> Map.find k
    let findValue k d = (snd d) |> Map.find k
    let tryFind k d = (fst d) |> Map.tryFind k
    let tryFindValue k d = (snd d) |> Map.tryFind k
    let add k v (m1, m2) = m1 |> Map.add k v, m2 |> Map.add v k
    let toSeq d = fst d |> Map.toSeq

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
    type Model = {
            properties: Property list
            roster: SymmetricMap.Data<Id, string>
            data: Map<Key, int>
            openRequests: (Key * Cmd) list // data which needs to be in data to proceed
            eventLog: FastList.Data<{| message: string |}>
        }
    let fresh = { properties = []; roster = SymmetricMap.empty; data = Map.empty; openRequests = []; eventLog = FastList.fresh }
    module Lens =
        let data = Lens.lens (fun d -> d.data) (fun v d -> { d with data = v})
        let creatureIds = Lens.lens (fun d -> d.roster) (fun v d -> { d with roster = v})
        let openRequests = Lens.lens (fun d -> d.openRequests) (fun v d -> { d with openRequests = v})

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
                        Ref(id, prop) |> Some
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
                            SetData((id, prop), expr) |> Some
                        | _ -> None
                    | _ -> SetData((0, cmd), expr) |> Some
                | _ -> None
            | _ -> None
        else
            match tryParseExpression model cmd with
            | Some e -> Some (Eval (cmd, e))
            | _ -> None
    type Evaluation = Ready of int | Awaiting of Key
    let rec eval model = function
        | Number v -> Ready v
        | Ref key -> match model.data.TryFind key with Some v -> Ready v | None -> Awaiting key
        | BinaryOperation(lhs, op, rhs) ->
            match eval model lhs, eval model rhs with
            | Awaiting key, _ | _, Awaiting key -> Awaiting key
            | Ready l, Ready r ->
                Ready(if op = Plus then l + r else l - r)
    let rec execute model = function
        | Eval (txt, expr) as cmd ->
            match eval model expr with
            | Ready v ->
                { model with eventLog = model.eventLog |> FastList.add ({| message = sprintf "%s: %d" txt v|}) }
            | Awaiting key ->
                model |> Lens.over Lens.openRequests (fun l -> l @ [key, cmd])
        | AddRow name ->
            let id = 1 + (model.roster |> SymmetricMap.toSeq |> Seq.map fst |> Seq.maxOrDefault 0)
            model |> Lens.over Lens.creatureIds (SymmetricMap.add id name)
        | SetData (key, expr) ->
            // execute any unblocked requests and add them to the event log when ready
            let unblock key (model: Model) =
                let unblocked = model.openRequests |> List.filter (fun (key', cmd) -> key' = key) |> List.map snd
                if unblocked = [] then
                    model
                else
                    unblocked |> List.fold execute model
            match eval model expr with
            | Ready v ->
                model |> Lens.over Lens.data (Map.add key v) |> unblock key
            | _ -> model

module View =
    type ConsoleLog = Resolved of string | Blocked of string
    type Model = { currentInput: string; console: ConsoleLog list; domainModel: Domain.Model }
    module Lens =
        let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
    type Cmd = NewValue of string | ENTER | RESET | Error of string
    let view m dispatch =
        let log = [
            for e in m.console ->
                match e with
                | Resolved s -> li[ClassName "resolved"][str s]
                | Blocked s -> li[ClassName "blocked"][str s]
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

    let init _ = { currentInput = ""; console = []; domainModel = Domain.fresh }, Cmd.ofMsg (Error "Testing err...")
    let update msg model =
        try
            match msg with
            | NewValue s -> { model with currentInput = s; }, Cmd.Empty
            | ENTER ->
                match Domain.tryParseCommand model.domainModel model.currentInput with
                | Some expr ->
                    { model with currentInput = "" } |> Lens.over Lens.domainModel (flip Domain.execute expr), Cmd.Empty
                | _ -> model, Cmd.Empty
            | RESET -> init()
            | Error err ->
                { model with console = model.console@[Resolved err] }, Cmd.Empty
        with err ->
            { model with console = model.console@[Resolved err.Message] }, Cmd.Empty


// App
Program.mkProgram View.init View.update View.view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun d ->
    window.onerror <-
    fun msg _src _lineNo _colNo err ->
        if msg.Contains "SocketProtocolError" = false then
            d (View.Error (sprintf "Error: %A" msg))
        ))
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
