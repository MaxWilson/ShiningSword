module UI.Ribbit

open System
open Elmish
open Common

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.React
open Fable.React.Props
open Data
open Data.Functor
open Domain
open Domain.Properties
open Domain.Commands

type Id = int
type ConsoleLog = Resolved of string | Blocked of Id
type Model = { currentInput: string; console: ConsoleLog list; domainModel: Domain.Model }
module Lens =
    let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
type Cmd = NewValue of string | ENTER | RESET | Error of string | Fulfill of Key * value:string
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
                let dependencies = d.blocking.backward.[EventRef id] |> List.map (fun (id, prop) -> if id > 0 then (d.roster |> SymmetricMap.find id) + "." + prop else prop)
                                    |> String.join ", "
                li[ClassName "blocked"][str <| sprintf "%s (needs %s)" event.cmdText dependencies]
        ]

    div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
        yield p[ClassName "summaryPane"][
                table [] (summaryOf m.domainModel)
            ]
        yield p[ClassName "queryPane"] [
            match m.domainModel.blocking.forward |> List.ofSeq with
            | [] -> ()
            | blocked -> div[][
                for KeyValue((id, prop) as key, _) in blocked do
                    yield View.ViewComponents.localForm
                            (sprintf "Enter value for %s's %s" (m.domainModel.roster |> SymmetricMap.find id) prop)
                            [ClassName "bordered"]
                            (fun v -> dispatch (Fulfill ((key, v))))
                ]
            h2[][str "Enter a command please:"]
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
    let exec txt cmd =
        let eventId, domain' = Domain.execute model.domainModel txt cmd
        let logEntry eventId =
            let event = domain'.eventLog.rows.[eventId]
            match event.status with
            | Domain.Blocked -> Blocked eventId
            | Domain.Resolved None -> Resolved (event.cmdText)
            | Domain.Resolved (Some output) -> sprintf "%s: %s" event.cmdText output |> Resolved
        let resolve = function Blocked id -> logEntry id | v -> v
        { model with currentInput = ""; domainModel = domain'; console = (model.console@[logEntry eventId] |> List.map resolve) }, Cmd.Empty

    try
        match msg with
        | NewValue s -> { model with currentInput = s; }, Cmd.Empty
        | ENTER ->
            match Domain.tryParseCommand model.domainModel model.currentInput with
            | Some cmd -> exec model.currentInput cmd
            | _ -> model, Cmd.Empty
        | RESET -> init()
        | Error err ->
            { model with console = model.console@[Resolved err] }, Cmd.Empty
        | Fulfill(key, v) ->
            exec "Set data" (SetProperty([key], System.Int32.Parse v |> Number |> Literal))
    with err ->
        { model with console = model.console@[Resolved err.Message] }, Cmd.Empty

