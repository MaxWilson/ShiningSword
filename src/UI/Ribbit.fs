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
type Model = { console: ConsoleLog list; domainModel: Domain.Model }
module Lens =
    let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
type Cmd = ENTER of string | RESET | Error of string | Fulfill of Key * value:string
let summaryOf (m:Domain.Model) =
    let data = m.data
    [
        thead[][
            tr[][
                yield th[][str "Name"]
                for p in m.properties do yield th[][str p.name]
            ]
        ]
        tbody[][
            for (id, name) in m.roster |> SymmetricMap.toSeq do
                yield tr[][
                    yield td[][str name]
                    for p in m.properties ->
                        match data |> Map.tryFind (id, p.name) with
                        | Some v -> td[][str (v.ToString())]
                        | None -> td[][str "???"]
                ]
        ]
    ]
let view m dispatch =
    try
        let log = [
            for e in m.console ->
                match e with
                | Resolved s -> li[ClassName "resolved"][str s]
                | Blocked id ->
                    try
                        let d = m.domainModel
                        let event = d.eventLog.rows.[id]
                        try
                            let dependencies = d.blocking.backward.[EventRef id] |> List.map (fun (id, prop) -> if id > 0 then (d.roster |> SymmetricMap.find id) + "." + prop else prop)
                                                |> String.join ", "
                            li[ClassName "blocked"][str <| sprintf "%s (needs %s)" event.cmdText dependencies]
                        with _ -> li[ClassName "blocked"][str <| sprintf "Couldn't render %s" event.cmdText]
                    with _ -> li[ClassName "blocked"][str <| sprintf "Couldn't render #%d" id]
            ]

        div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
            yield div[ClassName "summaryPane"][
                    table [] (summaryOf m.domainModel)
                ]
            yield div[ClassName "queryPane"] [
                View.ViewComponents.localForm "Enter a command please:" [AutoFocus true] (ENTER >> dispatch)
                br[]
                match m.domainModel.blocking.forward |> List.ofSeq with
                | [] -> ()
                | blocked -> div[][
                    for KeyValue((id, prop) as key, _) in blocked do
                        yield View.ViewComponents.localForm
                                (sprintf "Enter value for %s's %s" (m.domainModel.roster |> SymmetricMap.find id) prop)
                                [ClassName "bordered"]
                                (fun v -> dispatch (Fulfill ((key, v))))
                    ]
                br[]
                ]
            if log <> [] then
                yield div[ClassName "sidebar"][
                    ul[] log
                    button[OnClick (fun _ -> dispatch RESET)][str "Clear"]
                ]
        ]
    with e ->
        div[][str (e.ToString())]

let init _ = { console = []; domainModel = Domain.fresh }, Cmd.Empty
let update msg model =
    let logEntry domain eventId =
        let event = domain.eventLog.rows.[eventId]
        match event.status with
        | Domain.Blocked -> Blocked eventId
        | Domain.Resolved None -> Resolved (event.cmdText)
        | Domain.Resolved (Some output) -> sprintf "%s: %s" event.cmdText (output.ToString()) |> Resolved
    let resolve model =
        let resolve = function Blocked id -> logEntry model.domainModel id | v -> v
        { model with console = model.console |> List.map resolve }
    let exec txt cmd =
        let eventId, domain' = Domain.execute model.domainModel txt cmd
        resolve { model with domainModel = domain'; console = (model.console@[logEntry domain' eventId]) }

    try
        match msg with
        | ENTER cmd ->
            match Domain.tryParseCommand model.domainModel cmd with
            | Some cmd' -> exec cmd cmd', Cmd.Empty
            | _ -> model, Cmd.Empty
        | RESET -> init()
        | Error err ->
            { model with console = model.console@[Resolved ("Error: " + err)] }, Cmd.Empty
        | Fulfill(key, v) ->
            ({ model with domainModel = model.domainModel |> Domain.setProperty key (System.Int32.Parse v |> Number) } |> resolve), Cmd.Empty
    with err ->
        { model with console = model.console@[Resolved err.Message] }, Cmd.Empty

