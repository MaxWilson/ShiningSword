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
type ConsoleLog = { cmdText: string; eventId: Id option }
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
            for c in m.console ->
                match c.eventId |> Option.bind (fun id -> m.domainModel.eventLog |> tryFind id) with
                | None -> // failed to parse command
                    li[ClassName "error"][str (sprintf "Didn't understand '%s'" c.cmdText)]
                | Some e ->
                    match e.status with
                    | Ready v -> li[ClassName "resolved"][str <| v.ToString()]
                    | Awaiting(_) ->
                        let d = m.domainModel
                        let rec getDependency = function
                            | PropertyRef(id, _) ->
                                d.blocking.backward.[EventRef id] |> List.collect (function PropertyRef(id, prop) -> [if id > 0 then (d.roster |> SymmetricMap.find id) + "." + prop else prop] | ref -> getDependency ref)
                            | ref ->
                                let d = m.domainModel
                                d.blocking.backward.[ref].Head |> getDependency
                        let eventId = c.eventId.Value
                        try
                            try
                                let dependencies = getDependency (EventRef eventId) |> String.join ", "
                                li[ClassName "blocked"][str <| sprintf "%s (needs %s)" c.cmdText dependencies]
                            with _ -> li[ClassName "blocked"][str <| sprintf "Couldn't render %s" c.cmdText]
                        with _ -> li[ClassName "blocked"][str <| sprintf "Couldn't render #%d" eventId]
            ]
        div [ClassName ("frame" + if log = [] then "" else " withSidebar")] [
            yield div[ClassName "summaryPane"][
                    table [] (summaryOf m.domainModel)
                ]
            let notWhitespace = (not << System.String.IsNullOrWhiteSpace)
            yield div[ClassName "queryPane"] [
                View.ViewComponents.localForm "Enter a command please:" [AutoFocus true] notWhitespace (ENTER >> dispatch)
                br[]
                match m.domainModel.blocking.forward
                        |> Seq.choose (function KeyValue(PropertyRef(key), _) -> Some key | _ -> None)
                        |> List.ofSeq with
                | [] -> ()
                | blocked -> div[][
                    for ((id, prop) as key) in blocked do
                        yield View.ViewComponents.localForm
                                (sprintf "Enter value for %s's %s" (m.domainModel.roster |> SymmetricMap.find id) prop)
                                [ClassName "bordered"]
                                notWhitespace
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
    let exec txt cmd =
        let eventId, domain' = Domain.execute model.domainModel cmd
        { model with domainModel = domain'; console = (model.console@[{ cmdText = txt; eventId = Some eventId }]) }
    let logError txt = { cmdText = txt; eventId = None }
    try
        match msg with
        | ENTER cmd ->
            Browser.Dom.console.log(model.domainModel)
            Browser.Dom.console.log(model.domainModel.roster |> SymmetricMap.toSeq |> Array.ofSeq)
            match Domain.tryParseExecutable model.domainModel cmd with
            | Some cmd' -> exec cmd cmd', Cmd.Empty
            | _ -> { model with console = model.console@[logError (sprintf "Could not parse '%s'" cmd)] }, Cmd.Empty
        | RESET -> init()
        | Error err ->
            { model with console = model.console@[logError ("Error: " + err)] }, Cmd.Empty
        | Fulfill(key, v) ->
            { model with domainModel = model.domainModel |> Domain.setProperty key (System.Int32.Parse v |> Number) }, Cmd.Empty
    with err ->
        { model with console = model.console@[logError ("Exception: " + err.ToString())] }, Cmd.Empty

