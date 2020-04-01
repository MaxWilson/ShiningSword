module UI.Ribbit

open System
open Elmish
open Common

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Data
open Data.Functor
open Domain
open Domain.Prelude
open Feliz
open UI
open UI.Feliz.Html
open UI.Feliz.Prop

type Id = int
type ConsoleLog = { cmdText: string; eventId: Id option }
type Model = { console: ConsoleLog list; domainModel: Domain.Model }
module Lens =
    let domainModel = Lens.lens (fun d -> d.domainModel) (fun v d -> { d with domainModel = v })
type Cmd = ENTER of string | RESET | Error of string | FulfillProperty of Key * value:string | FulfillRoll of EventId * int
let summaryOf (m:Domain.Model) =
    let data = m.data
    table [
        children [
            thead [
                tr [
                    yield th "Name"
                    for p in m.properties do yield th p.name
                ]
            ]
            tbody [
                for (id, name) in m.roster |> SymmetricMap.toSeq do
                    yield tr[
                        yield td name
                        for p in m.properties ->
                            match data |> Map.tryFind (id, p.name) with
                            | Some v -> v.ToString() |> td
                            | None -> "???" |> td
                    ]
            ]
        ]
    ]
module Css =
    let error = className "error"
    let resolved = className "resolved"
    let blocked = className "blocked"
    let summaryPane = className "summaryPane"
    let queryPane = className "queryPane"
    let bordered = className "bordered"
    let sidebar = className "sidebar"

let view m dispatch =
    try
        let log = [
            for c in m.console do
                match c.eventId |> Option.bind (fun id -> m.domainModel.eventLog |> tryFind id) with
                | None -> // failed to parse command
                    yield li [Css.error; sprintf "Didn't understand '%s'" c.cmdText |> text]
                | Some e ->
                    match e.status with
                    | Ready v ->
                        if v <> Nothing then
                            yield li [Css.resolved; v.ToString() |> text]
                    | Awaiting(_) ->
                        let d = m.domainModel
                        let rec getDependency = function
                            | PropertyRef(_) as ref ->
                                d.blocking.backward |> Map.tryFind ref |> Option.defaultValue [ref] |> List.collect (function PropertyRef(id, prop) -> [if id > 0 then (d.roster |> find id) + "." + prop else prop] | ref -> getDependency ref)
                            | EventRef id as ref ->
                                let d = m.domainModel
                                match d.eventLog |> find id |> Event.Status with
                                | AwaitingRoll r -> [Dice.toString r]
                                | _ ->
                                    d.blocking.backward.[ref].Head |> getDependency
                        let eventId = c.eventId.Value
                        yield
                            try
                                try
                                    let dependencies = getDependency (EventRef eventId) |> String.join ", "
                                    li[sprintf "%s (needs %s)" c.cmdText dependencies |> text; Css.blocked]
                                with e -> li[sprintf "Couldn't render %s because '%s'" c.cmdText (e.ToString()) |> text; Css.blocked]
                            with _ -> li[sprintf "Couldn't render #%d" eventId |> text; Css.blocked]
            ]
        div [
            className ("frame" + if log = [] then "" else " withSidebar")
            children [
                div [
                    Css.summaryPane
                    children [summaryOf m.domainModel]
                    ]
                let notWhitespace = (not << System.String.IsNullOrWhiteSpace)
                div [
                    Css.queryPane
                    children [
                        localForm "Enter a command please:" [autoFocus true] notWhitespace (ENTER >> dispatch)
                        br[]
                        match m.domainModel.blocking.forward |> Map.keys
                                |> List.ofSeq with
                        | [] -> ()
                        | blocked -> div [
                            for blockage in blocked do
                                match blockage with
                                | PropertyRef((id, prop) as key) ->
                                    yield localForm
                                        (sprintf "Enter value for %s's %s" (m.domainModel.roster |> find id) prop)
                                        [Css.bordered]
                                        notWhitespace
                                        (fun v -> dispatch (FulfillProperty (key, v)))
                                | EventRef(id) ->
                                    match m.domainModel.eventLog |> find id |> Event.Status with
                                    | AwaitingRoll r ->
                                        yield localForm
                                            (sprintf "Roll %s" <| Dice.toString r)
                                            [Css.bordered]
                                            notWhitespace
                                            (fun v ->
                                                match System.Int32.TryParse v with
                                                | true, n -> dispatch (FulfillRoll(id, n))
                                                | _ -> dispatch (Error <| sprintf "'%s' is not a number" v))
                                    | _ -> ()
                            ]
                        br[]
                        ]
                    ]
                if log <> [] then
                    div[
                        Css.sidebar
                        children [
                            ul log
                            button[
                                onClick (fun _ -> dispatch RESET)
                                text "Clear"]
                        ]
                    ]
                ]
        ]
    with e ->
        div (e.ToString())

let init _ = { console = []; domainModel = Domain.fresh }, Cmd.Empty
let update msg model =
    let exec txt cmd =
        let eventId, domain' = Domain.execute model.domainModel cmd
        { model with domainModel = domain'; console = (model.console @ [{ cmdText = txt; eventId = Some eventId }]) }
    let logError txt = { cmdText = txt; eventId = None }
    try
        match msg with
        | ENTER cmd ->
            Browser.Dom.console.log(model.domainModel)
            Browser.Dom.console.log(model.domainModel.roster |> SymmetricMap.toSeq |> Array.ofSeq)
            match Domain.tryParseExecutable model.domainModel cmd with
            | Some cmd' -> exec cmd cmd', Cmd.Empty
            | _ -> { model with console = model.console @ [logError (sprintf "Could not parse '%s'" cmd)] }, Cmd.Empty
        | RESET -> init()
        | Error err ->
            { model with console = model.console @ [logError ("Error: " + err)] }, Cmd.Empty
        | FulfillProperty(key, v) ->
            { model with domainModel = model.domainModel |> Domain.setProperty key (System.Int32.Parse v |> Number) |> progressToFixedPoint }, Cmd.Empty
        | FulfillRoll(eventId, v) ->
            { model with domainModel = model.domainModel |> Domain.fulfillRoll eventId v |> progressToFixedPoint }, Cmd.Empty
    with err ->
        { model with console = model.console @ [logError ("Exception: " + err.ToString())] }, Cmd.Empty

