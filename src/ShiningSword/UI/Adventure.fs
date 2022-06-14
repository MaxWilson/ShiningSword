module UI.Adventure
open Feliz
open Domain.Adventure
open Domain.Character.Universal
open Domain.Ribbit
open Domain.Ribbit.Operations

type ControlMsg = Save | SaveAndQuit | Error of msg: string
type Activity = Downtime | AdventureIntro | Fighting | Looting | CompletingAdventure | PushingUpDaisies
type Msg = | Embark of AdventureSpec | Recruit of CharacterSheet | Proceed
type Model = {
    activity: Activity
    title: string option
    log: LogEntry list list
    spec: Domain.Adventure.AdventureSpec option // for referencing rewards, etc.
    state: Domain.Adventure.AdventureState
    }

let recruitCompanions model control dispatch =
    let getId = function Detail2e (char: CharacterSheet2e) -> char.id | Detail5e (char: CharacterSheet5e) -> char.id
    let isADND = model.state.mainCharacter.isADND
    let ineligibleIds = (model.state.mainCharacter::model.state.allies) |> List.map getId
    let candidates = LocalStorage.PCs.read() |> Array.filter (fun r -> r.isADND = isADND && not (ineligibleIds |> List.contains (getId r)))
    if candidates.Length = 0 then
        if model.state.allies.IsEmpty then
            "No companions are available at this time. (Try creating more characters first.)"
        else
            "No more companions are available at this time. (Try creating more characters first.)"
        |> Error |> control
    else
        Recruit (candidates |> chooseRandom) |> dispatch

let stillAlive (ribbit: Ribbit) (char: CharacterSheet) =
    let name = char.converge((fun c -> c.name), (fun c -> c.name))
    match ribbit.roster |> Map.tryFind name with
    | Some id ->
        (Domain.Ribbit.Operations.hpP.Get id ribbit) > (Domain.Ribbit.Operations.damageTakenP.Get id ribbit)
    | None -> true // if he's not been in combat yet then he's obviously still alive

let init sheet =
    let state = downtime sheet
    { activity = Downtime; state = state; title = None; spec = None; log = [] }

let update msg (model:Model) =
    match msg with
    | Embark spec ->
        let state = embark spec model.state.mainCharacter
        { state = state; title = spec.description |> Some; spec = Some spec; activity = AdventureIntro; log = [] }
    | Recruit companion ->
        let state' = model.state |> loadCharacters [companion]
        { model with state = { state' with allies = state'.allies@[companion] } }
    | Proceed when model.activity = Fighting ->
        let outcome, msgs, state' = model.state |> fightUntilFixedPoint
        match { model with state = state'; log = msgs::model.log }, outcome with
        | model, Victory -> { model with activity = Looting; title = "Victory!!! " + (msgs |> List.last).msg |> Some }
        | model, Defeat -> { model with title = Some "You have been defeated!"; activity = PushingUpDaisies }
        | model, _ -> model
    | Proceed ->
        match model.state.scheduledEncounters with
        | next::rest ->
            { model with state = model.state |> beginEncounter (next |> toOngoing) rest; title = Some next.description; activity = Fighting }
        | [] when model.activity = CompletingAdventure ->
            { model with state = downtime model.state.mainCharacter ; title = None; activity = Downtime }
        | [] ->
            let msg, state' = model.state |> finishAdventure model.spec.Value
            { model with state = state'; title = Some msg; activity = CompletingAdventure }

let class' element (className: string) (children: _ seq) = element [prop.className className; prop.children children]

// Bard's Tale-like summary of all friendlies and hostiles currently extant
let statusSummary (creatures: 'creature list) isFriendly (columns: {| title: string; render: 'creature -> string |} list) dispatch =
    class' Html.table "summaryTable" [
        Html.thead [
            Html.tr [
                for column in columns do
                    Html.th column.title
                ]
            ]
        Html.tbody [
            for creature in creatures do
                class' Html.tr (if isFriendly creature then "friendly" else "enemy") [
                    for column in columns do
                        Html.td (column.render creature)
                    ]
            ]
        ]

let view model control dispatch =
    class' Html.div "adventure" [
        yield! Chargen.View.viewCharacter model.state.mainCharacter
        match model.title with | Some title -> Html.span [prop.text title; prop.className "header"] | _ -> ()
        match model.log, model.activity with
        // If you won or lost, you want to see the log of how it happened
        | recent::_, (Fighting | Looting | PushingUpDaisies) ->
            class' Html.div "logOutput" [
                for entry in recent do
                    let className =
                        match entry.category, entry.important with
                        | Good, true -> "veryGood"
                        | Good, false -> "good"
                        | Bad, false -> "bad"
                        | Bad, true -> "veryBad"
                        | _ -> "neutral"
                    class' Html.div className [Html.text entry.msg]
                ]
        | _ -> ()
        if model.activity <> Downtime then
            let ribbit = model.state.ribbit |> Delta.derefM |> fst
            let isFriendly id =
                ribbit |> isFriendlyP.Get(id)
            let get f (_, id) = (f id ribbit).ToString()
            let getHP (_, id) =
                let hp = hpP.Get id ribbit
                let damage = damageTakenP.Get id ribbit
                if damage = 0 then
                    $"{hp}"
                else
                    $"{(hp-damage)}/{hp}"
            let rosterIds =
                ribbit.roster |> Map.toList
                |> List.sortBy(function
                    name, id ->
                        // friendlies first, then enemies; living before dead; otherwise in order of creation
                        let isFriendly = isFriendly id
                        let isAlive = (hpP.Get id ribbit) >= 0
                        not isFriendly, not isAlive, id)
            let columns = [
                {| title = "Name"; render = get personalNameP.Get |}
                {| title = "AC"; render = get acP.Get |}
                {| title = "HP"; render = getHP |}
                ]
            statusSummary rosterIds (snd >> isFriendly) columns dispatch
        let finalSection elements = class' Html.div "finalize" elements
        let choicebuttons elements = [
            if Browser.Dom.window.innerWidth < 800 then
                class' Html.div "beforeSummary" elements // redundant pre-summary section on mobile for better UX--not a perfect solution but better than needing scrolling
            finalSection elements
            ]
        match model.activity with
        | Downtime ->
            finalSection [
                let ruleSet = model.state.mainCharacter.raw
                Html.button [prop.text "Go on an easy adventure"; prop.onClick(fun _ -> easy ruleSet |> Embark |> dispatch)]
                Html.button [prop.text "Go on a hard adventure"; prop.onClick(fun _ -> hard() |> Embark |> dispatch)]
                Html.button [prop.text "Go on a deadly adventure"; prop.onClick(fun _ -> deadly() |> Embark |> dispatch)]
                Html.button [prop.text "Save and quit"; prop.onClick (thunk1 control SaveAndQuit)]
                ]
        | AdventureIntro ->
            // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices.
            // This might also be where you recruit companions.
            yield! choicebuttons [
                Html.button [prop.text "Proceed"; prop.onClick(fun _ -> Proceed |> dispatch)]
                Html.button [prop.text "Recruit companions"; prop.onClick(fun _ -> recruitCompanions model control dispatch)]
                ]
        | Fighting ->
            // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices
            yield! choicebuttons [
                Html.button [prop.text "Fight!"; prop.onClick(fun _ -> Proceed |> dispatch)]
                ]
        | Looting ->
            // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices
            yield! choicebuttons [
                Html.button [prop.text "Continue onward"; prop.onClick(fun _ -> Proceed |> dispatch)]
                Html.button [prop.text "Call it a day"; prop.onClick(fun _ -> SaveAndQuit |> control)]
                ]
        | CompletingAdventure ->
            // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices
            let finish _ =
                if model.state.mainCharacter |> stillAlive (model.state.ribbit |> Delta.derefM |> fst) then
                    Save |> control
                    Proceed |> dispatch
                else
                    SaveAndQuit |> control
            yield! choicebuttons [
                Html.button [prop.text "Finish"; prop.onClick(finish)]
                ]
        | PushingUpDaisies ->
            yield! choicebuttons [
                Html.button [prop.text "OK"; prop.onClick (thunk1 control SaveAndQuit)]
                ]
        ]

