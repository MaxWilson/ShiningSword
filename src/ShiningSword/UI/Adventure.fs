module UI.Adventure
open Feliz
open Domain.Adventure
open Domain.Character.Universal
open Domain.Ribbit.Operations

type ControlMsg = SaveAndQuit | Error of msg: string
type Activity = Downtime | AdventureIntro | Fighting | Looting | CompletingAdventure
type Msg = | Embark of AdventureSpec | Recruit of CharacterSheet | Proceed | Victory
type Model = {
    activity: Activity
    title: string option
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
        let chosen = candidates |> chooseRandom
        let getName = (function GenericCharacterSheet sheet -> sheet.name)
        printfn $"{(candidates |> Array.map getName)} => {getName chosen}"
        Recruit chosen |> dispatch

let init sheet =
    let state = downtime sheet
    { activity = Downtime; state = state; title = None; spec = None }

let update msg (model:Model) =
    match msg with
    | Embark spec ->
        let state = embark spec model.state.mainCharacter
        { state = state; title = spec.description |> Some; spec = Some spec; activity = AdventureIntro }
    | Recruit companion ->
        let state' = model.state |> Domain.Adventure.loadCharacters [companion]
        { model with state = { state' with allies = state'.allies@[companion] } }
    | Proceed ->
        match model.state.scheduledEncounters with
        | next::rest ->
            { model with state = model.state |> beginEncounter (next |> toOngoing) rest; title = Some next.description; activity = Fighting }
        | [] when model.activity = CompletingAdventure ->
            { model with state = downtime model.state.mainCharacter ; title = None; activity = Downtime }
        | [] ->
            let msg, state' = model.state |> finishAdventure model.spec.Value
            { model with state = state'; title = Some msg; activity = CompletingAdventure }
    | Victory ->
        match model.state.currentEncounter with
        | Some encounter ->
            let state', description = victory encounter model.state
            { model with state = state'; title = Some description; activity = Looting }
        | None -> shouldntHappen()

let class' element (className: string) (children: _ seq) = element [prop.className className; prop.children children]

// Bard's Tale-like summary of all friendlies and hostiles currently extant
let statusSummary (creatures: 'creature list) (columns: {| title: string; render: 'creature -> string |} list) dispatch =
    class' Html.table "summaryTable" [
        Html.thead [
            Html.tr [
                for column in columns do
                    Html.th column.title
                ]
            ]
        Html.tbody [
            for creature in creatures do
                Html.tr [
                    for column in columns do
                        Html.td (column.render creature)
                    ]
            ]
        ]

let view model control dispatch =
    class' Html.div "adventure" [
        yield! Chargen.View.viewCharacter model.state.mainCharacter
        match model.title with | Some title -> Html.span [prop.text title; prop.className "header"] | _ -> ()
        if model.activity <> Downtime then
            let ribbit = model.state.ribbit
            let friendlies = (model.state.mainCharacter::model.state.allies) |> List.map (function GenericCharacterSheet sheet -> sheet.name)
            let isFriendly name =
                friendlies |> List.contains name
            let get f id = (f id ribbit).ToString()
            let rosterIds =
                ribbit.roster |> List.ofSeq
                |> List.sortBy (fun kv ->
                    not (List.contains kv.Key friendlies), kv.Key)
                |> List.map (fun kv -> kv.Value)
            let columns = [
                {| title = "Name"; render = get personalNameP.Get |}
                {| title = "HP"; render = get hpP.Get |}
                {| title = "AC"; render = get acP.Get |}
                ]
            statusSummary rosterIds columns  dispatch
        class' Html.div "finalize" [
            match model.activity with
            | Downtime ->
                Html.button [prop.text "Go on an easy adventure"; prop.onClick(fun _ -> easy() |> Embark |> dispatch)]
                Html.button [prop.text "Go on a hard adventure"; prop.onClick(fun _ -> hard() |> Embark |> dispatch)]
                Html.button [prop.text "Go on a deadly adventure"; prop.onClick(fun _ -> deadly() |> Embark |> dispatch)]
                Html.button [prop.text "Save and quit"; prop.onClick (thunk1 control SaveAndQuit)]
            | AdventureIntro ->
                // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices.
                // This might also be where you recruit companions.
                Html.button [prop.text "Proceed"; prop.onClick(fun _ -> Proceed |> dispatch)]
                Html.button [prop.text "Recruit companions"; prop.onClick(fun _ -> recruitCompanions model control dispatch)]
            | Fighting ->
                // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices
                Html.button [prop.text "Win!"; prop.onClick(fun _ -> Victory |> dispatch)]
            | Looting ->
                // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices
                Html.button [prop.text "Continue onward"; prop.onClick(fun _ -> Proceed |> dispatch)]
                Html.button [prop.text "Call it a day"; prop.onClick(fun _ -> SaveAndQuit |> control)]
            | CompletingAdventure ->
                // later on if there are more choices, this could become a full-fledged Adventuring phase with RP choices
                Html.button [prop.text "Finish"; prop.onClick(fun _ -> Proceed |> dispatch)]
            ]
        ]

