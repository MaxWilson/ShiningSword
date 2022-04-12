module UI.Adventure
open Feliz
open Domain.Adventure

type ControlMsg = SaveAndQuit
type Activity = Downtime | AdventureIntro | Fighting | Looting | CompletingAdventure
type Msg = | Embark of AdventureSpec | Proceed | Victory
type Model = {
    activity: Activity
    title: string option
    spec: Domain.Adventure.AdventureSpec option // for referencing rewards, etc.
    state: Domain.Adventure.AdventureState
    }

let init sheet =
    let state = downtime sheet
    { activity = Downtime; state = state; title = None; spec = None }

let update msg (model:Model) =
    match msg with
    | Embark spec ->
        let state = embark spec model.state.mainCharacter
        { state = state; title = spec.description |> Some; spec = Some spec; activity = AdventureIntro }
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
let view model control dispatch =
    class' Html.div "adventure" [
        yield! Chargen.View.viewCharacter model.state.mainCharacter
        match model.title with | Some title -> Html.span [prop.text title; prop.className "header"] | _ -> ()
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

