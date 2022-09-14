[<AutoOpen>]
module Dev.Tracker.UI.Core
open Dev.Tracker
open Game.DataTypes
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz

open Fable.Core.JsInterop
open Fable.Core

type Msg =
    | ReviseInput of msg: string
    | SubmitInput
    | ExecuteCommand of string
    | ToggleHelp of bool option
    | ToggleBestiary of bool
    | ToggleLog of bool option
    | SetRewind of int option
    | LogNav of down:int * right: int

module Model =
    open Packrat
    open Commands
    type DisplayMode = MainScreen | HelpScreen
    type d = { input: string; game: Game.d; errors: string list; nowShowing: DisplayMode; showBestiary: bool; showLog: bool; rewindFrame: int option  }
    let fresh = { input = ""; game = Game.fresh; errors = []; nowShowing = MainScreen; showBestiary = true; showLog = false; rewindFrame = None }
    let executeIfPossible ui cmd =
        try
            { ui with input = ""; game = ui.game |> Game.update cmd; errors = [] }
        with err ->
            { ui with input = ""; errors = (err.ToString())::ui.errors }
    let executeTextCommandIfPossible (input:string) (ui: d) =
        if input.Trim().ToLowerInvariant() = "/startover" then fresh
        else
            match ParseArgs.Init(input, ui.game) with
            | Commands (cmds, End) ->
                let ui = cmds |> List.fold executeIfPossible ui
                let hadSuccessfulExecution = ui.errors.Length < cmds.Length
                if hadSuccessfulExecution then
                    executeIfPossible ui (Domain.Ribbit.Commands.AddLogEntry([], input) |> Game.RibbitCommand)
                else
                    ui
            | LoggingCommands(cmds, End) ->
                cmds |> List.fold executeIfPossible ui
            | _ -> ui

open Model
let init initialCmd =
    Model.fresh
#if DEBUG
    // start off with some data to make hot-reloading less onerous
    |> executeTextCommandIfPossible "add Loiosh, Boort, Hershel, Severiana"
    |> executeTextCommandIfPossible "define Fire Giant, Giant Crocodile, Mangler, Grue"
    |> executeTextCommandIfPossible "Fire Giant hp 162, xp 5000, init -1"
    |> executeTextCommandIfPossible "Giant Crocodile hp 85, xp 1800"
    |> executeTextCommandIfPossible "Mangler hp 71, xp 1800"
    |> executeTextCommandIfPossible "Boort hp 50, init +4"
    |> executeTextCommandIfPossible "Severiana hp 44, init +1"
    |> executeTextCommandIfPossible "add Fire Giant, Giant Crocodile"
    |> executeTextCommandIfPossible "fire giant 1 maxhp172"
    |> executeTextCommandIfPossible "Boort will wildshape and attack Fire Giant"
    |> executeTextCommandIfPossible "Loiosh will attack Fire Giant 1"
    |> executeTextCommandIfPossible "Hershel will disarm Fire Giant 1 and run off with his weapon, action surging if necessary"
    |> executeTextCommandIfPossible "Severiana will Fireball"
    |> executeTextCommandIfPossible "Fire Giant #1 will attack Severiana"
    |> executeTextCommandIfPossible "Giant Crocodile #1 will Dodge and shadow Hershel"
    |> executeTextCommandIfPossible "Severiana hits Fire Giant 1 for 33, Giant Crocodile 1 for 16"
    |> executeTextCommandIfPossible "add Grue, Grue, Grue, Grue"
    |> executeTextCommandIfPossible "Grue #1 hp 23"
    |> executeTextCommandIfPossible "Boort hits Grue 1 for 10, Grue 2 for 10, Grue 3 for 5"
    |> executeTextCommandIfPossible "Grue 17hp,50xp"
    |> executeTextCommandIfPossible "Grue 1 hp 14"
    |> executeTextCommandIfPossible "Grue 2 maxhp 18, xp 50"
    |> executeTextCommandIfPossible "Rename Giant Crocodile #1 Sparky"
    |> executeTextCommandIfPossible "Sparky hits Boort for 28"
    |> executeTextCommandIfPossible "Severiana hits Sparky for 20"
    |> executeTextCommandIfPossible "Severiana hits Sparky for 27"
    |> executeTextCommandIfPossible "Loiosh hits Sparky for 51" // even though Loiosh rolled high damage, Severiana should get more XP because most of Loiosh's damage was overkill
    |> executeTextCommandIfPossible "rename Fire Giant #1 Thor"
    |> executeTextCommandIfPossible "Thor: angry"
    |> executeTextCommandIfPossible "Thor:: angry and tired"

#endif
open Domain.Ribbit.Operations
open Dev.Tracker.Game.Game.Getters

// move to the next character or phase
let advance model =
    match model.game |> Game.combatPhaseP.Get with
    | Game.Declaring ->
        let initiatives =
            [
            for name in model.game.data.roster |> Map.keys do
                match tryGetRibbit name currentInitiativeP model.game with
                | Some v -> () // don't reroll if it's already there
                | _ ->
                    let initRoll = get name initiativeModifierP model.game + rand 20
                    name, initRoll
                ]
        let ribbit = initiatives |> List.fold (fun ribbit (name, initRoll) -> ribbit |> Game.setByName name currentInitiativeP initRoll) model.game
        let initiative name =
            tryGetRibbit name currentInitiativeP ribbit
        let newPhase =
            model.game.data.roster |> Map.keys |> List.ofSeq
            |> List.filter (fun name -> tryGetRibbit name actionDeclarationTextP ribbit |> Option.isSome)
            |> List.sortByDescending initiative
            |> Game.Executing
        { model with game = model.game |> Game.combatPhaseP.Set newPhase }
    | Game.Executing [] ->
        model.game.data.roster |> Map.keys |> List.ofSeq
        |> List.fold (fun model name -> match getId name model with | Some id -> currentInitiativeP.Clear id model | _ -> model) model.game
        |> fun ribbit ->
            { model with game = ribbit |> Game.combatPhaseP.Set Game.Declaring }
    | Game.Executing (_::rest) -> { model with game = model.game |> Game.combatPhaseP.Set (Game.Executing rest) }

let update msg (model: Model.d) =
    match msg with
    | ReviseInput input -> { model with input = input }
    | SubmitInput when model.input = "ok" ->
        { model with input = "" } |> advance
    | SubmitInput -> model |> Model.executeTextCommandIfPossible model.input
    | ExecuteCommand cmd -> model |> Model.executeTextCommandIfPossible cmd
    | ToggleHelp showHelp ->
        let showHelp =
            match showHelp with
            | Some v -> v
            | None -> model.nowShowing <> HelpScreen
        { model with nowShowing = if showHelp then HelpScreen else MainScreen }
    | ToggleBestiary showBestiary -> { model with showBestiary = showBestiary }
    | ToggleLog showLog -> { model with showLog = defaultArg showLog (model.showLog |> not) }
    | SetRewind ix -> { model with rewindFrame = ix }
    | LogNav(down, right) ->
        // LogNav is a more indirect, incremental way of doing SetRewind with arrow keys within the log
        if model.showLog |> not then
            model
        else
            // for now the only meaning of left is to unfocus the log; right means start at the end and is otherwise identical to down
            // these will change when log is hierarchical
            let r = model.game.data
            let roots = r.eventRoots.inOrder()
            let setRewind ix = { model with rewindFrame = ix }
            let setRewindByLogIndex ix = (if 0 <= ix && ix < roots.Length then r.events[roots[ix]].timeTravelIndex |> Some else None) |> setRewind
            match model.rewindFrame |> Option.bind (fun frame -> roots |> List.tryFindIndex (fun ix -> r.events[ix].timeTravelIndex >= frame)) with
            | None -> // if not currently focused anywhere valid
                if (right > 0 || down < 0) && roots.Length > 0 then // start at back; note that it's slightly wrong that we don't treat right=2 different from right=1, should fix when we add hierarchy
                    r.events[roots |> List.last].timeTravelIndex |> Some |> setRewind
                elif down > 0 && roots.Length > 0 then // start at back; note that it's slightly wrong that we don't treat right=2 different from right=1, should fix when we add hierarchy
                    r.events[roots |> List.head].timeTravelIndex |> Some |> setRewind
                else model
            | Some currentIndex ->
                if right < 0 then setRewind None
                else currentIndex + down + right |> setRewindByLogIndex

open UI.Components

module Getters =
    open Domain.Ribbit

    let getAllNames (model:Model.d) =
        model.game.data.roster |> Map.keys |> List.ofSeq
    let getAllNamesInOrder (model:Model.d) =
        model.game.data.roster |> Seq.map (function KeyValue(k,v) -> k,v) |> Seq.sortBy snd |> Seq.map fst |> List.ofSeq
    let tryGetRibbit name (prop: Property<_, Ribbit>) (model:Model.d) =
        model.game |> Game.Getters.tryGetRibbit name prop

open Getters
open Domain.Ribbit.Operations
open Dev.Tracker.Game.Game
open Domain.Ribbit

let view (model: Model.d) dispatch =
    let trueModel = model
    let model = match model.rewindFrame with Some ix -> { model with game = model.game.rewindTo ix } | None -> model
    let combatPhase = Game.combatPhaseP.Get model.game
    let setCommand txt =
        (ReviseInput txt) |> dispatch
    let table = class' Html.div "table" [
        Html.table [
            textHeaders ["Name"; "Type"; "Actions"; "XP earned"; "HP"]
            Html.tbody [
                for name in getAllNamesInOrder model do
                    let isSelected = match combatPhase with Executing (h::_) when h = name -> true | _ -> false
                    class' Html.tr (if isSelected then "currentTurn" else "") [
                        let tryGet prop = model |> tryGetRibbit name prop

                        let template =
                            match templateTypeName name (EvaluationContext.Create model.game) with
                            | Ok v -> Some v
                            | _ -> None
                        match tryGet notesP with
                        | None | Some [] ->
                            textCell $"{name}"
                        | Some notes ->
                            textCell $"""{name} ({String.join ";" notes})"""
                        textCell $"""({defaultArg template "PC"})"""
                        let action =
                            let initMod =
                                match get name initiativeModifierP model.game with
                                | v when v <> 0 -> Some $"%+i{v}"
                                | _ -> None
                            let verb =
                                let willAct = match combatPhase with | Declaring -> true | Executing haventActed -> haventActed |> List.contains name
                                if willAct then "will" else "did"
                            match tryGet actionDeclarationTextP, tryGetRibbit name currentInitiativeP model, initMod with
                            | Some (action), Some init, Some initMod ->
                                $"{init}: {name} {verb} {action} ({initMod})"
                            | Some (action), Some init, None ->
                                $"{init}: {name} {verb} {action}"
                            | Some (action), None, Some initMod ->
                                $"{name} {verb} {action} ({initMod})"
                            | Some (action), None, None ->
                                $"{name} {verb} {action}"
                            | None, _, Some initMod when combatPhase = Declaring ->
                                $"({initMod})"
                            | None, _, None when combatPhase = Declaring ->
                                $""
                            | _ -> $"{name} does nothing"

                        let clickableText (txt: string) msg =
                            Html.td [prop.text txt; prop.onDoubleClick (fun _ -> setCommand msg)]

                        clickableText action $"{name} will "
                        let remainingHP =
                            match tryGetRibbit name Domain.Ribbit.Operations.hpP model |> Option.defaultValue 0, tryGetRibbit name Domain.Ribbit.Operations.damageTakenP model  |> Option.defaultValue 0 with
                            | hp, dmg when dmg = 0 -> $"{hp} HP"
                            | hp, dmg when dmg > hp -> $"{hp - dmg}/{hp} HP (dead)"
                            | hp, dmg -> $"{hp - dmg}/{hp} HP"
                        textCell (if template.IsNone then $"{tryGet xpEarnedP} XP" else "")
                        textCell remainingHP
                        ]
                    ]
                ]
            ]
    let inputPanel =
        class' Html.div "inputPanel" [
            let label =
                match combatPhase with
                | Declaring -> "Declaring"
                | Executing (h::_) -> $"{h}'s turn"
                | Executing [] -> "End of round"
            Html.div [prop.text label; prop.className "inputHeader"]
            Html.input [
                prop.id "userInput"
                prop.placeholder "Enter a command, e.g. define Beholder"
                prop.autoFocus true
                prop.ref(fun e ->
                    let value = model.input
                    if e |> isNull |> not && !!e?value <> !!value then
                        // Feliz valueOrDefault logic: set the underlying HTML element's value. Note that !! in Fable just means unbox, not deref.
                        // We use this in lieu of setting value directly in order to avoid a weird race condition: https://github.com/Zaid-Ajaj/Feliz/issues/320
                        e?value <- !!value
                        // maybe in future there will be a better way to complete commands than setting focus (e.g. click name, click "attack", click target name)
                        // but maybe not, and for now filling in e.g. "Fire Giant #1 will " at least saves typing and mistakes (such as forgetting the #1).
                        e?focus()
                    )
                prop.onKeyDown (fun e ->
                    if e.key = "Escape" then // KeyPress doesn't fire for Escape
                        e.preventDefault()
                        dispatch (ReviseInput "")
                    )
                prop.onKeyPress (fun e ->
                    if e.key = "Enter" then
                        e.preventDefault()
                        dispatch SubmitInput
                    );
                prop.onChange (fun (e: string) ->
                    ReviseInput e |> dispatch)
                ]
            Html.button [prop.text "OK"; prop.onClick (fun _ -> dispatch SubmitInput)]
            ]
    let errors =
        class' Html.div "errors" [
            for err in model.errors do
                Html.div err
            ]
    let bestiary =
        CollapsibleSection.create ("bestiary", model.showBestiary, ToggleBestiary >> dispatch) <| fun () ->
            class' Html.table "bestiary" [
                Html.thead [
                    Html.tr [Html.th [prop.text "Type"]; Html.th [prop.text "HP"]; Html.th [prop.text "XP reward"]]
                    ]
                Html.tbody [
                    for KeyValue(name, id) in model.game.data.kindsOfMonsters do
                        Html.tr [
                            match combatPhase with
                            | Executing (h::_) when h = name ->
                                prop.className "currentTurn"
                            | _ -> ()
                            prop.children [
                                textCell name
                                let onNumber makeMessage newNumber = newNumber |> makeMessage |> ExecuteCommand |> dispatch
                                let hp = tryGetRibbit name Domain.Ribbit.Operations.hpP model |> Option.map toString |> Option.defaultValue ""
                                EditableNumberCell (hp, (onNumber <| fun hp -> $"{name} maxhp {hp}"))
                                EditableNumberCell ((match tryGetRibbit name xpValueP model with Some v -> v.ToString() | None -> ""), (onNumber (fun v -> $"{name} xp {v}")))
                                ]
                        ]
                    ]
                ]
    let log() =
        Html.div [
            prop.className "log"
            prop.children [
                let title (txt: string) = Html.div [prop.className "title"; prop.text txt]
                let log = trueModel.game.data.eventRoots
                if log.Length = 0 then
                    title "Nothing has happened yet"
                else
                    title "What has happened so far:"
                    class' Html.ul "entries" [
                        for ix in log.inOrder() do
                            let event = trueModel.game.data.events[ix]
                            match event.log with
                            | Some logEntry ->
                                Html.li [
                                    if Some event.timeTravelIndex = model.rewindFrame then
                                        prop.className "logEntry selected"
                                    else
                                        prop.className "logEntry"
                                    prop.text $"{logEntry.msg}"
                                    prop.onClick (fun e ->
                                        e.preventDefault();
                                        let historyIx = event.timeTravelIndex
                                        SetRewind(match trueModel.rewindFrame with Some ix' when ix' = historyIx -> None | _ -> Some historyIx)
                                            |> dispatch)
                                    ]
                            | None -> ()
                        ]
                ]
            ]
    let logLink() =
        Html.a [
            prop.text "Log"
            prop.onClick (thunk1 dispatch (ToggleLog None))
            ]
    class' Html.div (["dev"; if model.showLog then begin "withsidebar" end; if trueModel.rewindFrame.IsSome then begin "historical" end] |> String.join " ") [
        withHeader (model.nowShowing = HelpScreen) helpText (Some >> ToggleHelp >> dispatch) [logLink()] [
            table
            inputPanel
            errors
            CollapsibleSection.renderAsButton bestiary
            if model.showLog then
                log()
            ]
        ]
