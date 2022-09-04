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
    | ToggleHelp of bool
    | ToggleBestiary of bool
    | ToggleLog of bool

module Model =
    open Packrat
    open Commands
    type GameMode = Declaring | Executing of Name list
    type d = { input: string; game: Game.d; errors: string list; log: string list; showHelp: bool; showBestiary: bool; showLog: bool; mode: GameMode }
    let fresh = { input = ""; game = Game.fresh; errors = []; log = []; showHelp = false; showBestiary = true; showLog = false; mode = Declaring }
    let executeIfPossible ui cmd =
        try
            { ui with input = ""; game = ui.game |> Game.update cmd; errors = [] }
        with err ->
            { ui with input = ""; errors = (err.ToString())::ui.errors }
    let executeTextCommandIfPossible input (ui: d) =
        match ParseArgs.Init(input, ui.game) with
        | Commands (cmds, End) ->
            let ui = cmds |> List.fold executeIfPossible ui
            let hadSuccessfulExecution = ui.errors.Length < cmds.Length
            if hadSuccessfulExecution then
                { ui with log = ui.log@[input] }
            else
                ui
        | Command (cmd, End) ->
            executeIfPossible ui cmd
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
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Mangler"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"
    //|> executeTextCommandIfPossible "add Grue"

#endif
open Domain.Ribbit.Operations
open Dev.Tracker.Game.Game.Getters

// move to the next character or phase
let advance model =
    match model.mode with
    | Declaring ->
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
        { model with
            game = ribbit
            mode =
                model.game.data.roster |> Map.keys |> List.ofSeq
                |> List.filter (fun name -> tryGetRibbit name actionDeclarationTextP ribbit |> Option.isSome)
                |> List.sortByDescending initiative
                |> Executing
            }
    | Executing [] ->
        model.game.data.roster |> Map.keys |> List.ofSeq
        |> List.fold (fun model name -> match getId name model with | Some id -> currentInitiativeP.Clear id model | _ -> model) model.game
        |> fun ribbit ->
            { model with mode = Declaring; game = ribbit }
    | Executing (_::rest) -> { model with mode = Executing rest }

let update msg (model: Model.d) =
    match msg with
    | ReviseInput input -> { model with input = input }
    | SubmitInput when model.input = "ok" ->
        { model with input = "" } |> advance
    | SubmitInput -> model |> Model.executeTextCommandIfPossible model.input
    | ExecuteCommand cmd -> model |> Model.executeTextCommandIfPossible cmd
    | ToggleHelp showHelp -> { model with showHelp = showHelp }
    | ToggleBestiary showBestiary -> { model with showBestiary = showBestiary }
    | ToggleLog showLog -> { model with showLog = showLog }

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
    let setCommand txt =
        (ReviseInput txt) |> dispatch
    let table = class' Html.div "table" [
        Html.table [
            textHeaders ["Name"; "Type"; "Actions"; "Notes"; "XP earned"; "HP"]
            Html.tbody [
                for name in getAllNamesInOrder model do
                    let isSelected = match model.mode with Executing (h::_) when h = name -> true | _ -> false
                    class' Html.tr (if isSelected then "currentTurn" else "") [
                        let tryGet prop = model |> tryGetRibbit name prop

                        let type1 =
                            match templateTypeName name (EvaluationContext.Create model.game) with
                            | Ok v -> v
                            | _ -> "PC"
                        textCell $"{name}"
                        textCell $"({type1})"
                        let action =
                            let initMod =
                                match get name initiativeModifierP model.game with
                                | v when v <> 0 -> Some $"%+i{v}"
                                | _ -> None
                            let verb =
                                let willAct = match model.mode with | Declaring -> true | Executing haventActed -> haventActed |> List.contains name
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
                            | None, _, Some initMod when model.mode = Declaring ->
                                $"({initMod})"
                            | None, _, None when model.mode = Declaring ->
                                $""
                            | _ -> $"{name} does nothing"

                        let clickableText (txt: string) msg =
                            Html.td [prop.text txt; prop.onDoubleClick (fun _ -> setCommand msg)]

                        clickableText action $"{name} will "
                        clickableText (System.String.Join(";", tryGet notesP)) $"{name}: "
                        textCell $"{tryGet xpEarnedP} XP earned"
                        let remainingHP =
                            match tryGetRibbit name Domain.Ribbit.Operations.hpP model |> Option.defaultValue 0, tryGetRibbit name Domain.Ribbit.Operations.damageTakenP model  |> Option.defaultValue 0 with
                            | hp, dmg when dmg = 0 -> $"{hp} HP"
                            | hp, dmg when dmg > hp -> $"{hp - dmg}/{hp} HP (dead)"
                            | hp, dmg -> $"{hp - dmg}/{hp} HP"
                        textCell remainingHP
                        ]
                    ]
                ]
            ]
    let inputPanel =
        class' Html.div "inputPanel" [
            let label =
                match model.mode with
                | Declaring -> "Declaring"
                | Executing (h::_) -> $"{h}'s turn"
                | Executing [] -> "End of round"
            Html.div [prop.text label; prop.className "inputHeader"]
            Html.input [
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
                            match model.mode with
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
    let log =
        CollapsibleSection.create("log", model.showLog, ToggleLog >> dispatch) <| fun () ->
            Html.div [
                let title (txt: string) = Html.div [prop.className "title"; prop.text txt]
                if model.log.Length = 0 then
                    title "Nothing has happened yet"
                else
                    title "What has happened so far:"
                    Html.ul [
                        for logEntry in model.log do
                            Html.li logEntry
                        ]
                ]
    class' Html.div (if log.show then "dev withsidebar" else "dev") [
        withHelp model.showHelp helpText (ToggleHelp >> dispatch) [
            table
            inputPanel
            errors
            CollapsibleSection.render bestiary
            CollapsibleSection.render log
            ]
        ]
