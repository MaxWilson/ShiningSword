module Dev.App.Tracker.UI

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
    | ExecuteCommand of Game.Command
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
    let executeInputIfPossible input (ui: d) =
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
    |> executeInputIfPossible "add Loiosh, Boort, Hershel, Severiana"
    |> executeInputIfPossible "define Fire Giant, Giant Crocodile, Mangler, Grue"
    |> executeInputIfPossible "Fire Giant hp 162, xp 5000, init -1"
    |> executeInputIfPossible "Giant Crocodile hp 85, xp 1800"
    |> executeInputIfPossible "Mangler hp 71, xp 1800"
    |> executeInputIfPossible "Boort hp 50, init +4"
    |> executeInputIfPossible "Severiana hp 44, init +1"
    |> executeInputIfPossible "add Fire Giant, Giant Crocodile"
    |> executeInputIfPossible "Boort will wildshape and attack Fire Giant"
    |> executeInputIfPossible "Loiosh will attack Fire Giant 1"
    |> executeInputIfPossible "Hershel will disarm Fire Giant 1 and run off with his weapon, action surging if necessary"
    |> executeInputIfPossible "Severiana will Fireball"
    |> executeInputIfPossible "Fire Giant #1 will attack Severiana"
    |> executeInputIfPossible "Giant Crocodile #1 will Dodge and shadow Hershel"
    |> executeInputIfPossible "Severiana hits Fire Giant 1 for 33, Giant Crocodile 1 for 16"
    |> executeInputIfPossible "add Grue, Grue, Grue, Grue"
    |> executeInputIfPossible "Grue #1 hp 23"
    |> executeInputIfPossible "Boort hits Grue 1 for 10, Grue 2 for 10, Grue 3 for 5"
    |> executeInputIfPossible "Grue hp 17, xp 50"
    |> executeInputIfPossible "Grue 1 hp 14"
    |> executeInputIfPossible "Grue 2 maxhp 18, xp 50"

#endif

// move to the next character or phase
let advance model =
    match model.mode with
    | Declaring ->
        let initiatives =
            [
            for name in model.game.roster do
                let init =
                    match model.game.initRolls |> Map.tryFind name with
                    | Some v -> v
                    | None ->
                        match model.game.stats |> Map.tryFind name with
                            | Some creature -> creature.initiativeMod
                            | None -> None
                        |> Option.defaultValue 0
                        |> (fun initMod -> rand 20 + initMod)
                name, init
                ]
            |> Map.ofList
        let initiative name =
            initiatives[name]
        { model with
            game = { model.game with initRolls = initiatives }
            mode =
                model.game.roster
                |> List.filter (fun name -> model.game.stats[name].actionDeclaration.IsSome)
                |> List.sortByDescending initiative
                |> Executing
            }
    | Executing [] -> { model with mode = Declaring; game = { model.game with initRolls = Map.empty } }
    | Executing (_::rest) -> { model with mode = Executing rest }

let update msg (model: Model.d) =
    match msg with
    | ReviseInput input -> { model with input = input }
    | SubmitInput when model.input = "ok" ->
        { model with input = "" } |> advance
    | SubmitInput -> model |> Model.executeInputIfPossible model.input
    | ExecuteCommand cmd -> Model.executeIfPossible model cmd
    | ToggleHelp showHelp -> { model with showHelp = showHelp }
    | ToggleBestiary showBestiary -> { model with showBestiary = showBestiary }
    | ToggleLog showLog -> { model with showLog = showLog }

open UI.Components

module Getters =
    open Domain.Ribbit

    let getAllNames (model:Model.d) =
        model.game.roster
    let get name getter (model:Model.d) = model.game.stats[name] |> getter
    let tryGetRibbit name (prop: Property<_>) (model:Model.d) =
        model.game |> Game.Getters.tryGetRibbit name prop

open Getters
open Game.Properties

let view (model: Model.d) dispatch =
    let setCommand txt =
        (ReviseInput txt) |> dispatch
    let table = Html.table [
        textHeaders ["Name"; "Type"; "Actions"; "Notes"; "XP earned"; "HP"]
        Html.tbody [
            for (Name name) as name' in getAllNames model do
                let isSelected = match model.mode with Executing (h::_) when h = name' -> true | _ -> false
                class' Html.tr (if isSelected then "currentTurn" else "") [
                    let get f = model |> get name' f
                    let type1 = get templateType
                    textCell $"{name}"
                    textCell (sprintf "(%s)" <| match get templateType with Some (Name v) -> v | None -> "PC")
                    let action =
                        let initMod =
                            match get initiativeMod with
                            | Some v -> Some v
                            | None ->
                                get templateType
                                |> Option.bind (fun t -> model.game.bestiary |> Map.tryFind t)
                                |> Option.bind (fun def -> def.initiativeMod)
                            |> Option.map (fun v -> $"%+i{v}")
                        let verb =
                            let willAct = match model.mode with | Declaring -> true | Executing haventActed -> haventActed |> List.contains name'
                            if willAct then "will" else "did"
                        match get actionDeclaration, (model.game.initRolls |> Map.tryFind name'), initMod with
                        | Some (Game.Action action), Some init, Some initMod ->
                            $"{init}: {name} {verb} {action} ({initMod})"
                        | Some (Game.Action action), Some init, None ->
                            $"{init}: {name} {verb} {action}"
                        | Some (Game.Action action), None, Some initMod ->
                            $"{name} {verb} {action} ({initMod})"
                        | Some (Game.Action action), None, None ->
                            $"{name} {verb} {action}"
                        | None, _, Some initMod when model.mode = Declaring ->
                            $"({initMod})"
                        | None, _, None when model.mode = Declaring ->
                            $""
                        | _ -> $"{name} does nothing"

                    let clickableText (txt: string) msg =
                        Html.td [prop.text txt; prop.onDoubleClick (fun _ -> setCommand msg)]

                    clickableText action $"{name} will "
                    clickableText (System.String.Join(";", get notes)) $"{name}: "
                    textCell $"{get xpEarned} XP earned"
                    let remainingHP =
                        match tryGetRibbit name Domain.Ribbit.Operations.hpP model |> Option.defaultValue 0, tryGetRibbit name Domain.Ribbit.Operations.damageTakenP model  |> Option.defaultValue 0 with
                        | hp, dmg when dmg = 0 -> $"{hp} HP"
                        | hp, dmg when dmg > hp -> $"{hp - dmg}/{hp} HP (dead)"
                        | hp, dmg -> $"{hp - dmg}/{hp} HP"
                    textCell remainingHP
                    ]
                ]
            ]
    let inputPanel =
        class' Html.div "inputPanel" [
            let label =
                match model.mode with
                | Declaring -> "Declaring"
                | Executing (Name h::_) -> $"{h}'s turn"
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
        Html.div [
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
                    for KeyValue(name, type1) in model.game.bestiary do
                        Html.tr [
                            match model.mode with
                            | Executing (h::_) when h = name ->
                                prop.className "currentTurn"
                            | _ -> ()
                            prop.children [
                                textCell (match name with Name name -> name)
                                let onNumber ctor valueCtor value = ctor(name, valueCtor value) |> ExecuteCommand |> dispatch
                                let hp = tryGetRibbit name.extract Domain.Ribbit.Operations.hpP model |> Option.map toString |> Option.defaultValue ""
                                editableNumberCell hp (onNumber Game.DeclareHP HP)
                                editableNumberCell (match type1.xp with Some (XP v) -> v.ToString() | None -> "") (onNumber Game.DeclareXP XP)
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
    class' Html.div "dev" [
        withHelp model.showHelp helpText (ToggleHelp >> dispatch) [
            table
            inputPanel
            errors
            CollapsibleSection.render bestiary
            CollapsibleSection.render log
            ]
        ]
