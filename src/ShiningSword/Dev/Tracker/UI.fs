module Dev.App.Tracker.UI

open Game.DataTypes
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz

open Fable.Core.JsInterop
open Fable.Core

module Model =
    open Packrat
    open Commands
    type GameMode = Declaring | Executing of Name list
    type d = { input: string; game: Game.d; errors: string list; showHelp: bool; mode: GameMode; initRolls: Map<Name, int> }
    let fresh = { input = ""; game = Game.fresh; errors = []; showHelp = false; mode = Declaring; initRolls = Map.empty }
    let executeIfPossible ui cmd =
        try
            { ui with input = ""; game = ui.game |> Game.update cmd; errors = [] }
        with err ->
            { ui with input = ""; errors = (err.ToString())::ui.errors }
    let executeInputIfPossible input (ui: d) =
        match ParseArgs.Init(input, ui.game) with
        | Commands (cmds, End) ->
            cmds |> List.fold executeIfPossible ui
        | Command (cmd, End) ->
            executeIfPossible ui cmd
        | _ -> ui
open Model

type Msg =
    | ReviseInput of msg: string
    | SubmitInput
    | ExecuteCommand of Game.Command
    | ToggleHelp of bool

let init initialCmd =
    Model.fresh
#if DEBUG
    // start off with some data to make hot-reloading less onerous
    |> executeInputIfPossible "add Loiosh, Boort, Hershel, Severiana"
    |> executeInputIfPossible "define Fire Giant, Giant Crocodile, Mangler, Grue"
    |> executeInputIfPossible "Fire Giant hp 162, xp 5000, init -1"
    |> executeInputIfPossible "Giant Crocodile hp 85, xp 1800"
    |> executeInputIfPossible "Mangler hp 71, xp 1800"
    |> executeInputIfPossible "Grue hp 17, xp 50"
    |> executeInputIfPossible "Boort hp 50, init +4"
    |> executeInputIfPossible "Severiana hp 44, init +1"
    |> executeInputIfPossible "add Fire Giant, Giant Crocodile"
    |> executeInputIfPossible "Boort will wildshape and attack Fire Giant"
#endif

let update msg (model: Model.d) =
    match msg with
    | ReviseInput input -> { model with input = input }
    | SubmitInput when model.input = "ok" ->
        let mode =
            match model.mode with
            | Declaring ->
                model.game.roster |> Executing
            | Executing [] -> Declaring
            | Executing (_::rest) -> Executing rest
        { model with mode = mode; input = "" }
    | SubmitInput -> model |> Model.executeInputIfPossible model.input
    | ExecuteCommand cmd -> Model.executeIfPossible model cmd
    | ToggleHelp showHelp -> { model with showHelp = showHelp }

open UI.Components

let view (model: Model.d) dispatch =
    let setCommand txt =
        (ReviseInput txt) |> dispatch
    let table = Html.table [
        textHeaders ["Name"; "Type"; "Actions"; "Notes"; "XP earned"; "HP"]
        Html.tbody [
            for (Name name) as name' in model.game.roster do
                Html.tr [
                    let creature = model.game.stats[name']
                    textCell $"{name}"
                    textCell (sprintf "(%s)" <| match creature.templateType with Some (Name v) -> v | None -> "PC")
                    let action =
                        let initMod =
                            match creature.initiativeMod with
                            | Some v -> Some v
                            | None ->
                                creature.templateType
                                |> Option.bind (fun t -> model.game.bestiary |> Map.tryFind t)
                                |> Option.bind (fun def -> def.initiativeMod)
                            |> Option.map (fun v -> $"%+i{v}")
                        match creature.actionDeclaration, initMod with
                        | Some (Game.Action action), Some initMod ->
                            $"{name} will {action} ({initMod})"
                        | Some (Game.Action action), None ->
                            $"{name} will {action}"
                        | None, Some initMod ->
                            $"({initMod})"
                        | None, None ->
                            $""
                    Html.td [prop.text action; prop.onDoubleClick (fun _ -> setCommand $"{name} will ")]
                    textCell "Notes TODO"
                    textCell $"{creature.xpEarned} XP earned"
                    textCell $"{creature.HP} HP"
                    ]
                ]
            ]
    let inputPanel =
        class' "inputPanel" Html.div [
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
        class' "bestiary" Html.table [
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
                            editableNumberCell (match type1.hp with Some (HP v) -> v.ToString() | None -> "") (onNumber Game.DeclareHP HP)
                            editableNumberCell (match type1.xp with Some (XP v) -> v.ToString() | None -> "") (onNumber Game.DeclareXP XP)
                            ]
                    ]
                ]
            ]
    let ui =
        [
            table
            inputPanel
            errors
            bestiary
            ]
        |> withHelp model.showHelp helpText (ToggleHelp >> dispatch)
    class' "dev" Html.div ui
