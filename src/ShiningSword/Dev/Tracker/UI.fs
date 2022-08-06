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
    type d = { input: string; game: Game.d; errors: string list; showHelp: bool }
    let fresh = { input = ""; game = Game.fresh; errors = []; showHelp = false }
    let executeIfPossible ui cmd =
        try
            { ui with input = ""; game = ui.game |> Game.update cmd; errors = [] }
        with err ->
            { ui with input = ""; errors = (err.ToString())::ui.errors }
    let executeInputIfPossible (ui: d) =
        match ParseArgs.Init(ui.input, ui.game) with
        | Commands (cmds, End) ->
            cmds |> List.fold executeIfPossible ui
        | Command (cmd, End) ->
            executeIfPossible ui cmd
        | _ -> ui

type Msg =
    | ReviseInput of msg: string
    | SubmitInput
    | ExecuteCommand of Game.Command
    | ToggleHelp of bool

let init initialCmd = Model.fresh

let update msg (model: Model.d) =
    match msg with
    | ReviseInput input -> { model with input = input }
    | SubmitInput -> model |> Model.executeInputIfPossible
    | ExecuteCommand cmd -> Model.executeIfPossible model cmd
    | ToggleHelp showHelp -> { model with showHelp = showHelp }

open Feliz.Router
let view (model: Model.d) dispatch =
    let class' (className: string) ctor (children: ReactElement list) =
        ctor [prop.className className; prop.children children]
    class' "dev" Html.div [
        if model.showHelp then
            Html.div [
                for line in helpText.Split("\n") do
                    Html.div line
                Html.button [prop.text "OK"; prop.onClick(fun _ -> dispatch (ToggleHelp false))]
                ]
        else
            class' "header" Html.div [
                Html.a [prop.text "Help"; prop.onClick (fun _ -> dispatch (ToggleHelp (not model.showHelp)))]
                ]

            Html.table [
                Html.thead [
                    Html.tr [Html.th [prop.text "Name"]; Html.th [prop.text "Declaration"]; Html.th [prop.text "Initiative"]; Html.th [prop.text "Notes"]; Html.th [prop.text "XP earned"]; Html.th [prop.text "HP"]]
                    ]
                Html.tbody [
                    for name in model.game.roster do
                        Html.tr [
                            let creature = model.game.stats[name]
                            Html.td [prop.text (match name with Name name -> $"{name}")]
                            Html.td [prop.text "Declaration TODO"]
                            Html.td [prop.text "Initiative TODO"]
                            Html.td [prop.text "Notes TODO"]
                            Html.td [prop.text creature.xpEarned]
                            Html.td [prop.text creature.HP]
                            ]
                        ]
                    ]
            class' "inputPanel" Html.div [
                Html.div [prop.text "Your wish is my command"; prop.className "inputHeader"]
                Html.input [
                    prop.placeholder "Enter a command, e.g. define Beholder"
                    prop.autoFocus true
                    prop.valueOrDefault model.input;
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
            Html.div [
                for err in model.errors do
                    Html.div err
                ]
            class' "bestiary" Html.table [
                Html.thead [
                    Html.tr [Html.th [prop.text "Type"]; Html.th [prop.text "HP"]; Html.th [prop.text "XP reward"]]
                    ]
                Html.tbody [
                    for KeyValue(name, type1) in model.game.bestiary do
                        Html.tr [
                            Html.td [prop.text (match name with Name name -> name)]
                            Html.td [
                                Html.input [prop.valueOrDefault (match type1.hp with Some (HP v) -> v.ToString() | None -> ""); prop.onChange (fun (txt:string) -> match System.Int32.TryParse(txt) with true, hp -> dispatch (Game.DeclareHP(name, HP hp) |> ExecuteCommand))]
                                ]
                            Html.td [
                                Html.input [prop.valueOrDefault (match type1.xp with Some (XP v) -> v.ToString() | None -> ""); prop.onChange (fun (txt:string) -> match System.Int32.TryParse(txt) with true, xp -> dispatch (Game.DeclareXP(name, XP xp) |> ExecuteCommand))]
                                ]
                            ]
                        ]
                    ]
        ]
