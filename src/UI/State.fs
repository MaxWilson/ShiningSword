module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open Model.Types
open Model.Operations

let urlUpdate (parseResult: (GameState * ViewModel) option) model =
    match parseResult with
    | Some (state, vm) -> { model with game = state; mode = [vm]}, Cmd.Empty
    | None -> model, Cmd.Empty

let init parseResult =
    { modalDialogs = []; game = GameState.empty; undo = None; logSkip = None; mode = [] } |> urlUpdate parseResult

let update msg model =
    printfn "Cmd: %A Modes: %A" msg model.mode
    match msg with
    | NewMode(vm) -> { model with mode = vm::model.mode }, Cmd.Empty
    | EndMode(vm) ->
        match model.mode with
        | vm'::rest when vm = vm' -> { model with mode = rest }, Cmd.Empty
        | _ -> { model with mode = (Error (sprintf "Unable to pop viewModel: expected %A, found %A" vm model.mode))::model.mode }, Cmd.Empty
    | NewModal(op,gameState) ->
        { model with
            modalDialogs = op :: model.modalDialogs
            game = gameState
            undo = Some(model.game, model.modalDialogs)
            logSkip = None }, Cmd.Empty
    | UpdateModalOperation(op, gameState) ->
        let m =
            match model.modalDialogs with
            | _::rest -> op::rest
            | _ -> []
        { model with
            modalDialogs = m
            game = gameState
            undo = Some(model.game, model.modalDialogs)
            logSkip = None }, Cmd.Empty
    | UpdateCurrentViewModel vm ->
        let vm =
            match model.mode with
            | _::rest -> vm::rest
            | _ -> []
        { model with mode = vm }, Cmd.Empty
    | CloseModal ->
        let pop = function [] -> [] | _::t -> t
        { model with modalDialogs = model.modalDialogs |> pop }, Cmd.Empty
    | UndoModal ->
        // support up to one level of undo
        match model.undo with
        | Some(game, stack) ->
            { model with undo = None; game = game; modalDialogs = stack }, Cmd.Empty
        | None -> model, Cmd.Empty
    | LogSkip n -> { model with logSkip = Some n }, Cmd.Empty
