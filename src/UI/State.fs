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
    | Some (state, vm) -> { model with game = state; viewModel = [vm]}, Cmd.Empty
    | None -> model, Cmd.Empty

let init parseResult =
    { modalDialogs = []; game = GameState.empty; undo = None; logSkip = None; viewModel = [] } |> urlUpdate parseResult

let update msg model =
    match msg with
    | NewModal(op,gameState,viewModel) ->
        { model with
            modalDialogs = op :: model.modalDialogs
            viewModel = viewModel:: model.viewModel
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
    | UpdateCurrentViewModal vm ->
        let vm =
            match model.viewModel with
            | _::rest -> vm::rest
            | _ -> []
        { model with viewModel = vm }, Cmd.Empty
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
