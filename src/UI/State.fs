module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open Model.Types
open Model.Operations

let urlUpdate (parseResult: Msg option) model =
    model, []

let init parseResult =
    { modalDialogs = []; game = GameState.empty; undo = None } |> urlUpdate parseResult

let update msg model =
    match msg with
    | NewModal(op,gameState,viewModel) ->
        { model with modalDialogs = (op, viewModel) :: model.modalDialogs; game = gameState; undo = Some(model.game, model.modalDialogs) }, Cmd.Empty
    | UpdateModalOperation(op, gameState) ->
        let m =
            match model.modalDialogs with
            | (_, st)::rest -> (op, "")::rest
            | _ -> []
        { model with modalDialogs = m; game = gameState; undo = Some(model.game, model.modalDialogs) }, Cmd.Empty
    | UpdateModalViewModel vm ->
        let m =
            match model.modalDialogs with
            | (op, _)::rest -> (op, vm)::rest
            | _ -> []
        { model with modalDialogs = m }, Cmd.Empty
    | CloseModal ->
        let pop = function [] -> [] | _::t -> t
        { model with modalDialogs = model.modalDialogs |> pop }, Cmd.Empty
    | UndoModal ->
        // support up to one level of undo
        match model.undo with
        | Some(game, stack) ->
            { model with undo = None; game = game; modalDialogs = stack }, Cmd.Empty
        | None -> model, Cmd.Empty
