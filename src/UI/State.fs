module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open Model.Types

let urlUpdate (parseResult: Msg option) model =
    model, []

let init parseResult =
    { modalDialogs = []; gameLength = None; name = None } |> urlUpdate parseResult

let queryInteraction = Interaction.InteractionBuilder<Query, string>()

let update msg model =
    match msg with
    | NewModal(op,state) ->
        { model with modalDialogs = (op, state) :: model.modalDialogs }, Cmd.Empty
    | UpdateModalOperation op ->
        let m =
            match model.modalDialogs with
            | (_, st)::rest -> (op, st)::rest
            | _ -> []
        { model with modalDialogs = m }, Cmd.Empty
    | UpdateModalViewModel vm ->
        let m =
            match model.modalDialogs with
            | (op, _)::rest -> (op, vm)::rest
            | _ -> []
        { model with modalDialogs = m }, Cmd.Empty
    | CloseModal ->
        let pop = function [] -> [] | _::t -> t
        { model with modalDialogs = model.modalDialogs |> pop }, Cmd.Empty
    | SetGameLength x ->
        { model with gameLength = Some x }, Cmd.Empty
    | SetName name ->
        { model with name = if not (System.String.IsNullOrWhiteSpace name) then Some name else None }, Cmd.Empty
