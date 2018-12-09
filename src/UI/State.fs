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
    { modalDialogs = []; gameLength = None } |> urlUpdate parseResult

let queryInteraction = Interaction.InteractionBuilder<Query, string>()
let rec game i : Interaction.Eventual<_,_,_> = queryInteraction {
    let! keepGoing = Model.Operations.Query.confirm "Want to keep going?"
    if keepGoing then
        let! rest = game (1+i)
        return rest
    else
        return i
    }

let update msg model =
    match msg with
    | NewModal op ->
        { model with modalDialogs = op :: model.modalDialogs }, Cmd.Empty
    | UpdateModal op ->
        { model with modalDialogs = op :: model.modalDialogs.Tail }, Cmd.Empty
    | CloseModal ->
        let pop = function [] -> [] | _::t -> t
        { model with modalDialogs = model.modalDialogs |> pop }, Cmd.Empty
    | SetGameLength x ->
        { model with gameLength = Some x }, Cmd.Empty
