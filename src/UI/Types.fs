module App.Types

open Global
open Interaction
open Model.Types

type ViewModel = DataEntry of string | Battle | Campaign
type Operation = Operation<Query, string, Query * GameState>
type Model = {
    modalDialogs: Operation list
    viewModel: ViewModel list
    game: GameState
    undo: (GameState * Operation list) option
    logSkip: int option
    }

type Msg =
    | NewModal of Operation * GameState * ViewModel
    | UpdateCurrentViewModal of ViewModel
    | UpdateModalOperation of Operation * GameState
    | CloseModal
    | UndoModal
    | LogSkip of int

