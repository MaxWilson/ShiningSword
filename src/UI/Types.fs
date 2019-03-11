module UI.Types

open Global
open Interaction
open Model.Types

type ViewModel = Battle | Campaign | Error of string
type Operation = Operation<Query, string, Query * GameState>
type Model = {
    modalDialogs: Operation list
    mode: ViewModel list
    game: GameState
    undo: (GameState * Operation list) option
    logSkip: int option
    }

module Battle =
    type Msg =
        | Finish of GameState

type Msg =
    | NewMode of ViewModel
    | UpdateCurrentViewModel of ViewModel
    | EndMode of ViewModel
    | NewModal of Operation * GameState
    | UpdateModalOperation of Operation * GameState
    | CloseModal
    | UndoModal
    | LogSkip of int
    | UpdateGameState of GameState
    | BattleUpdate of Battle.Msg

