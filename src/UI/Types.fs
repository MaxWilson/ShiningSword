module UI.Types

open Global
open Interaction
open Model.Types

type BusyStatus = NotBusy | BusyWith of string
type ProgressCallback = BusyStatus -> unit
module MapGen =
    type Msg = Reset of int * int | Tick

type ViewModel = Battle | Campaign | Error of string | MapGen
type Operation = Operation<Query, string, Query * GameState>
type Model = {
    busy: BusyStatus
    modalDialogs: Operation list
    mode: ViewModel list
    game: GameState
    undo: (GameState * Operation list) option
    logSkip: int option
    }

module Battle =
    type Msg =
        | Update of Battle2.State

module Battle1 =
    type Msg = Finish of GameState

type Msg =
    | Busy of BusyStatus
    | NewMode of ViewModel
    | UpdateCurrentViewModel of ViewModel
    | EndMode of ViewModel
    | NewModal of Operation * GameState
    | UpdateModalOperation of Operation * GameState
    | CloseModal
    | UndoModal
    | LogSkip of int
    | UpdateGameState of GameState
    | Battle1Update of Battle1.Msg
    | BattleUpdate of Battle.Msg
    | MapGen of MapGen.Msg

