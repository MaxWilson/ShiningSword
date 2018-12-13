module App.Types

open Global
open Interaction
open Model.Types

type ViewModel = string
type Operation = Operation<Query, string, Query * GameState>
type Model = {
    modalDialogs: (Operation * ViewModel) list
    game: GameState
    }

type Msg =
    | NewModal of Operation * GameState * ViewModel
    | UpdateModalViewModel of ViewModel
    | UpdateModalOperation of Operation * GameState
    | CloseModal


