module App.Types

open Global
open Interaction
open Model.Types

type ViewModel = string

type Model = {
    modalDialogs: (Operation<Query, string> * ViewModel) list
    game: GameState
    }

type Msg =
    | NewModal of Operation<Query, string> * ViewModel
    | UpdateModalViewModel of ViewModel
    | UpdateModalOperation of Operation<Query, string>
    | CloseModal


