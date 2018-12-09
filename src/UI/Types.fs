module App.Types

open Global
open Interaction
open Model.Types

type Model = {
    modalDialogs: Operation<Query, string> list
    gameLength: int option
    }

type Msg =
    | NewModal of Operation<Query, string>
    | UpdateModal of Operation<Query, string>
    | CloseModal
    | SetGameLength of int

