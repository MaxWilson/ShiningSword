module App.Types

open Global
open Interaction
open Model.Types

type Msg = NewModal of Operation<Query, string> | UpdateModal of Operation<Query, string> | CloseModal

type Model = {
    modalDialogs: Operation<Query, string> list
    }

