module App.Types

open Global
open Interaction
open Model.Types

type Msg = NewModal of Operation<Query, string> | UpdateModal of Operation<Query, string> | CloseModal

type Model = {
    modalDialogs: Operation<Query, string> list
    }

type OldMsg =
  | CounterMsg of Counter.Types.Msg
  | HomeMsg of Home.Types.Msg

type OldModel = {
    currentPage: OldPage
    counter: Counter.Types.Model
    home: Home.Types.Model
  }
