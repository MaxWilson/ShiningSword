module App.Types

open Global
open Interaction

type Msg = NewModal of Operation<string, string> | CloseModal

type Page =
    | StringOperation of Operation<string, string>

type Model = {
    modalDialog: Operation<string, string> option
    stack: Page list
    }

type OldMsg =
  | CounterMsg of Counter.Types.Msg
  | HomeMsg of Home.Types.Msg

type OldModel = {
    currentPage: OldPage
    counter: Counter.Types.Model
    home: Home.Types.Model
  }
