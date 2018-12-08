module App.Types

open Global

type OldMsg =
  | CounterMsg of Counter.Types.Msg
  | HomeMsg of Home.Types.Msg

type OldModel = {
    currentPage: OldPage
    counter: Counter.Types.Model
    home: Home.Types.Model
  }
