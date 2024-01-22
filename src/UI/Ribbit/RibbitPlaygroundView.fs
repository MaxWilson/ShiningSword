module UI.Ribbit.PlaygroundView
open Ribbit
open Feliz
open Feliz.UseElmish

type DataKey = string
type Value = Text of string | Number of int
type DataBank = Map<DataKey, Value>
type TODO = Unit
type Model = { data: DataBank; definitions: TODO }
    with
    static member fresh = { data = Map.empty; definitions = () }

type Msg = TODO

let init _ = Model.fresh

let update msg model = model

let View() =
    let model dispatch = React.useElmishSimple init update
    Html.div "RibbitPlayground: placeholder"