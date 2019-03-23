module UI.MapGen

open Common
open Model.Types
open Model.Functions
open UI.Global
open UI.Types
open UI.Types.MapGen

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React

let update (st: MapGen.State) = function
    | Tick -> st |> MapGen.iterate
    | Reset(n,m) -> MapGen.init n m

let view dispatch (state: MapGen.State) =
    let size = state.Length
    [
        button [OnClick (thunk1 dispatch (Msg.Reset(size-1, size-1)))][str "Smaller"]
        button [OnClick (thunk1 dispatch (Msg.Reset(size+1, size+1)))][str "Bigger"]
        table [] [
            for row in state do
                yield tr [] [
                    for cell in row do
                        yield
                            match cell with
                            | None -> td[Style[Width "20px"]][br[]]
                            | Some v ->
                                td[Style [Width "20px"; BackgroundColor v]][br[]]
                    ]
            ]
        button [OnClick (thunk1 dispatch Msg.Tick)][str "Tick"]
        ]
