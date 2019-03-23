module UI.MapGen

open Common
open Model.Types
open Model.MapGen
open Model.Functions
open UI.Global
open UI.Types
open UI.Types.MapGen

open Fable.PowerPack
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React

let stabilize dispatch (st: MapGen.State) =
    let rec loop i (st: MapGen.State) =
        promise {
            SetState st |> dispatch // Update the UI
            do! Promise.sleep 20
            // loop until 100 iterations pass or one of the colors goes away
            if not (i >= 100 || st.cells |> Array.collect id |> Array.filter Option.isSome |> Array.map Option.get |> Array.countBy id |> Array.length <= 2) then
                do! loop (i+1) { st with cells = iterate st.cells }
            }
    loop 1 st

let update (st: MapGen.State) = function
    | Tick -> { st with cells = st.cells |> iterate }
    | Reset(n,m) -> init n m
    | SetState st -> st

let view dispatch (state: MapGen.State) =
    let size = state.cells.Length
    [
        button [OnClick (thunk1 dispatch (Msg.Reset(size-1, size-1)))][str "Smaller"]
        button [OnClick (thunk1 dispatch (Msg.Reset(size+1, size+1)))][str "Bigger"]
        table [] [
            tbody [] [
                for row in state.cells do
                    yield tr [] [
                        for cell in row do
                            yield
                                match cell with
                                | None -> td[Style[Width "20px"]][br[]]
                                | Some v ->
                                    td[Style [Width "20px"; BackgroundColor v]][br[]]
                        ]
                ]
            ]
        button [OnClick (thunk1 dispatch Msg.Tick)][str "Tick"]
        button [OnClick (fun _ -> stabilize dispatch state |> ignore)][str "Stabilize"]
        ]
