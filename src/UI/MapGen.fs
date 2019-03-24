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
                do! loop (i+1) { st with cells = iterate randomNeighbor st.cells }
            }
    loop 1 st

// grow live patches, based on algorithm here: https://gamedevelopment.tutsplus.com/tutorials/generate-random-cave-levels-using-cellular-automata--gamedev-9664
let grow dispatch (st: MapGen.State) =
    let grow n m cell cells =
        let neighbors = neighbors n m st.cells
        // we'll count anything off the map as a live neighbor
        let count = (neighbors |> Seq.filter Option.isSome |> Seq.length) + (9 - neighbors.Length)
        match cell with
        | None when count >= 5 -> neighbors |> Array.ofList |> chooseRandom // birth
        | Some _ when count <= 4 -> None // starvation
        // n.b. there is no overcrowding rule! This is one reason this algorithm produces such
        // stable terrain maps.
        | _ -> cell
    { st with cells = iterate grow st.cells } |> SetState |> dispatch

// grow dead patches, based on algorithm here: https://gamedevelopment.tutsplus.com/tutorials/generate-random-cave-levels-using-cellular-automata--gamedev-9664
let trim dispatch (st: MapGen.State) =
    let trim n m cell cells =
        let neighbors = neighbors n m st.cells
        let emptyCount = 9 - (neighbors |> Seq.filter Option.isSome |> Seq.length) // edges count as non-empty
        match cell with
        | Some _ when emptyCount >= 5 -> None // birth
        | None when emptyCount <= 4 -> neighbors |> Array.ofList |> chooseRandom // starvation
        // n.b. there is no overcrowding rule! This is one reason this algorithm produces such
        // stable terrain maps.
        | _ -> cell
    { st with cells = iterate trim st.cells } |> SetState |> dispatch

let update (st: MapGen.State) = function
    | Tick -> { st with cells = st.cells |> iterate randomNeighbor }
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
                                    let icon url color = td[Style [Width "20px"; color]][img[Src url]]
                                    let blank color = td[Style [Width "20px"; color]][br[]]
                                    let color = BackgroundColor v
                                    match v with
                                    | MapGen.Brown -> blank (BackgroundColor "saddleBrown")
                                    | MapGen.Green -> icon "https://img.icons8.com/metro/26/000000/deciduous-tree.png" color
                                    | MapGen.Grey -> icon "https://img.icons8.com/ios/50/000000/mountain-filled.png" color
                                    | MapGen.Red -> icon "https://img.icons8.com/ios-glyphs/30/000000/fire-element.png" color
                                    | _ -> blank <| BackgroundColor v
                        ]
                ]
            ]
        button [OnClick (thunk1 dispatch Msg.Tick)][str "Tick"]
        button [OnClick (fun _ -> stabilize dispatch state |> ignore)][str "Stabilize"]
        button [OnClick (fun _ -> trim dispatch state)][str "Trim"]
        button [OnClick (fun _ -> grow dispatch state)][str "Grow"]
        br[]
        button [OnClick (thunk1 dispatch (Msg.Reset(size, size)))][str "Reset"]
        ]
