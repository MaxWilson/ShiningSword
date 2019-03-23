module Model.MapGen
open Model.Types

let choices = [|None; None; None; Some MapGen.Color.Red; Some MapGen.Color.Blue; Some MapGen.Color.Green|]
let neighbors n m (st: MapGen.State) =
    [for i in [n-1..n+1] do
        for j in [m-1..m+1] do
            if not (i = n && j = m || i < 0 || i >= st.Length || j < 0 || j >= st.[0].Length) then
                yield i,j]
let next n m cell (st: MapGen.State) =
    if Common.rand 3 = 3 then
        // copy the most common neighbor
        let mostCommon =
            neighbors n m st |> List.map (fun (n,m) -> st.[n].[m])
            |> Array.ofList|> Common.chooseRandom
        mostCommon
    else cell
let iterate (st: MapGen.State) =
    st |> Array.mapi (fun n row -> row |> Array.mapi (fun m cell -> next n m cell st))
let init n m =
    let gen _ = Common.chooseRandom choices
    Array.init n (fun _ -> Array.init m gen)

