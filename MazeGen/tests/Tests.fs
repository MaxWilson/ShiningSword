module Tests
open Expecto
open Hedgehog
open Swensen.Unquote

module Array =
    let every f arr = arr |> Array.exists (not << f) |> not
let add x y = x + y

let appTests = testList "App tests" [
    testCase "add works" <| fun _ ->
        let result = add 2 3
        Expect.equal result 5 "Result must be 5"
]

[<Tests>]
let allTests = testList "All" [
    appTests
    testCase "checkMazeDimensions" <| fun _ ->
        let flip f x y = f y x 
        
        property {
            let! w = Range.linear 1 100 |> Gen.int32
            let! h = Range.linear 1 100 |> Gen.int32
            where (w > 0 && h > 0)
            let! connected = Gen.bool
            let maze = Domain.newMaze(w,h,connected)
            // E.g. a 5 x 5 maze has 6 x 6 possible corridors if you count exits to outside. Otherwise it would be 4 x 4.
            test <@ maze.connections.Length = w + 1 && (maze.connections |> Array.every (fun (row: bool[]) -> row.Length = h + 1)) @>            
        } |> Property.check
]
   
[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args allTests
