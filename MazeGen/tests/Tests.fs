module Tests
open Expecto
open Hedgehog
open Swensen.Unquote
open Domain

module Array =
    let every f arr = arr |> Array.exists (not << f) |> not
let add x y = x + y

let appTests = testList "App tests" [
    testCase "add works" <| fun _ ->
        let result = add 2 3
        Expect.equal result 5 "Result must be 5"
]

let chooseFrom inputs = 
    inputs |> List.map Gen.constant |> Gen.choice
    
[<Tests>]
let allTests = testList "All" [
    appTests
    testCase "checkMazeDimensions" <| fun _ ->
        let flip f x y = f y x 
        
        property {
            let! w = Range.linear 1 100 |> Gen.int32
            let! h = Range.linear 1 100 |> Gen.int32
            let! connected = Gen.bool
            let maze = Domain.newMaze(w,h,connected)
            // E.g. a 5 x 5 maze has 6 x 6 possible corridors if you count exits to outside. Otherwise it would be 4 x 4.
            test <@ maze.connections.Length = w + 1 && (maze.connections |> Array.every (fun (row: bool[]) -> row.Length = h + 1)) @>            
        } |> Property.check
    testCase "checkNavigationDirections" <| fun _ ->
        property {
            let! x = Range.linear 1 100 |> Gen.int32
            let! y = Range.linear 1 100 |> Gen.int32
            let reverse = function Up -> Down | Down -> Up | Left -> Right | Right -> Left
            let! direction = chooseFrom [Up;Down;Left;Right]
            let startPoint = PointCoord(x,y)
            let endPoint = moveTo direction startPoint
            test <@ (connectionTo direction startPoint) = (connectionTo (reverse direction) endPoint) @>
        } |> Property.check
]
   
[<EntryPoint>]
let main args =
    runTestsWithArgs defaultConfig args allTests
