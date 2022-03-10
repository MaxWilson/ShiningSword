module Tests
#if FABLE_COMPILER
open Fable.Mocha
#else
open Expecto
#endif

let add x y = x + y

let appTests = testList "App tests" [
    testCase "add works" <| fun _ ->
        let result = add 2 3
        Expect.equal result 5 "Result must be 5"
]

[<Tests>]
let allTests = testList "All" [
    appTests
    testCase "deliberate failure" <| fun _ ->
        let result = add 2 3
        Expect.equal result 4 "Result must be 5"
]
   
[<EntryPoint>]
let main args =
#if FABLE_COMPILER
    Mocha.runTests allTests
#else
    runTestsWithArgs defaultConfig args allTests
#endif
