module POCs

open Expecto
open Swensen.Unquote

open POC

[<Tests>]
let CQRSTests() = testLabel "POCs" <| testList "CQRS" [
    testCase "Number test" <| fun () ->
        let test1 = CQRS.Create(0, fun f n -> f n)
        test <@ test1.State = 0 @>
        let zeroish = test1.Checkpoint()
        test1.Execute ((+) 3)
        test <@ test1.State = 3 @>
        let threeish= test1.Checkpoint()
        test1.Execute ((*) 2)
        test <@ test1.State = 6 @>
        let sixish = test1.Checkpoint()
        test1.Rewind threeish
        test <@ test1.State = 3 @>
        test1.Rewind zeroish
        test <@ test1.State = 0 @>
        test1.FastForward sixish
        test <@ test1.State = 6 @>
        test1.Rewind threeish
        test1.Rewind threeish // should be idempotent
        test <@ test1.State = 3 @>
        test1.Execute ((+) 4)
        test1.Execute ((-) 15)
        test <@ test1.State = 8 @>
        raises <@ test1.FastForward sixish @> // should throw
    ptestCase "Nested events" <| fun () ->
        ()
    ]
