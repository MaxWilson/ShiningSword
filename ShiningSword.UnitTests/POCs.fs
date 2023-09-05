module POCs

open Swensen.Unquote
let verify = Swensen.Unquote.Assertions.test
open Expecto

open CQRS

[<Tests>]
let Tests() = testList "POCs" [
    testList "CQRS" [
        testCase "Number test" <| fun () ->
            let test1 = CQRS.Create(0, fun f n -> f n)
            verify <@ test1.State = 0 @>
            let zeroish = test1.Checkpoint()
            test1.Execute ((+) 3)
            verify <@ test1.State = 3 @>
            let threeish= test1.Checkpoint()
            test1.Execute ((*) 2)
            verify <@ test1.State = 6 @>
            let sixish = test1.Checkpoint()
            test1.Rewind threeish
            verify <@ test1.State = 3 @>
            test1.Rewind zeroish
            verify <@ test1.State = 0 @>
            test1.FastForward sixish
            verify <@ test1.State = 6 @>
            test1.Rewind threeish
            test1.Rewind threeish // should be idempotent
            verify <@ test1.State = 3 @>
            test1.Execute ((+) 4)
            test1.Execute ((-) 15)
            verify <@ test1.State = 8 @>
            raises <@ test1.FastForward sixish @> // should throw
        ]
    ]
