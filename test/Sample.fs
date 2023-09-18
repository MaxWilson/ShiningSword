module Tests

open Expecto

[<Tests>]
let tests =
  testLabel "Unit" <| testList "samples" [
    testCase "universe exists (╭ರᴥ•́)" <| fun _ ->
      let subject = true
      Expect.isTrue subject "I compute, therefore I am."
  ]
