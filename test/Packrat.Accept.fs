module Packrat.Accept
open Expecto
open Domain.ADND.PriestSpells
open Swensen.Unquote

[<Tests>]
let tests = testList "Accept.Packrat" [
    testCase "smoke" <| fun () ->
        // make sure the basic parser is working
        test <@ defaultSpheres() <> [] @>
        test <@ defaultDeities() <> [] @>
    ]