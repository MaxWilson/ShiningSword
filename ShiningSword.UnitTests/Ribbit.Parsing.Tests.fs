module Ribbit

#if INTERACTIVE
#I ".."
#load "Common.fs"
#load "Abstractions\Parsing.fs"
#load "Domain\Dice.fs"
#endif

open Expecto
open FsCheck
open Common
open Packrat
open Domain.Dice


#if INTERACTIVE
let parseFail ((args: ParseArgs), _) = failwithf "Could not parse '%s'" args.input
#else
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input
#endif

[<Tests>]
let tests = testList "parsing" [
    testCase "Basic parsing" <| fun _ ->
        // In this context, all external references are mapped to unit ()
        let (|Term|_|) = Parse.(|Term|_|) (Parse.External.(|External|_|) ignore)
        let parse x =
            let adaptor : Parse.RosterAdaptor = {
                isValidNamePrefix = fun _ -> false
                tryNamePrefix = fun _ -> []
                tryId = fun _ -> None
                tryName = fun _ -> None
                }
            match ParseArgs.Init(x, adaptor) with
            | Term(x, End) -> x
            | v -> parseFail v
        Expect.equal (parse "3d6+1") (Sum(Dice(3,6), Modifier 1)) "Should understand simple basic rolls"
    testCase "Parse references" <| fun _ ->
        let (|Term|_|) = Parse.(|Term|_|) (Parse.External.(|External|_|) id)
        let parse x =
            let adaptor : Parse.RosterAdaptor = {
                isValidNamePrefix = "Bob".StartsWith
                tryNamePrefix = fun x -> [0]
                tryId = fun _ -> Some "Bob"
                tryName = fun _ -> Some 0
                }
            match ParseArgs.Init(x, adaptor) with
            | Term(x, End) -> x
            | v -> parseFail v
        Expect.equal (parse "3d6+Bob.STR") (Sum(Dice(3,6), External(0, "STR"))) "Should understand references to external things"
        Expect.equal (parse "3d6+Bob's STR") (Sum(Dice(3,6), External(0, "STR"))) "Should understand references to external things"
    ]
