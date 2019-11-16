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
open Domain.Properties
open Domain.Dice
open Domain.Commands
type Command = Domain.Commands.Command
let Plus = Domain.Dice.Plus

#if INTERACTIVE
let parseFail ((args: ParseArgs), _) = failwithf "Could not parse '%s'" args.input
#else
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input
#endif

type ParsingResult = Literal of Command | Match of (Command -> bool)

[<Tests>]
let tests = testList "ribbit" [
    let uiCommandExamplars = [
        "d20+7", Literal (Evaluate("d20+7", Sample (Plus (Dice(1, 20), Modifier 7))))
        "add Eladriel", Match <| function _ -> true
        "Eladriel loses d6 HP", Match <| function _ -> true
        "/Suddenly [d10] trolls attack!", Literal <| Log [Text "Suddenly "; LogExpression ("d10", Sample (Dice(1,10))); Text " trolls attack!"]
        "avg 4.att 20 6a 2d6+5", Match <| function _ -> true
        "d20+6 at least 14?0:12d6", Match <| function _ -> true
        "13d?8d6/2:8d6", Match <| function _ -> true
        "6.4d6k3", Match <| function _ -> true
        "save party1", Match <| function _ -> true
        "load party1", Match <| function _ -> true
        "export save maxwilson/party1", Match <| function _ -> true
        "load import maxwilson/party1", Match <| function _ -> true
        ]

    // simple adaptor, maps list of names to ids for ease of testing
    let adaptorOf names: RosterAdaptor =
        let names = List.ofSeq names
        {
                    isValidNamePrefix = fun prefix -> names |> List.exists (fun (n:string) -> n.StartsWith prefix)
                    tryNamePrefix = fun prefix -> names |> List.mapi(fun i n -> (i,n)) |> List.choose (fun (i,n) -> if n.StartsWith prefix then Some i else None)
                    tryId = fun id -> if names.Length >= id then None else Some (names.[id])
                    tryName = fun name -> names |> List.tryFindIndex ((=)name)
                    }

    testList ".parsing" [
        testCase ".Basic parsing" <| fun _ ->
            // In this context, all external references are mapped to unit ()
            let (|Term|_|) = Domain.Dice.Parse.(|Term|_|) (Domain.Properties.Parse.(|PropertyReference|_|) ignore)
            let parse x =
                let adaptor : RosterAdaptor = {
                    isValidNamePrefix = fun _ -> false
                    tryNamePrefix = fun _ -> []
                    tryId = fun _ -> None
                    tryName = fun _ -> None
                    }
                match ParseArgs.Init(x, adaptor) with
                | Term(x, End) -> x
                | v -> parseFail v
            Expect.equal (parse "3d6+1") (Plus(Dice(3,6), Modifier 1)) "Should understand simple basic rolls"
        testCase ".Parse references" <| fun _ ->
            let parse x =
                let adaptor : RosterAdaptor = {
                    isValidNamePrefix = "Bob".StartsWith
                    tryNamePrefix = fun x -> [0]
                    tryId = fun _ -> Some "Bob"
                    tryName = fun _ -> Some 0
                    }
                match ParseArgs.Init(x, adaptor) with
                | Domain.Commands.Parse.Term(x, End) -> x
                | v -> parseFail v
            Expect.equal (parse "3d6+Bob.STR") (Plus(Dice(3,6), External(PropertyRef(0, "STR")))) "Should understand references to external things"
            Expect.equal (parse "3d6+Bob's STR") (Plus(Dice(3,6), External(PropertyRef(0, "STR")))) "Should understand references to external things"
        testList ".exemplars"
            (uiCommandExamplars |> List.map (fun (cmd, verifier) ->
                testCase (sprintf "Parse: %s" (cmd.Replace(".", "_"))) <| fun _ ->
                    match ParseArgs.Init(cmd, adaptorOf ["Eladriel"]) with
                    | Domain.Commands.Parse.Command(cmd, End) ->
                        match verifier with
                        | Literal c' -> Expect.equal cmd c' "Didn't parse correctly"
                        | Match f -> Expect.isTrue (f cmd) "Didn't parse correctly"
                    | v -> parseFail v
                ))
        ]

    testList ".evaluation" [
        testCase ".Basic rolls" <| fun _ ->
            // In this context, all external references are mapped to unit ()
            let (|Term|_|) = Domain.Dice.Parse.(|Term|_|) (Domain.Properties.Parse.(|PropertyReference|_|) ignore)
            let parse x =
                let adaptor : RosterAdaptor = {
                    isValidNamePrefix = fun _ -> false
                    tryNamePrefix = fun _ -> []
                    tryId = fun _ -> None
                    tryName = fun _ -> None
                    }
                match ParseArgs.Init(x, adaptor) with
                | Term(x, End) -> x
                | v -> parseFail v
            let resolve = resolveSynchronously (thunk None) >> (function (Domain.Dice.Plus(Dice(3,6), Modifier 1)) -> 11 | _ -> shouldntHappen())
            Expect.equal (parse "3d6+1" |> resolve) 11 "Should understand simple basic rolls"
        testCase ".Parse references" <| fun _ ->
            let parse x =
                let adaptor : RosterAdaptor = {
                    isValidNamePrefix = "Bob".StartsWith
                    tryNamePrefix = fun x -> [0]
                    tryId = fun _ -> Some "Bob"
                    tryName = fun _ -> Some 0
                    }
                match ParseArgs.Init(x, adaptor) with
                | Domain.Commands.Parse.Term(x, End) -> x
                | v -> parseFail v
            Expect.equal (parse "3d6+Bob.STR") (Plus(Dice(3,6), External(PropertyRef(0, "STR")))) "Should understand references to external things"
            let resolve = resolveSynchronously (function PropertyRef(0, "STR") -> Modifier 5 |> Some | _ -> None) >> (function (Domain.Dice.Plus(Dice(3,6), Modifier 5)) -> 9 | v -> matchfail v)
            Expect.equal (parse "3d6+Bob's STR" |> resolve) 9 "Should understand references to external things"
        ]
    ]
