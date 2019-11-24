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
open Domain
open Domain.Prelude
open Domain.Properties
open Domain.Dice
open Domain.Commands
type Command = Domain.Commands.Command
let inline dice x = Domain.Dice.Dice(x)

#if INTERACTIVE
let parseFail ((args: ParseArgs), _) = failwithf "Could not parse '%s'" args.input
#else
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input
#endif

type ParsingResult = Literal of Command | Match of (Command -> bool)

let whatever = Match(function _ -> true)
[<Tests>]
let tests = testList "ribbit" [
    let uiCommandExamplars = [
        "d20+7", Literal (Evaluate(Roll (Binary(dice(1, 20), Plus, Modifier 7)))), None
        "add Eladriel", Literal (AddRow("Eladriel")), None
        "Eladriel loses d6 HP", Literal(ChangeProperty([0, "HP"], Negate(Roll(dice(1,6))))), None
        "/Suddenly [d10] trolls attack!", (Literal <| Log [Text "Suddenly "; LogExpression ("d10", Roll (dice(1,10))); Text " trolls attack!"]), None
        "avg 4.att 20 6a 2d6+5", whatever, None
        "d20+6 at least 14?0:12d6", whatever, None
        "13d?8d6/2:8d6", whatever, None
        "6.4d6k3", whatever, None
        "save party1", whatever, None
        "load party1", whatever, None
        "export save maxwilson/party1", whatever, None
        "load import maxwilson/party1", whatever, None
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
            Expect.equal (parse "3d6+1") (Binary(dice(3,6), Plus, Modifier 1)) "Should understand simple basic rolls"
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
            Expect.equal (parse "3d6+Bob.STR") (Binary(dice(3,6), Plus, External(PropertyRef(0, "STR")))) "Should understand references to external things"
            Expect.equal (parse "3d6+Bob's STR") (Binary(dice(3,6), Plus, External(PropertyRef(0, "STR")))) "Should understand references to external things"
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
            let resolve = resolveSynchronously (thunk None) >> (function (Binary(Dice.Dice(3,6), Plus, Modifier 1)) -> 11 | _ -> shouldntHappen())
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
            Expect.equal (parse "3d6+Bob.STR") (Binary(Dice.Dice(3,6), Plus, External(PropertyRef(0, "STR")))) "Should understand references to external things"
            let resolve = resolveSynchronously (function PropertyRef(0, "STR") -> Modifier 5 |> Some | _ -> None) >> (function (Dice.Binary(Dice.Dice(3,6), Plus, Modifier 5)) -> 9 | v -> matchfail v)
            Expect.equal (parse "3d6+Bob's STR" |> resolve) 9 "Should understand references to external things"
        ]
    testList ".exemplars"
        (uiCommandExamplars |> List.map (fun (cmd, verifier, evalResult) ->
            testCase (sprintf "Parse: %s" (cmd.Replace(".", "_"))) <| fun _ ->
                match ParseArgs.Init(cmd, adaptorOf ["Eladriel"]) with
                | Domain.Commands.Parse.Command(cmd, End) ->
                    match verifier with
                    | Literal c' -> Expect.equal cmd c' "Didn't parse correctly"
                    | Match f -> Expect.isTrue (f cmd) "Didn't parse correctly"
                | v -> parseFail v
            ))
    testCase "Changing HP" <| fun _ ->
        let m = Domain.fresh
        let exec txt (model: Domain.Model) =
            let names: string seq = model.roster |> Data.SymmetricMap.values
            let adaptor : RosterAdaptor = {
                    isValidNamePrefix = fun prefix -> names |> Seq.exists (fun n -> n.StartsWith prefix)
                    tryNamePrefix = fun prefix -> model.roster |> Data.SymmetricMap.toSeq |> Seq.choose (fun (id, name) -> if name.StartsWith prefix then Some id else None) |> List.ofSeq
                    tryId = flip Data.SymmetricMap.tryFind model.roster
                    tryName = flip Data.SymmetricMap.tryFindValue model.roster
                    }
            match ParseArgs.Init(txt, adaptor) with
            | Domain.Commands.Parse.Command(cmd, End) ->
                Domain.execute model txt cmd
            | v -> parseFail v
        let thenExec txt (_, model) = exec txt model
        let get (eventId, model: Model) =
            match model.eventLog.rows.[eventId].status with
            | Resolved v -> v
            | Blocked -> Tests.failtest "Expected result to be available synchronously"
        Expect.equal (m |> exec "add John" |> thenExec "John has 10 HP" |> thenExec "John.HP" |> get) (Some (Number 10)) "John didn't gain the right number of HP"
    ]
