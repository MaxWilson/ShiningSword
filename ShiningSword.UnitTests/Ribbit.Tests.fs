module Ribbit

#if INTERACTIVE
#I """..\src"""
#load "Common.fs"
#load "Data.fs"
#load "Abstractions\Parsing.fs"
#load "Domain\Prelude.fs"
#load "Domain\Properties.fs"
#load "Domain\Dice.fs"
#load "Domain\Commands.fs"
#load "Domain\Domain.fs"
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

#if INTERACTIVE
let parseFail ((args: ParseArgs), _) = failwithf "Could not parse '%s'" args.input
module Tests =
    let failtest = failwith
    let failtestf = failwithf
#else
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input
#endif

type ParsingResult = IO of IOCommand | Literal of Executable | Match of (Executable -> bool)

let whatever = Match(function _ -> true)

let m = Domain.fresh
let exec txt (model: Domain.Model) =
    match Domain.tryParseCommand model txt with
    | Some cmd ->
        Domain.execute model cmd
    | None -> Tests.failtestf "Could not parse %s" txt
let parse txt (model: Domain.Model) =
    match Domain.tryParseCommand model txt with
    | Some cmd -> cmd
    | None -> Tests.failtestf "Could not parse %s" txt
let thenExec txt (_, model) = exec txt model
let thenParse txt (_, model) = parse txt model
let get (eventId, model: Model) =
    match model.eventLog.rows.[eventId] with
    | Ready v -> v
    | Awaiting _ -> Tests.failtest "Expected result to be available synchronously"
#if INTERACTIVE
m |> exec "add John" |> thenParse "John.HP = 10"
m |> exec "add John" |> thenExec "John.HP = 10"
#endif
[<Tests>]
let tests = testList "ribbit" [
    let uiCommandExamplars = [
        "John.HP", whatever, Some(["add John"; "John has 10 HP"], (Some (Number 10)))
        "John.HP+7", whatever, Some(["add John"; "John has 10 HP"], (Some (Number 17)))
        "Eladriel loses 10 HP", whatever, Some(["Eladriel.HP = 40"], None)
        "d20+7", Literal (Evaluate(Roll (Binary(Dice(1, 20), Plus, Modifier 7)))), None
        "add Eladriel", Literal (AddRow("Eladriel")), None
        "Eladriel.HP = 10", Literal(SetProperty([1, "HP"], Expression.Literal(Number 10))), None
        "Eladriel loses d6 HP", Literal(ChangeProperty([1, "HP"], Negate(Roll(Dice(1,6))))), None
        "/Suddenly [d10] trolls attack!", (Literal <| Log [Text "Suddenly "; LogExpression ("d10", Roll (Dice(1,10))); Text " trolls attack!"]), None
        "avg 4.att 20 6a 2d6+5", whatever, None
        "d20+6 at least 14?0:12d6", whatever, None
        "13d?8d6/2:8d6", whatever, None
        "6.4d6k3", whatever, None
        "save party1", IO(Save("party1", false)), None
        "load party1", IO(Load("party1", false)), None
        "export save maxwilson/party1", IO(Save("party1", true)), None
        "load import maxwilson/party1", IO(Load("party1", true)), None
        ]

    // simple adaptor, maps list of names to ids for ease of testing
    let adaptorOf names: RosterAdaptor =
        let names = "globalPlaceholder" :: List.ofSeq names // just a placeholder to make the first name come out to ID 1, which probably doesn't matter for now
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
            Expect.equal (parse "3d6+1") (Binary(Dice(3,6), Plus, Modifier 1)) "Should understand simple basic rolls"
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
            Expect.equal (parse "3d6+Bob.STR") (Binary(Dice(3,6), Plus, External(PropertyRef(0, "STR")))) "Should understand references to external things"
            Expect.equal (parse "3d6+Bob's STR") (Binary(Dice(3,6), Plus, External(PropertyRef(0, "STR")))) "Should understand references to external things"
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
        (uiCommandExamplars |> List.map (fun (cmdTxt, verifier, evalResult) ->
            testCase (sprintf ".Parse: %s" (cmdTxt.Replace(".", "_"))) <| fun _ ->
                match ParseArgs.Init(cmdTxt, adaptorOf ["Eladriel"]) with
                | Domain.Commands.Parse.ConsoleCommand(cmd, End) ->
                    match verifier, cmd with
                    | Literal c', DomainCommand cmd ->
                        Expect.equal cmd c' "Didn't parse correctly"
                    | Match f, DomainCommand cmd ->
                        Expect.isTrue (f cmd) "Didn't parse correctly"
                    | IO c', IOCommand cmd ->
                        Expect.equal cmd c' "Didn't parse correctly"
                    | _ ->
                        Tests.failtest "Didn't parse correctly"
                    match evalResult with
                    | None -> ()
                    | Some(setupSteps, result) ->
                        let m =
                            match setupSteps with
                            | h::rest ->
                                rest |> List.fold (fun m step -> thenExec step m) (m |> exec h) |> snd
                            | [] -> m
                        match cmd with
                        | DomainCommand cmd ->
                            let post = Domain.execute m cmd
                            match result with
                            | None -> ()
                            | Some v -> Expect.equal (get post) v "Wrong result"
                        | _ -> ()
                | v -> parseFail v
            ))
    ]
