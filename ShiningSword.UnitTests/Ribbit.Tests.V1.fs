// Keeping this around for now even though I'm in the middle of rewriting Ribbit
module Ribbit.V1

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
#else
open Expecto
open FsCheck
#endif

open Common
open Packrat
open Data.Functor
open Domain.Prelude
open Domain.Properties
open Domain.Dice
open Domain.Commands
open Domain

#if INTERACTIVE
let parseFail ((args: ParseArgs), _) = failwithf "Could not parse '%s'" args.input
module Tests =
    let failtest = failwith
    let failtestf = failwithf
#else
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input
#endif

type ParsingResult = IO of IOCommand | Exact of Executable | Match of (Executable -> bool)

let whatever = Match(function _ -> true)

let exec txt (model: Domain.Model) =
    match Domain.tryParseExecutable model txt with
    | Some cmd ->
        Domain.execute model cmd
    | None -> Tests.failtestf "Could not parse %s" txt
let parse txt (model: Domain.Model) =
    match Domain.tryParseExecutable model txt with
    | Some cmd -> cmd
    | None -> Tests.failtestf "Could not parse %s" txt
let thenExec txt (_, model) = exec txt model
let thenParse txt (_, model) = parse txt model
let get (eventId, model: Model) =
    match model.eventLog.rows.[eventId].status with
    | Ready v -> v
    | Awaiting _ as v -> Tests.failtestf "Expected result to be available synchronously but got %A" v
type Spec = ParseOnly | CheckValue of setup:string list * Value | CheckRolledValue of setup:string list * Value * (string*int) list
let fulfillRolls fulfiller model =
    let dice =
        model.blocking.forward
        |> Map.keys
        |> Seq.choose (
            function
            | EventRef(id) ->
                match model.eventLog |> find id |> Event.Status with
                | AwaitingRoll d -> Some (id, d)
                | _ -> None
            | _ -> None
                )
    let tryFulfill model (eventId, d:Dice<_>) =
        match fulfiller d with
        | Some n ->
            fulfillRoll eventId n model
        | None -> model
    dice |> Seq.fold tryFulfill model
let supply listOfRandoms =
    let fulfiller roll =
        let target = Dice.toString roll
        listOfRandoms |> List.tryFind (fst >> (=) target) |> Option.map snd
    fulfillRolls fulfiller
#if INTERACTIVE
let m = Domain.fresh |> exec "add Eladriel" |> snd
let roll rolls (id, model) = (id, supply rolls model)
m |> exec "add John" |> thenExec "John has 29 HP" |> thenExec "John loses 2d8 HP" |> roll ["2d8", 4]
m |> exec "Eladriel has 10 HP" |> thenExec "Eladriel loses 2d6 HP" |> roll ["2d6", 4] |> get
m |> exec "d20+7" |> roll ["1d20+7", 9] |> get
m |> exec "Eladriel gains 2d6 HP" |> thenExec "Eladriel has 10 HP" |> roll ["2d6", 4]
#endif
[<Tests>]
let tests = testList "ribbit.unit" [
    let uiCommandExamplars = [
        "John.HP", Exact(Evaluate(Ref(PropertyRef(2, "HP")))),
            CheckValue(["add John"; "John has 10 HP"], Number 10)
        "John.HP+7", Exact(Evaluate(BinaryOperation(Ref(PropertyRef(2, "HP")), Plus, Literal (Number 7)))),
            CheckValue(["add John"; "John has 10 HP"], Number 17)
        "d20+7", Exact (Evaluate(Roll (Binary(Dice(1, 20), Plus, Modifier 7)))),
            CheckRolledValue([], Number 9, ["1d20+7", 9])
        "add Eladriel", Exact (AddRow("Eladriel")),
            CheckValue ([], Nothing)
        "Eladriel.HP = 10", Exact(SetProperty([1, "HP"], Expression.Literal(Number 10))),
            CheckValue ([], Nothing)
        "Eladriel loses d6 HP", Exact(ChangeProperty([1, "HP"], Negate(Roll(Dice(1,6))))), ParseOnly
        "Eladriel loses 10 HP", whatever,
            CheckValue(["Eladriel.HP = 40"], Nothing)
        "Eladriel gains 2d6 HP", whatever,
            CheckRolledValue(["Eladriel.HP = 40"], Nothing, ["2d6", 4])
        "Eladriel.HP", whatever,
            CheckRolledValue(["Eladriel loses 2d6 HP";"Eladriel.HP = 40"], Number 36, ["2d6", 4])
        "/Suddenly [d10] trolls attack!", (Exact <| Log [Text "Suddenly "; LogExpression ("d10", Roll (Dice(1,10))); Text " trolls attack!"]), ParseOnly
        "avg 4.att 20 6a 2d6+5", whatever, ParseOnly
        "d20+6 at least 14?0:12d6", whatever, ParseOnly
        "13d?8d6/2:8d6", whatever, ParseOnly
        "6.4d6k3", whatever, ParseOnly
        "save party1", IO(Save("party1", false)), ParseOnly
        "load party1", IO(Load("party1", false)), ParseOnly
        "export save maxwilson/party1", IO(Save("maxwilson/party1", true)), ParseOnly
        "load import maxwilson/party1", IO(Load("maxwilson/party1", true)), ParseOnly
        ]

    testList "parsing" [
        testCase "Basic parsing" <| fun _ ->
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
        testCase "Parse references" <| fun _ ->
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

    testList "evaluation" [
        testCase "Basic rolls" <| fun _ ->
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
        testCase "Parse references" <| fun _ ->
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
    testList "exemplars"
        (uiCommandExamplars |> List.map (fun (cmdTxt, verifier, evalResult) ->
            testCase (sprintf ".%s: %s" (if evalResult = ParseOnly then "Parse" else "Execute") (cmdTxt.Replace(".", "_"))) <| fun _ ->
                let m = Domain.fresh |> exec "add Eladriel" |> snd
                let verify cmd =
                    match verifier, cmd with
                    | Exact c', ExecutableCommand cmd ->
                        Expect.equal cmd c' "Didn't parse correctly"
                    | Match f, ExecutableCommand cmd ->
                        Expect.isTrue (f cmd) "Didn't parse correctly"
                    | IO c', IOCommand cmd ->
                        Expect.equal cmd c' "Didn't parse correctly"
                    | _ ->
                        Tests.failtest "Didn't parse correctly"
                let checkValue randoms setupSteps m expectedResult =
                    let fulfill =
                        let fulfill = supply randoms
                        fun (arg, model) -> arg, fulfill model
                    let model =
                        match setupSteps with
                        | h::rest ->
                            rest |> List.fold (fun m step -> thenExec step m |> fulfill) (m |> exec h |> fulfill) |> snd
                        | [] -> m
                    match Domain.tryParseCommand model cmdTxt with
                    | Some (ExecutableCommand executable as cmd) ->
                        verify cmd
                        let post = Domain.execute model executable |> fulfill
                        Expect.equal (get post) expectedResult "Wrong result"
                    | _ -> ()
                match evalResult with
                | ParseOnly ->
                    match Domain.tryParseCommand m cmdTxt with
                    | Some cmd -> verify cmd
                    | None -> Tests.failtestf "Could not parse: %s" cmdTxt
                | CheckValue(setupSteps, expectedResult) ->
                    checkValue [] setupSteps m expectedResult
                | CheckRolledValue(setupSteps, expectedResult, rolls) ->
                    checkValue rolls setupSteps m expectedResult
            ))
    ]
