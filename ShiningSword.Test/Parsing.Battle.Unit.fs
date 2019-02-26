module Parsing.Battle.Unit
open Model.Types.Roll
open Model.Dice.Roll
open Xunit
open Model.Types
open Packrat
open Common
open Model.Dice

let DieInputs =
  [
    "4d6", Dice(4,6), Some "4d6"
    "((4d6))", Dice(4,6), Some "4d6"
    "d12-4", Combine(Sum, Aggregate [Dice(1,12); StaticValue -4]), Some "d12-4"
    "d12-4", d 1 12 -4, None
    "3d+2", d 3 6 +2, None
    "((4d6))+1", d 4 6 +1, None
    "4d6k3+2", Combine(Sum, Aggregate [Combine(Sum, Best(3, Repeat(4, Dice(1,6)))); StaticValue +2]), None
    "4d+  d12 + d6 +2d4", Combine(Sum, Aggregate[Dice(4,6);Dice(1,12);Dice(1,6);Dice(2,4)]), None
    "4d+d100+ 3d-4", Combine(Sum, Aggregate[Dice(4,6);Dice(1,100);Dice(3,6); StaticValue -4]), None
    "4.d8+1", Combine(Sum, Repeat(4, d 1 8 +1)), None
    "12a?d8", Branch(adv +0, [AtLeast 12, Dice(1,8)]), None
    "(d8+1)", d 1 8 +1, None
    "(d8+1)/2", Transform(d 1 8 +1, Div 2), None
    "4.3.14d?3d6+1:((3d6+1)/2)", Combine(Sum, Repeat(4, Combine(Sum, Repeat(3, Branch(disadv +0, [AtLeast 14, d 3 6 +1; Else, Transform(d 3 6 +1, Div 2)]))))), None
    "att 12 2d4+7", Branch(normal 0, [Crit, d 4 4 7; AtLeast 12, d 2 4 7]), None
    "att 12a 2d4+7", Branch(adv 0, [Crit, d 4 4 7; AtLeast 12, d 2 4 7]), None
    "att 12 +7 2d4+7", Branch(normal +7, [Crit, d 4 4 7; AtLeast 12, d 2 4 7]), None
    "12.att 12 2d4+7", Combine(Sum, Repeat(12, Branch(normal 0, [Crit, d 4 4 7; AtLeast 12, d 2 4 7]))), None
    "11a?", Branch(adv 0, [AtLeast 11, StaticValue 1]), None
    "max(3d6, 3d8)", Combine(Max, Aggregate [Dice(3,6); Dice(3,8)]), None
    "(d8+1 at least 3)?5:d4", Branch((Dice(1,8), StaticValue 1), [AtLeast 3, StaticValue 5; Else, Dice(1,4)]), None
    "(max(3.d20)+7 at most 14)?12d6:((12d6)/2)", Branch((Combine(Max, Repeat(3, Dice(1,20))), StaticValue +7), [AtMost 14, Dice(12,6); Else, Transform(Dice(12,6), Div 2)]), None
    "best 3 of 4.d6", Combine(Sum, Best(3, Repeat(4, Dice(1,6)))), None
    "d20a", Combine(Max, Repeat(2, d20)), None
    "(d20d + 7 at least 17)?", Branch(disadv 7, [AtLeast 17, StaticValue 1]), None
    "att 12d +7 2d8+4", Branch(disadv 7, [Crit, d 4 8 4; AtLeast 12, d 2 8 4]), None
    "att 12 +7a 2d8+4", Branch(adv 7, [Crit, d 4 8 4; AtLeast 12, d 2 8 4]), None
    "att 25 +4 d100", Branch(normal 4, [Crit, Dice(2,100); AtLeast 25, Dice(1,100)]), None
    "att 25 4 d100", Branch(normal 4, [Crit, Dice(2,100); AtLeast 25, Dice(1,100)]), None
    "att 21 +4d d100", Branch(disadv 4, [Crit, Dice(2,100); AtLeast 21, Dice(1,100)]), None
    "att 12 7 (2d4+7)", Branch(normal +7, [Crit, d 4 4 7; AtLeast 12, d 2 4 7]), None
    "att 18 +4a 2d8+2+d6", Branch(adv 4, [
            Crit, Combine(Sum, Aggregate[Dice(4,8); StaticValue 2; Dice(2,6)])
            AtLeast 18, Combine(Sum, Aggregate[Dice(2,8); StaticValue 2; Dice(1,6)])
            ]),
        Some "when min(d20,d20)+4 has natural 20 then 4d8+2+2d6 else when >= 18 then 2d8+2+d6"
  ]
  |> List.map (fun (input, parsedForm, renderedForm) -> [|box input; box parsedForm; box renderedForm|])
  |> Array.ofList
[<Theory>]
[<MemberData("DieInputs")>]
let DieParsingTest(txt: string, expected: Roll.Request, rendered: string option) =
  match ParseArgs.Init txt with
  | Model.Dice.Parse.Roll(roll, End) ->
    Assert.Equal(expected, roll)
    match rendered with
    | Some expectedOutput ->
        // check that round-tripping works in some form
        Assert.Equal(expectedOutput, Model.Dice.Roll.render (Common.String.join " ") roll)
    | None -> () // no expectation, skip it
  | ParseInput.FailureAnalysis(_, analysis) ->
    failwithf "Could not parse '%s'\nSuccessful matches: %s" txt (String.join "\n" analysis)

let AggregateDieInputs =
  [
    "d20, d20", Aggregate [Dice(1,20); Dice(1,20)]
    "2.d20, d20", Aggregate [Combine(Sum, Repeat(2, Dice(1,20))); Dice(1,20)]
  ]
  |> List.map (fun (x,y) -> [|box x; box y|])
  |> Array.ofList
[<Theory>]
[<MemberData("AggregateDieInputs")>]
let AggregateParsingTest(txt: string, expected: Roll.AggregateRequest) =
  match ParseArgs.Init txt with
  | Model.Dice.Parse.Aggregate(roll, End) ->
    Assert.Equal(expected, roll)
  | ParseInput.FailureAnalysis(_, analysis) ->
    failwithf "Could not parse '%s'\nSuccessful matches: %s" txt (String.join "\n" analysis)

[<Theory>]
[<InlineData("2d6", 7., 2, 12)>]
[<InlineData("2d6+1", 8., 3, 13)>]
[<InlineData("20?100", 5, 0, 100)>]
[<InlineData("11?3d8+3", 8.25, 0, 27)>]
[<InlineData("11?3d8+3:10", 13.25, 0, 27)>]
[<InlineData("2d6/2", 3.25, 1, 6)>]
[<InlineData("4d6k3", 12.2445, 3, 18)>]
[<InlineData("att 25 +4 d100", 5.05, 0, 200)>]
let DieRollTests(txt: string, expectedAverage: float, expectedMin: int, expectedMax: int) =
  match ParseArgs.Init txt with
  | Model.Dice.Parse.Roll(roll, End) ->
    Assert.Equal(expectedAverage, Roll.mean roll)
    for _ in 1..100 do
      let v = (Roll.eval roll |> Result.getValue)
      Assert.True(betweenInclusive expectedMin expectedMax v, sprintf "Expected result between %d and %d, got %d" expectedMin expectedMax v)
  | ParseInput.FailureAnalysis(_, analysis) ->
    failwithf "Could not parse '%s'\nSuccessful matches: %s" txt (String.join "\n" analysis)

[<Fact(DisplayName = "Verify that ParseInput analyze shows successful matches")>]
let VerifyAnalyze() =
  match ParseArgs.Init "4d+  d12 + d6 +2d4z" with
  | Model.Dice.Parse.Roll(roll, End) ->
    ()
  | ParseInput.FailureAnalysis(_, analysis) ->
    Assert.Contains("<<<4d+  d12 + d6 +2d4>>>z Combine (Sum,Aggregate [Dice (4,6); Dice (1,12); Dice (1,6); Dice (2,4)])", analysis)

