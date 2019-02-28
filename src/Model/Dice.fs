module Model.Dice

open Common
open Model.Types.RollModule
open System.Numerics
let betweenInclusive bound1 bound2 x = (min bound1 bound2) <= x && x <= (max bound1 bound2)
module Predicate =
    let eval = function
        | AtLeast rhs -> fun (r, m) -> r + m >= rhs
        | AtMost rhs -> fun (r, m) -> r + m <= rhs
        | Natural(min, max) -> fun (r, _) -> betweenInclusive min max r
        | Else -> thunk true
module Transform =
    let eval = function
        | Div x -> flip (/) x
        | Times x -> (*) x
module Result =
    let getValue (r: Result) = r.value
module AggregateResult =
    let getValue (r: AggregateResult) = r.value

// Some convenience values and functions
let d20 = Dice(1,20)
let adv bonus = Combine(Max, Repeat(2, d20)), StaticValue bonus
let disadv bonus = Combine(Min, Repeat(2, d20)), StaticValue bonus
let normal bonus = d20, StaticValue bonus
let Crit = Natural(20, 20)
let d nDice dieSize bonus = Combine(Sum, Aggregate[Dice(nDice, dieSize); StaticValue bonus])
// multiplies the dice that actually get rolled, e.g. for crits
let multiplyResultDice n roll =
    let rec mapRoll = function
        | Dice(nDice, d) -> Dice(nDice * n, d)
        | StaticValue _ as r -> r
        | Combine(op, agg) -> Combine(op, mapAgg (agg))
        | Branch(test, branches) -> Branch(test, branches |> List.map (fun (pred, roll) -> (pred, mapRoll roll)))
        | Transform(roll, t) -> Transform(mapRoll roll, t)
    and mapAgg = function
        | Aggregate rs -> Aggregate (rs |> List.map mapRoll)
        | Repeat(n, roll) -> Repeat (n, mapRoll roll)
        | Best(n, agg) -> Best(n, mapAgg agg)
    mapRoll roll
let doubleDice = multiplyResultDice 2

let rec evaluate (r: int -> int) roll =
    let toResult priors v = { Result.value = v; source = roll; sublog = priors }
    0 |> toResult []

let eval = evaluate rand // convenience method for ubiquitous case

let rec mean (roll:Request) = Fraction.ratio 4 3I 7I

let rec render (r:Model.Types.RollType) = Common.notImpl()

#nowarn "40" // suppress warning 40--reference loops are not a problem for packrat parsing
module Parse =
    open Packrat
    let rec (|SumOfSimpleRolls|_|) = pack <| function
        | SimpleRoll(roll, rest) -> Some([roll], rest)
        | _ -> None
    and (|SimpleRoll|_|) = pack <| function
        | OWS(IntNoWhitespace(n, rest)) -> Some(StaticValue n, rest)
        | _ -> None
    let (|RollsWithModifiers|_|) = pack <| function
        | SumOfSimpleRolls([v], rest) -> Some(v, rest)
        | _ -> None
    let rec (|CommaSeparatedRolls|_|) = pack <| function
        | Roll(r, rest) -> Some([r], rest)
        | _ -> None
    and (|PlusSeparatedRolls|_|) = pack <| function
        | PlusSeparatedRolls(rolls, OWS(Str "+" (OWS (Roll(r, rest))))) -> Some(rolls @ [r], rest)
        | Roll(r, rest) -> Some([r], rest)
        | _ -> None
    and (|NumericBonus|_|) = pack <| function
        | OWS(Optional "+" (Chars numeric (v, rest))) ->
            match System.Int32.TryParse(v) with
            | true, v -> Some(v, rest)
            | _ -> None
        | OWS(Str "-" (Chars numeric (v, rest))) ->
            match System.Int32.TryParse(v) with
            | true, v -> Some(-v, rest)
            | _ -> None
        | _ -> None
    and (|Advantage|_|) = function
        | Char('a', rest) -> Some rest
        | _ -> None
    and (|Disadvantage|_|) = function
        | Char('d', LookaheadStr " " rest) -> Some rest // "d 4" denotes disadvantage, "d4" does NOT
        | _ -> None
    and (|Attack|_|) = pack <| function
        // multiple shorthands for specifying advantage and disadvantage
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, NumericBonus(toHit, Advantage(WS(Roll(dmg, rest)))))) -> Some(Branch(adv toHit, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, NumericBonus(toHit, Disadvantage(WS(Roll(dmg, rest)))))) -> Some(Branch(disadv toHit, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, NumericBonus(toHit, WS (Roll(dmg, rest))))) -> Some(Branch(normal toHit, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, Advantage(Roll(dmg, rest)))) -> Some(Branch(adv 0, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, Disadvantage(Roll(dmg, rest)))) -> Some(Branch(disadv 0, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, Advantage(NumericBonus(toHit, WS(Roll(dmg, rest)))))) -> Some(Branch(adv toHit, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, Disadvantage(NumericBonus(toHit, WS(Roll(dmg, rest)))))) -> Some(Branch(disadv toHit, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | Word(AnyCase("att" | "attack"), IntNoWhitespace(ac, Roll(dmg, rest))) -> Some(Branch(normal 0, [Crit, doubleDice dmg; AtLeast ac, dmg]), rest)
        | _ -> None
    and (|TestVariable|_|) =
        let toBaseMods = function
            | Combine(Sum, AggregateRequest.Aggregate [b; mods]) -> b, mods // optimize this representation
            | r -> r, StaticValue 0
        pack <| function
        | Int(n, Str "?" rest) -> Some((normal 0, AtLeast n), rest)
        | Int(n, Str "a?" rest) -> Some((adv 0, AtLeast n), rest)
        | Int(n, Str "d?" rest) -> Some((disadv 0, AtLeast n), rest)
        | Str "(" (Roll(r, Word("at", Word("least", Int(n, Str ")?" rest))))) -> Some((r |> toBaseMods, AtLeast n), rest)
        | Str "(" (Roll(r, Word("at", Word("most", Int(n, Str ")?" rest))))) -> Some((r |> toBaseMods, AtMost n), rest)
        | _ -> None
    and (|Branch|_|) = pack <| function
        | TestVariable((tv,condition), Roll(r1, Str ":"(Roll(r2, rest)))) -> Some(Request.Branch(tv, [condition, r1; Else, r2]), rest)
        | TestVariable((tv,condition), Roll(r, rest)) -> Some(Request.Branch(tv, [condition, r]), rest)
        | TestVariable((tv,condition), rest) -> Some(Request.Branch(tv, [condition, StaticValue 1]), rest)
        | _ -> None
    and (|Repeat|_|) = pack <| function
        | Int(n, Str "." (Roll(r, rest))) -> Some(AggregateRequest.Repeat(n, r), rest)
        | Int(n, Str "x" (Roll(r, rest))) -> Some(AggregateRequest.Repeat(n, r), rest)
        | _ -> None
    and (|Aggregate|_|) = pack <| function
        | CommaSeparatedRolls(rolls, rest) when rolls.Length >= 2 -> Some(AggregateRequest.Aggregate rolls, rest)
        | _ -> None
    and (|Best|_|) = pack <| function
        | Word (AnyCase "best", (Int(n, Word(AnyCase "of", Aggregatation(rolls, rest))))) -> Some(AggregateRequest.Best(n, rolls), rest)
        | _ -> None
    and (|Aggregatation|_|) = pack <| function
        | Best(rolls, rest) -> Some(rolls, rest)
        | Repeat(r, rest) -> Some(r, rest)
        | Aggregate(r, rest) -> Some(r, rest)
        | _ -> None
    and (|Roll|_|) = pack <| function
        | Roll(r, Str "/" (Int(rhs, rest))) -> Some(Transform(r, Div rhs), rest)
        | Roll(r, Str "*" (Int(rhs, rest))) -> Some(Transform(r, Times rhs), rest)
        | PlusSeparatedRolls(rolls, rest) when rolls.Length >= 2 -> Some(Combine(Sum,Aggregate rolls), rest)
        | Repeat(rolls, rest) -> Some(Combine(Sum, rolls), rest)
        | Best(rolls, rest) -> Some(Combine(Sum, rolls), rest)
        | Branch(r, rest) -> Some(r, rest)
        | RollsWithModifiers(r, rest) -> Some(r, rest)
        | Str "(" (OWS (Roll(r, OWS(Str ")" rest)))) -> Some(r, rest)
        | Word(AnyCase("max"), Str "(" (Aggregatation(rolls, (Str ")" rest)))) ->
            Some(Combine(Max, rolls), rest)
        | Word(AnyCase("min"), Str "(" (Aggregatation(rolls, (Str ")" rest)))) ->
            Some(Combine(Min, rolls), rest)
        | Attack(roll, rest) -> Some(roll, rest)
        | _ -> None
