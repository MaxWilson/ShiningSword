module Model.Dice
open Common

module Roll =
    open Model.Types.Roll
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
        match roll with
        | Dice(nDice, d) -> seq { for _ in 1..nDice -> r d } |> Seq.sum |> toResult []
        | StaticValue v -> v |> toResult []
        | Combine(op, aggregate) ->
            let vResult = evaluateAggregate r aggregate
            let vs = vResult.value |> List.map Result.getValue
            match op with
            | Sum -> vs |> List.sum |> toResult vResult.sublog
            | Max -> vs |> List.max |> toResult vResult.sublog
            | Min -> vs |> List.min |> toResult vResult.sublog
        | Branch((baseRoll, modsRoll), branches) ->
            let bResult = evaluate r baseRoll
            let mResult = evaluate r modsRoll
            let b,mods = bResult.value, mResult.value
            let rec evalBranch = function
                | [] -> 0 |> toResult [bResult;mResult; evaluate r (StaticValue 0)] // fallback case
                | (condition, roll)::rest ->
                    if Predicate.eval condition (b, mods) then
                        let toResult (v:Result) =
                            v.value |> toResult [bResult;mResult;v]
                        evaluate r roll |> toResult
                    else
                        evalBranch rest
            evalBranch branches
        | Transform(roll, t) ->
            let v = evaluate r roll
            v.value |> Transform.eval t |> toResult [v]
    and evaluateAggregate (r: int -> int) aggregate =
        let toResult priors v = { AggregateResult.value = v; source = aggregate; sublog = priors }
        match aggregate with
        | Aggregate rs ->
            let priors = rs |> List.map (evaluate r)
            priors |> toResult priors // all of the priors are included in the results
        | Repeat(n, roll) ->
            let priors = [for _ in 1..n -> evaluate r roll]
            priors |> toResult priors // all of the priors are included in the results
        | Best(n, agg) ->
            let priors = (evaluateAggregate r agg).value
            let results = priors |> List.sortByDescending Result.getValue |> List.take n
            // finally, a case where some priors are excluded from results!
            results |> toResult priors

    let eval = evaluate rand // convenience method for ubiquitous case

    let distribution (roll:Request) : DistributionResult =
        let distributionOfList valuesAndCounts =
            let add distr (v: ResultValue, count:BigInteger) =
                match Map.tryFind v distr with
                | Some(count') -> Map.add v (count + count') distr
                | None -> Map.add v count distr
            valuesAndCounts |> List.fold add Map.empty
        let combine op lhs rhs =
            let distributionValues =
                // compute the probability of each value based on the probability of its inputs
                [for KeyValue(lval, lcount) in lhs do
                    for KeyValue(rval, rcount) in rhs do
                        yield (op lval rval), lcount * rcount
                    ]
            // now need to group counts by value
            distributionValues |> distributionOfList
        let sum = combine (+)
        let rec dist = function
            | Dice(n, d) ->
                let oneDie = [for i in 1..d -> i, 1I] |> Map.ofSeq
                let allDice = List.init n (thunk oneDie)
                List.reduce sum allDice
            | StaticValue v -> Map<_,_>[v, 1I]
            | Combine(op, agg) ->
                let f =
                    match op with
                    | Sum -> (+)
                    | Min -> min
                    | Max -> max
                distAgg agg |> List.reduce (combine f)
            | Branch((b,m), branches) ->
                let bdist, mdist = dist b, dist m
                // we group by rolls BEFORE computing distributions
                // because rolls are smaller and ought to be more performant,
                // and we're trying to avoid computing and summing distributions
                // more often than necessary
                let rollCounts =
                    [for KeyValue(bv, bcount) in bdist do
                        for KeyValue(mv, mcount) in mdist do
                            let testVar = (bv, mv)
                            let count = bcount * mcount
                            match branches |> List.tryFind (fun (cond, roll) -> Predicate.eval cond testVar) with
                            | Some(_, roll) ->
                                yield roll, count
                            | None ->
                                yield StaticValue 0, count
                        ]
                    |> Seq.groupBy fst
                    |> Seq.map (fun (roll, rollCounts) -> roll, (rollCounts |> Seq.sumBy snd))
                    |> Map.ofSeq
                let rollDistributionSize =
                    rollCounts |> Seq.sumBy (fun (KeyValue(_roll,vSize)) -> vSize)
                let rollDistributions =
                    rollCounts
                    |> Map.map (fun roll count ->
                                                let d = dist roll
                                                let vSize = d |> Seq.sumBy (function KeyValue(_roll, count) -> count)
                                                d, count, vSize)
                // the joint distribution size is the product of all the individual roll distributions
                let jointDistributionSize =
                    rollDistributions |> Seq.fold (fun product (KeyValue(_roll,(_, _rollFrequency, vSize))) -> vSize * product) rollDistributionSize
                [for KeyValue(roll, (dist, rollFrequency, vSize)) in rollDistributions do
                    let rollWeight = jointDistributionSize * rollFrequency / rollDistributionSize // number of total outcomes which belong to this set of rolls
                    let valueWeight = rollWeight / vSize // normalization factor so that dist will sum to rollWeight
                    for KeyValue(v, vcount) in dist do
                        yield v, valueWeight * vcount
                    ]
                |> distributionOfList
            | Transform(roll, trans) ->
                let t = Transform.eval trans
                [for KeyValue(v, count) in (dist roll) do
                        yield (t v), count
                    ]
                |> distributionOfList
        and distAgg = function
            | Aggregate rs -> rs |> List.map dist
            | Repeat(n, roll) -> [for _ in 1..n -> dist roll]
            | Best(n, agg) ->
                let dists = distAgg agg
                let sizeOf dist =
                    dist |> Seq.sumBy (fun (KeyValue(_, count)) -> count)
                let jointDistributionSize = dists |> List.fold (fun product dist -> sizeOf dist) 1I
                let bestOf n (lst: ResultValue list) =
                    lst |> List.sortDescending |> List.take n |> List.sum
                let cross combine x y =
                    seq {
                        for x' in x do
                            for y' in y do
                                yield (combine x' y') }
                let crossAll combine initial lst =
                    lst |> List.fold (fun st dist -> cross combine dist st) initial
                let combine (KeyValue(v1, vcount)) (vs, count) =
                    v1::vs, count * vcount
                crossAll combine [[], 1I] dists
                    |> Seq.map (fun (vs, count) -> (bestOf n vs), count)
                    |> List.ofSeq
                    |> distributionOfList
                    |> List.singleton // bestOf produces only one output stream
        dist roll |> DistributionResult

    let rec mean (roll:Request) =
        match roll with
        // some optimizations to improve perf
        | Combine(Sum, Repeat(n, roll)) -> float n * mean(roll)
        | Dice(n,d) -> (n * (d+1) |> float) / 2.
        | _ ->
            match distribution roll with
            | DistributionResult(dist) ->
                let count, total =
                    [for KeyValue(v, count) in dist -> count, (BigInteger v) * count]
                    |> List.reduce (fun (count, total) (count', total') -> count + count', total + total')
                Fraction.ratio 4 total count

    let render combineLines (r:Model.Types.Roll) =
        let rec renderRoll r = 
            let renderAgg = function
            | AggregateRequest.Aggregate(reqs) -> List.map renderRoll reqs
            | Repeat(n, req)-> Common.notImpl()
            | Best(n:int, req) -> Common.notImpl()
            match r with
            | Dice(1, die:int) -> sprintf "d%d" die
            | Dice(n:int, die:int) -> sprintf "%dd%d" n die
            | StaticValue n -> n.ToString()
            | Combine(Aggregation.Sum, req) ->
                let rec renderSum req =
                    match req with
                    | AggregateRequest.Aggregate(reqs) ->
                        // we would use String.join here except that we need to special-case two cases
                        let rec renderSums = function
                        | [] -> []
                        | StaticValue n::rest when n < 0 -> n.ToString()::(renderSums rest) // special case to prevent d12-4 from showing up as d12+-4
                        | StaticValue n::rest when n = 0 -> renderSums rest // filter out irrelevant +0
                        | r::rest -> "+" :: renderRoll r :: renderSums rest
                        match reqs with
                            | [] -> []
                            | [first] -> [renderRoll first]
                            | first::rest -> renderRoll first :: renderSums rest
                        |> Common.String.join ""
                    | AggregateRequest.Repeat(n, req) -> sprintf "%d x %s" n (renderRoll req)
                    | Best(k, Repeat(n, Dice(1, d))) -> sprintf "%dd%d keep %d" n d k
                    | Best(n, req) -> Common.matchfail r
                renderSum req
            | Combine(Aggregation.Min | Aggregation.Max as op, req) ->
                let rec renderAgg req =
                    match req with
                    | AggregateRequest.Aggregate(reqs) ->
                        sprintf "min(%s)" (Common.String.join "," (List.map renderRoll reqs))
                    | AggregateRequest.Repeat(n, req) ->
                        let rendered = renderRoll req
                        sprintf "min(%s)" (List.init n (thunk rendered) |> Common.String.join ",")
                    | Best(n, req) -> Common.matchfail r
                renderAgg req
            | Branch((baseRoll,mods), branches) ->
                let operand = renderRoll <| Combine(Sum, (AggregateRequest.Aggregate [baseRoll; mods]))
                let renderBranch isFirst pred req =
                    if pred = Else then
                        sprintf "otherwise %s" <| renderRoll req
                    else
                        let renderPredicate = function
                            | AtLeast n -> sprintf ">= %d" n
                            | AtMost n -> sprintf "<= %d" n
                            | Natural(n, m) ->
                                if n = m then
                                    sprintf "has natural %d" n
                                else
                                    sprintf "has natural %d-%d" n m
                            | Else as v -> Common.matchfail v
                        if isFirst then
                            sprintf "when %s %s then %s" operand (renderPredicate pred) (renderRoll req)
                        else
                            sprintf "else when %s then %s" (renderPredicate pred) (renderRoll req)
                let branches = branches |> List.mapi (fun i (pred, req) -> renderBranch (i=0) pred req)
                sprintf "%s" <| combineLines branches
            | Transform(req, transform) -> Common.matchfail r
        renderRoll r
    // Dev string: Packrat.parser Parse.(|Roll|_|) "att 18 +4a 2d8+2+d6" |> render (Common.String.join " ") |> printfn "%s"

    let rec renderExplanation (result: Result) : string =
        let renderExplanation = renderExplanation
        match result.source with
        | Combine(Sum, (Aggregate(_) | Repeat(_))) ->
            sprintf "[%s] => %d" (String.join ", " (result.sublog |> List.map renderExplanation)) result.value
        | Combine(Max, (Aggregate(_) | Repeat(_))) ->
            sprintf "max(%s) => %d" (String.join ", " (result.sublog |> List.map renderExplanation)) result.value
        | Combine(Min, (Aggregate(_) | Repeat(_))) ->
            sprintf "min(%s) => %d" (String.join ", " (result.sublog |> List.map renderExplanation)) result.value
        | Transform(roll, t) ->
            sprintf "(%s) -> %d" (renderExplanation (result.sublog.Head)) result.value
        | Branch((_,mods),_) ->
            let b,m,v = match result.sublog with [b;m;v] -> b,m,v | v -> failwithf "No match for %A" v
            let test = match mods with StaticValue 0 -> renderExplanation b | _ -> (sprintf "%s+%s" (renderExplanation b) (renderExplanation m))
            sprintf "(%s) -> %s" test (renderExplanation v)
        | _ ->
            result.value.ToString()

#nowarn "40" // suppress warning 40--reference loops are not a problem for packrat parsing
module Parse =
    open Packrat
    open Model.Types.Roll
    open Roll
    let rec (|SumOfSimpleRolls|_|) = pack <| function
        | SumOfSimpleRolls(lhs, OWS(Char('+', OWS(SimpleRoll(r, rest))))) -> Some(lhs@[r], rest)
        | SumOfSimpleRolls(lhs, OWS(Char('+', OWS(Int(n, rest))))) -> Some(lhs@[StaticValue n], rest)
        | SumOfSimpleRolls(lhs, OWS(Char('-', OWS(Int(n, rest))))) -> Some(lhs@[StaticValue -n], rest)
        | SimpleRoll(roll, rest) -> Some([roll], rest)
        | _ -> None
    and (|SimpleRoll|_|) = pack <| function
        | OWS(IntNoWhitespace(n, Char ('d', IntNoWhitespace(d, Char ('k', IntNoWhitespace(m, rest)))))) -> Some (Combine(Sum, Best(m, (Repeat(n, Dice(1, d))))), rest)
        | OWS(IntNoWhitespace(n, Char ('d', IntNoWhitespace(d, rest)))) -> Some (Dice(n, d), rest)
        | OWS(IntNoWhitespace(n, Char ('d', rest))) -> Some (Dice(n, 6), rest)
        | OWS(Char ('d', IntNoWhitespace(d, Char('a', rest)))) -> Some (Combine(Max, Repeat(2, Dice(1,d))), rest)
        | OWS(Char ('d', IntNoWhitespace(d, Char('d', rest)))) -> Some (Combine(Min, Repeat(2, Dice(1,d))), rest)
        | OWS(Char ('d', IntNoWhitespace(d, rest))) -> Some (Dice(1,d), rest)
        | OWS(IntNoWhitespace(n, rest)) -> Some(StaticValue n, rest)
        | _ -> None
    let (|RollsWithModifiers|_|) = pack <| function
        | SumOfSimpleRolls([v], rest) -> Some(v, rest)
        | SumOfSimpleRolls(rolls, rest) -> Some(Combine(Sum, Aggregate rolls), rest)
        | _ -> None
    let rec (|CommaSeparatedRolls|_|) = pack <| function
        | CommaSeparatedRolls(rolls, OWS(Str "," (OWS (Roll(r, rest))))) -> Some(rolls @ [r], rest)
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
