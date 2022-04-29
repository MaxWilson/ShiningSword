/// A module for dynamically building interactive wizards that minimize the number of unnecessary questions they ask.
/// Key concepts:
///   DerivationRule: used to create DerivationRules
///   Choice: specific choice
///   DerivationInstance: set of choices already made or in the process of being made, against a specific set of DerivationRules
///   Summary: from instance, yield a summary of traits
///   Setting<Trait>: a list of Traits and how they were derived. DerivationRules must be stored separately, as must the summarization logic.
module DerivedTraits
open Common.Map

type Choice<'trait0, 'filterContext when 'trait0: comparison> = {
    options: 'trait0 array;
    preconditions: (('trait0 * ('trait0 Set * 'filterContext)) -> bool) option
    numberAllowed: int;
    mustBeDistinct: bool;
    elideFromDisplayAndSummary: bool;
    autopick: bool
    }

let fresh options = { options = options |> Array.ofSeq; preconditions = None; numberAllowed = 1; mustBeDistinct = false; elideFromDisplayAndSummary = false; autopick = false }
type DerivationRules<'trait0, 'ctx when 'trait0: comparison> = Map<'trait0, Choice<'trait0, 'ctx> array>
type DerivationInstance<'trait0 when 'trait0: comparison> = Map<'trait0, Map<int, int array>>
type Setting<'trait0, 'summary when 'trait0: comparison> = { instance: DerivationInstance<'trait0>; summary: 'summary }

let (==>) (trait0: 'trait0) (options: 'trait0 list) =
    trait0, fresh options
let confer (trait0: 'trait0) (options: 'trait0 list) =
    trait0, { fresh options with numberAllowed = options.Length; autopick = true }
let invisiblyConfer (trait0: 'trait0) (options: 'trait0 list) =
    trait0, { fresh options with numberAllowed = options.Length; autopick = true; elideFromDisplayAndSummary = true }

let rulesOf rules : DerivationRules<_, _> =
    let mutable derivationRules = Map.empty
    for key, choice in rules do
        match derivationRules |> Map.tryFind key with
        | None ->
            derivationRules <- derivationRules |> Map.add key [|choice|]
        | Some choices ->
            derivationRules <- derivationRules |> Map.add key (Array.append choices [|choice|])
    derivationRules

// similar to summarize but ignores the elideFromDisplayAndSummary flag. Intended for internal use such as
// implementing preconditions. This guy is complicated enough that I should probably add a unit test for it.
let collect (rules: DerivationRules<'trait0, 'ctx>) (roots: 'trait0 seq) (ctx:'ctx) (instance: DerivationInstance<'trait0>) =
    let rec seekFixedPoint filterArgs priorPoint =
        let rec recur (roots: 'trait0 seq) = [
            for root in roots do
                match rules with
                | Lookup root choices ->
                    for ix, choice in choices |> Array.mapi tuple2 do
                        let isValid =
                            match choice.preconditions with
                            | None -> thunk true
                            | Some filter -> fun (trait1: 'trait0) -> filter(trait1, filterArgs)
                        let chosenOptions =
                            match instance with
                            | _ when choice.autopick ->
                                choice.options |> Array.filter isValid
                            | Lookup root (Lookup ix decision) ->
                                let deref i = choice.options[i]
                                (instance[root][ix] |> Array.map deref |> Array.filter isValid)
                            | _ -> Array.empty
                        yield! chosenOptions
                        yield! recur chosenOptions
                | _ -> () // a root with no rules is judged irrelevant
            ]
        let retval = recur roots
        if Some retval = priorPoint then
            // we're at a fixed point: go ahead and return
            retval
        else
            seekFixedPoint (Set.ofSeq retval, ctx) (Some retval)
    seekFixedPoint (Set.empty, ctx) None

let summarize f (rules: DerivationRules<'trait0, 'ctx>) (roots: _ seq) (ctx:'ctx) (instance: DerivationInstance<'trait0>) =
    let currentTraits = instance |> collect (rules: DerivationRules<'trait0, 'ctx>) (roots: 'trait0 seq) (ctx:'ctx)
    let filterArgs = currentTraits |> Set.ofSeq, ctx
    let rec recur (roots: 'trait0 seq) = [
        for root in roots do
            match rules with
            | Lookup root choices ->
                for ix, choice in choices |> Array.mapi tuple2 do
                    let isValid =
                        match choice.preconditions with
                        | None -> thunk true
                        | Some filter -> fun (trait1: 'trait0) -> filter(trait1, filterArgs)
                    let chosenOptions =
                        match instance with
                        | _ when choice.autopick ->
                            choice.options |> Array.filter isValid
                        | Lookup root (Lookup ix decision) ->
                            let pick i = choice.options[i]
                            (instance[root][ix] |> Array.map pick |> Array.filter isValid)
                        | _ -> Array.empty
                    if choice.elideFromDisplayAndSummary = false then
                        match f(root, ix, choice, chosenOptions) with
                        | Some v ->
                            yield v
                        | None -> ()
                    yield! recur chosenOptions
            | _ -> () // a root with no rules is judged irrelevant
        ]
    recur roots

// like summarize, but ignore elides. May be redundant with collect, I'm not entirely sure.
let summarizeAll f (rules: DerivationRules<'trait0, 'ctx>) (roots: _ seq) (ctx: 'ctx) (instance: DerivationInstance<'trait0>) =
    let currentTraits = instance |> collect (rules: DerivationRules<'trait0, 'ctx>) (roots: 'trait0 seq) (ctx:'ctx)
    let filterArgs = currentTraits |> Set.ofSeq, ctx
    let rec recur (roots: 'trait0 seq) = [
        for root in roots do
            match rules with
            | Lookup root choices ->
                for ix, choice in choices |> Array.mapi tuple2 do
                    let isValid =
                        match choice.preconditions with
                        | None -> thunk true
                        | Some filter -> fun (trait1: 'trait0) -> filter(trait1, filterArgs)
                    let chosenOptions =
                        match instance with
                        | _ when choice.autopick ->
                            choice.options |> Array.filter isValid
                        | Lookup root (Lookup ix decision) ->
                            let pick i = choice.options[i]
                            (instance[root][ix] |> Array.map pick |> Array.filter isValid)
                        | _ -> Array.empty
                    match f(root, ix, choice, chosenOptions) with
                    | Some v ->
                        yield v
                    | None -> ()
                    yield! recur chosenOptions
            | _ -> () // a root with no rules is judged irrelevant
        ]
    recur roots


let describeChoiceAsText (toText: 'trait0 -> string) (head, ix, choice, decision: 'trait0 array) =
    if choice.numberAllowed = decision.Length then
        Some ($"{decision |> Array.map toText}")
    else
        $"""{head} ==> [{System.String.Join("; ", decision |> Array.map toText)}]""" |> Some

let toSetting summarize' rules roots ctx instance =
    match instance |> collect rules roots ctx with
    | traits -> { instance = instance; summary = traits |> summarize' }

let toggleTrait (rules: DerivationRules<_,_>, head, choiceIx, decisionIx) (instance: DerivationInstance<_>) : DerivationInstance<_> =
    let rule =
        match rules with
        | Lookup head rules ->
            rules[choiceIx]
        | _ -> shouldntHappen()
    instance |> Map.change head (function
        | None -> Map.ofList [choiceIx, [|decisionIx|]] |> Some
        | Some decisions ->
            let change = function
            | Some ixs ->
                let d =
                    if ixs |> Array.contains decisionIx then ixs |> Array.filter ((<>) decisionIx)
                    else
                        match Array.append [|decisionIx|] ixs with | ixs when rule.mustBeDistinct -> Array.distinct ixs | ixs -> ixs
                if rule.numberAllowed >= d.Length then d else d |> Array.take rule.numberAllowed
                |> Some
            | None -> [|decisionIx|] |> Some
            decisions |> Map.change choiceIx change |> Some
        )

let (|HasTrait|) (rules: DerivationRules<_,_>) head trait' (instance: DerivationInstance<_>) =
    match rules with
    | Lookup head choices ->
        let hasTraitSelected (ix, choice: Choice<'trait0,_>)=
            match instance with
            | Lookup head (Lookup ix decisionIxs) ->
                decisionIxs |> Array.exists (fun ix -> ix < choice.options.Length && choice.options[ix] = trait')
            | _ -> false
        choices |> Array.mapi tuple2 |> Array.exists hasTraitSelected
    | _ -> false

