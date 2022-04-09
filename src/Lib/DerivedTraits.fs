/// A module for dynamically building interactive wizards that minimize the number of unnecessary questions they ask.
/// Key concepts:
///   DerivationRule: used to create DerivationRules
///   Choice: specific choice
///   DerivationInstance: set of choices already made or in the process of being made, against a specific set of DerivationRules
///   Summary: from instance, yield a summary of traits
///   Setting<Trait>: a list of Traits and how they were derived. DerivationRules must be stored separately, as must the summarization logic.
module DerivedTraits

type Choice<'trait0, 'filterContext> = {
    options: 'trait0 array;
    preconditions: (('trait0 * 'filterContext) -> bool) option
    numberAllowed: int;
    mustBeDistinct: bool;
    elideFromDisplayAndSummary: bool;
    autopick: bool
    }

let fresh options = { options = options |> Array.ofSeq; preconditions = None; numberAllowed = 1; mustBeDistinct = false; elideFromDisplayAndSummary = false; autopick = false }
type DerivationRules<'trait0, 'ctx when 'trait0: comparison> = Map<'trait0, Choice<'trait0, 'ctx> array>
type DerivationInstance<'trait0 when 'trait0: comparison> = Map<'trait0, Map<int, int array>>
type Setting<'trait0, 'summary when 'trait0: comparison> = { instance: DerivationInstance<'trait0>; summary: 'summary; validated: bool }

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
let collect (rules: DerivationRules<'trait0, 'ctx>) (instance: DerivationInstance<'trait0>) (roots: 'trait0 seq) (makeContext: 'trait0 seq -> 'ctx) =
    let rec seekFixedPoint ctx priorPoint =
        let rec recur (roots: 'trait0 seq) = [
            for root in roots do
                match rules with
                | Lookup root choices ->
                    for ix, choice in choices |> Array.mapi tuple2 do
                        let chosenOptions =
                            match instance with
                            | _ when choice.autopick ->
                                choice.options
                            | Lookup root (Lookup ix decision) ->
                                let pick i =
                                    match choice.options[i], choice.preconditions with
                                    | choice, Some isValid when isValid(choice, ctx) -> Some choice
                                    | choice, None -> Some choice
                                    | _ -> None
                                (instance[root][ix] |> Array.choose pick)
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
            seekFixedPoint (makeContext retval) (Some retval)
    seekFixedPoint (makeContext roots) None

let summarize f (rules: DerivationRules<'trait0, 'ctx>) (instance: DerivationInstance<'trait0>) (roots: _ seq) =
    let rec recur (roots: 'trait0 seq) = [
        for root in roots do
            match rules with
            | Lookup root choices ->
                for ix, choice in choices |> Array.mapi tuple2 do
                    let chosenOptions =
                        match instance with
                        | _ when choice.autopick ->
                            choice.options
                        | Lookup root (Lookup ix decision) ->
                            let pick i = choice.options[i]
                            (instance[root][ix] |> Array.map pick)
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


let describeChoiceAsText (head, ix, choice, decision: _ array) =
    let toString x = x.ToString()
    if choice.options.Length = decision.Length then
        Some ($"{decision}")
    else
        $"""{head} can be {System.String.Join(", ", choice.options |> Array.map toString)}. Current: {decision}""" |> Some
let toSetting summarize' rules roots instance =
    let mutable isValid = true
    let validate(head', ix, choice:Choice<_,_>, chosenOptions: _ array) =
        if choice.numberAllowed = chosenOptions.Length then
            Some chosenOptions
        else
            isValid <- false
            None
    match summarize validate rules instance roots with
    | traits -> { instance = instance; summary = traits |> List.collect List.ofArray |> summarize'; validated = isValid }
// very rough helper function for FSI choosing. UI choosing will be totally different, based on summarize
let choose rules roots head value instance =
    let render(head', ix, choice, _) =
        if head = head' && (choice.options |> Array.contains value) then
            Some(head, ix, choice)
        else None
    match summarize render rules instance roots with
    | [(head, ix, choice)] ->
        let choiceIx = [|for i in 0..(choice.options.Length - 1) do if choice.options[i] = value then i|]
        let assign = function
        | None -> Map.ofList [ix, choiceIx] |> Some
        | Some existing -> existing |> Map.add ix choiceIx |> Some
        instance |> Map.change head assign
    | _ -> instance

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
