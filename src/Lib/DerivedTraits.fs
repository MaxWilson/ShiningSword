/// A module for dynamically building interactive wizards that minimize the number of unnecessary questions they ask.
/// Key concepts:
///   DerivationRule: used to create DerivationRules
///   Choice: specific choice
///   DerivationInstance: set of choices already made or in the process of being made, against a specific set of DerivationRules
///   Summary: from instance, yield a summary of traits
///   Setting<Trait>: a list of Traits and how they were derived. DerivationRules must be stored separately, as must the summarization logic.
module DerivedTraits

#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#load "Optics.fs"
#load "Common.fs"
#endif

type Choice<'trait0> =
    { options: 'trait0 list; numberAllowed: int; mustBeDistinct: bool; elideFromDisplayAndSummary: bool; autopick: bool }

let fresh options = { options = options; numberAllowed = 1; mustBeDistinct = false; elideFromDisplayAndSummary = false; autopick = false }
type DerivationRules<'trait0 when 'trait0: comparison> = Map<'trait0, Choice<'trait0> list>
type DerivationInstance<'trait0 when 'trait0: comparison> = Map<'trait0, Map<int, int list>>
type Setting<'trait0, 'summary when 'trait0: comparison> = { instance: DerivationInstance<'trait0>; summary: 'summary; validated: bool }

let (==>) (trait0: 'trait0) (options: 'trait0 list) =
    trait0, fresh options
let confer (trait0: 'trait0) (options: 'trait0 list) =
    trait0, { fresh options with numberAllowed = options.Length; autopick = true }
let invisiblyConfer (trait0: 'trait0) (options: 'trait0 list) =
    trait0, { fresh options with numberAllowed = options.Length; autopick = true; elideFromDisplayAndSummary = true }

let rulesOf rules =
    let mutable derivationRules = Map.empty
    for key, choice in rules do
        match derivationRules |> Map.tryFind key with
        | None ->
            derivationRules <- derivationRules |> Map.add key [choice]
        | Some choices ->
            derivationRules <- derivationRules |> Map.add key (choices@[choice])
    derivationRules

let summarize f (rules: DerivationRules<'trait0>) (instance: DerivationInstance<'trait0>) roots =
    let rec recur roots =
        [
            for root in roots do
                match rules with
                | Lookup root choices ->
                    for ix, choice in choices |> List.mapi tuple2 do
                        let chosenOptions =
                            match instance with
                            | _ when choice.autopick ->
                                choice.options
                            | Lookup root (Lookup ix decision) ->
                                let pick i = choice.options[i]
                                (instance[root][ix] |> List.map pick)
                            | _ -> []
                        if choice.elideFromDisplayAndSummary = false then
                            match f(root, ix, choice, chosenOptions) with
                            | Some v ->
                                yield v
                            | None -> ()
                        yield! recur chosenOptions

                | _ -> () // a root with no rules is judged irrelevant
            ]
    recur roots

let describeChoiceAsText (head, ix, choice, decision: _ list) =
    let toString x = x.ToString()
    if choice.options.Length = decision.Length then
        Some ($"{decision}")
    else
        $"""{head} can be {System.String.Join(", ", choice.options |> List.map toString)}. Current: {decision}""" |> Some
let toSetting summarize' rules roots instance =
    let mutable isValid = true
    let validate(head', ix, choice:Choice<_>, chosenOptions: _ list) =
        if choice.numberAllowed = chosenOptions.Length then
            Some chosenOptions
        else
            isValid <- false
            None
    match summarize validate rules instance roots with
    | traits -> { instance = instance; summary = traits |> List.collect id |> summarize'; validated = isValid }
// very rough helper function for FSI choosing. UI choosing will be totally different, based on summarize
let choose rules roots head value instance =
    let render(head', ix, choice, _) =
        if head = head' && (choice.options |> List.contains value) then
            Some(head, ix, choice)
        else None
    match summarize render rules instance roots with
    | [(head, ix, choice)] ->
        let choiceIx = [for i in 0..(choice.options.Length - 1) do if choice.options[i] = value then i]
        let assign = function
        | None -> Map.ofList [ix, choiceIx] |> Some
        | Some existing -> existing |> Map.add ix choiceIx |> Some
        instance |> Map.change head assign
    | _ -> instance
