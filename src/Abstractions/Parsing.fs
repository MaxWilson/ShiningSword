module Packrat

(* DEEP MAGIC BEGINS HERE
The following memo function allows construction of left-recursive grammars using a stateful
iterated algorithm. Don't mess with the code in this file unless you've read the computer
science papers involved and thoroughly understand them, and also have unit tests for the change
you want to make.
*)

type Id = int
type Pos = int
type ParseResult = Success of obj * Pos | Fail
type ParseArgs =
    {
        input: string
        active: Set<Pos * Id> ref
        settled: Map<(Pos * Id), ParseResult> ref
        externalContext: obj option
    }
    with
    static member Init(input, ?externalContext) : ParseInput = { input = input; active = ref Set.empty; settled = ref Map.empty; externalContext = externalContext }, 0
and ParseInput = ParseArgs * Pos
module ParseInput =
    let analyze = function
    | { settled = settled; input = input }, ix ->
        let distinguish startPos afterPos (input: string) =
            let before = input.Substring(0, startPos)
            let after = input.Substring(afterPos)
            let mid = input.Substring(startPos, afterPos - startPos)
            sprintf "%s<<<%s>>>%s" before mid after
        let results =
            [for KeyValue((startPos, id), result) in !settled do
                match result with
                | Success(v, afterPos) ->
                    yield (sprintf "%s %A" (input |> distinguish startPos afterPos) v)
                | Fail -> ()]
        "..." + input.Substring(ix), results
    let (|FailureAnalysis|) = analyze
type ParseRule<'a> = (ParseInput -> ('a * ParseInput) option)
// Convenience method for defining active patterns for external context (like property definitions) to affect parsing
let ExternalContextOf<'t> ((x, _): ParseInput) =
    match x.externalContext with
    | Some(v) -> Some (unbox<'t> v)
    | _ -> None


let nextId =
    let mutable i = 0
    fun() ->
        i <- i + 1
        i

let pack (rule: ParseRule<'t>) : ParseRule<'t> =
    let id: Id = nextId()
    let eval (input: ParseInput) =
        let ctx, (pos: Pos) = input
        let active' = ctx.active.Value
        let key = (pos, id)
        ctx.active := ctx.active.Value.Remove key // mark visited
        match ctx.settled.Value.TryFind key with
        | Some(Success(v, endpos)) ->
            Some(unbox v, ((ctx, endpos) : ParseInput)) // cache says the biggest possible match is v, ending at endpos
        | Some(Fail) ->
            None // cache says it will fail
        | None -> // nothing settled yet--we have to grow a match or failure
            let settled = ctx.settled.Value // in left recursive case, holding on to an old reference lets us "forget" unsettled results
            let active = ctx.active.Value
            ctx.active := active.Add key
            ctx.settled := settled.Add(key, Fail) // initialize intermediate set to failure to prevent infinite left-recursion
            let evalResult = rule (ctx, pos)
            let hadLeftRecursion = not <| ctx.active.Value.Contains(key) // todo: check and see if any heads grew in the same position
            ctx.active := ctx.active.Value.Remove key // Clean up after ourselves, though it shouldn't be necessary
            let grow seed settled =
                let rec grow seed settled =
                    // update the intermediate cache before re-evaluating
                    ctx.settled := settled
                    match seed, (rule (ctx, pos)) with // we just had our first success--try to grow!
                    | None, Some(v, (_, endpos)) ->
                        grow (Some (box v, endpos)) (settled |> Map.add key (Success (box v, endpos)))
                    | Some(_, oldendpos), Some(v, (_, endpos) as rest) when endpos > oldendpos -> // we just grew, let's try growing again!
                        grow (Some (box v, endpos)) (settled |> Map.add key (Success (box v, endpos)))
                    | Some(v, endpos), _ ->
                        Some(v, endpos)
                    | None, None ->
                        None
                // we want to revert to the original "settled" before memoizing our results
                match grow seed settled with
                    | Some(v, endpos) ->
                        ctx.settled := (settled |> Map.add key (Success (v, endpos))) // remember the largest success
                        Some(unbox v, (ctx, endpos))
                    | None ->
                        ctx.settled := (settled |> Map.add key Fail) // remember the failure
                        None
            match evalResult with
            | None ->
                if hadLeftRecursion then
                    // since left recursion happened, we use our original "settled" as a start set, ignoring all the intermediate results already
                    // in ctx.settled, because using them could cause false negatives on parse recognition
                    grow None (settled |> Map.add key Fail)
                else
                    // no left recursion, so we can take all the intermediate results in ctx.settled instead of undoing back to settled
                    ctx.settled := ctx.settled.Value |> Map.add key Fail // remember the failure
                    None
            | Some(v, ((ctx, outpos) as output)) ->
                if hadLeftRecursion then
                    // since left recursion happened, we use our original "settled" as a start set, ignoring all the intermediate results already
                    // in ctx.settled, because using them could cause false negatives on parse recognition
                    grow (Some(box v, outpos)) (settled |> Map.add key (Success (box v, outpos)))
                else
                    ctx.settled := ctx.settled.Value |> Map.add key (Success (box v, outpos)) // remember the success
                    Some(v, output)
    eval // return eval function

let packrec parseRule =
    // work around issue w/ recursive data structures without requiring #nowarn "40"
    // by leveraging local state. This lets you declare recursive active patterns. See examples
    // in Dice.fs
    let mutable f = function _ -> None
    f <- pack (parseRule (fun x -> f x))
    f

// Here's some basic parser primitives that might be useful for anything

let (|End|_|) ((ctx, ix): ParseInput) =
    if ix = ctx.input.Length then Some() else None

let (|Str|_|) (str: string) ((ctx, ix): ParseInput) =
    if ix + str.Length <= ctx.input.Length && System.String.Equals(ctx.input.Substring(ix, str.Length), str, System.StringComparison.InvariantCultureIgnoreCase) then Some(ctx, ix+str.Length) else None

// FAIL if input matches Str, otherwise return ix unchanged
let (|PrecludeStr|_|) (str: string) ((ctx, ix): ParseInput) =
    if ix + str.Length <= ctx.input.Length && System.String.Equals(ctx.input.Substring(ix, str.Length), str, System.StringComparison.InvariantCultureIgnoreCase) then None else Some(ctx, ix)

// Test but do not consume next input: returns ix unchanged iff matches str, otherwise fail
let (|LookaheadStr|_|) (str: string) ((ctx, ix): ParseInput) =
    if ix + str.Length <= ctx.input.Length && System.String.Equals(ctx.input.Substring(ix, str.Length), str, System.StringComparison.InvariantCultureIgnoreCase) then Some(ctx, ix) else None

let (|Optional|) (str: string) ((ctx, ix): ParseInput) =
    if ix + str.Length <= ctx.input.Length && System.String.Equals(ctx.input.Substring(ix, str.Length), str, System.StringComparison.InvariantCultureIgnoreCase) then (ctx, ix+str.Length) else (ctx, ix)

// set up some basic alphabets
let alpha = Set<_>['A'..'Z'] + Set<_>['a'..'z']
let numeric = Set<_>['0'..'9']
let whitespace = Set<_>[' '; '\t'; '\n'; '\r']
let arithmeticOperators = Set<_>['+'; '-']
let alphanumeric = alpha + numeric
let alphawhitespace = alpha + whitespace

let (|Char|_|) ((ctx, ix): ParseInput) =
    if ix < ctx.input.Length then
        Some(ctx.input.[ix], (ctx, ix+1))
    else None

let (|Chars|_|) alphabet ((ctx, ix): ParseInput) =
    let rec seek i =
        if i < ctx.input.Length && Set.contains ctx.input.[i] alphabet then seek (i+1)
        else i
    match seek ix with
    | endpos when endpos > ix -> Some(ctx.input.Substring(ix, endpos - ix), (ctx, endpos))
    | _ -> None

let readBetween (start: ParseInput) (finish: ParseInput) = // assumes that start and finish are both part of the same parse
    let (ctx, ix) = start
    let endIx = finish |> snd
    ctx.input.Substring(ix, (endIx - ix))

let (|CharsExcept|_|) exclusions ((ctx, ix): ParseInput) =
    let rec seek i =
        if i < ctx.input.Length && not (Set.contains ctx.input.[i] exclusions) then seek (i+1)
        else i
    match seek ix with
    | endpos when endpos > ix -> Some(ctx.input.Substring(ix, endpos - ix), (ctx, endpos))
    | _ -> None

let (|LongestSubstringWhere|_|) pred maxLength ((ctx, ix): ParseInput) =
        let start = ix
        let rec seek i biggestMatch =
                let isMatch() = ctx.input.Substring(start, i - start) |> pred
                if i <= ctx.input.Length && (i - start) <= maxLength then
                        if isMatch() then seek (i+1) (Some i)
                        else
                                // if we've already found a match, quit as soon as we fail to find another
                                if biggestMatch |> Option.isSome then biggestMatch
                                // if we haven't found one yet, keep going until we run out of inputs or exceed maxLength
                                else seek (i+1) biggestMatch
                else
                        biggestMatch
        match seek ix None with
        | Some endpos -> Some(ctx.input.Substring(ix, endpos - ix), (ctx, endpos))
        | _ -> None

let (|AnyCase|) (input: string) = input.ToLowerInvariant()

let (|Any|) ((ctx, ix): ParseInput) =
    ctx.input.Substring(ix), (ctx, ctx.input.Length)

// Optional whitespace
let (|OWS|) ((ctx, ix): ParseInput) =
    let rec seek i =
        if i < ctx.input.Length && Set.contains ctx.input.[i] whitespace then seek (i+1)
        else i
    ctx, (seek ix)
// Required whitespace
let (|WS|_|) ((ctx, ix): ParseInput) =
    let rec seek i =
        if i < ctx.input.Length && Set.contains ctx.input.[i] whitespace then seek (i+1)
        else i
    match seek ix with
    | endx when endx > ix -> Some(ctx, endx)
    | _ -> None

// Int with no WS padding on either side
let (|IntNoWhitespace|_|) = pack <| function
    | Chars numeric (v, rest) ->
        match System.Int32.TryParse(v) with
        | true, v -> Some(v, rest)
        | _ -> None
    | _ -> None

let (|Int|_|) = pack <| function
    | OWS(Chars numeric (v, OWS(rest))) ->
        match System.Int32.TryParse(v) with
        | true, v -> Some(v, rest)
        | _ -> None
    | _ -> None

let (|Word|_|) = pack <| function
    | OWS(Chars alphanumeric (v, OWS rest)) -> Some(v, rest)
    | _ -> None

// case-insensitive exact word
let (|Keyword|_|) keyword = function
    | OWS(Str keyword (OWS ctx)) -> Some ctx
    | _ -> None

let (|Words|_|) =
    packrec <| fun (|Words|_|) -> function
    | Words(_, Word(_, ((_, endix) as rest))) & (ctx, ix) ->
        // instead of just accepting the output from Word, we'll re-derive and trim it in order to preserve the interior whitespace
        let txt = ctx.input.Substring(ix, (endix - ix)).Trim()
        Some(txt, rest)
    | Word(w, rest) -> Some(w, rest)
    | _ -> None

// helper function for dynamically creating parsers from root rules. May be useful for scripting.
let parser (recognizerRoot: ParseRule<_>) txt =
    let (|Root|_|) = recognizerRoot
    match ParseArgs.Init txt with
    | Root(v, End) -> v
    | ParseInput.FailureAnalysis(_, analysis) ->
        failwithf "Could not parse '%s'\nSuccessful matches: %s" txt (System.String.Join("\n", analysis))

// helper function for dynamically creating parsers from root rules. May be useful for scripting.
let parserWithExternalContext (recognizerRoot: ParseRule<_>) ctx txt =
    let (|Root|_|) = recognizerRoot
    match ParseArgs.Init(txt, ctx) with
    | Root(v, End) -> v
    | ParseInput.FailureAnalysis(_, analysis) ->
        failwithf "Could not parse '%s'\nSuccessful matches: %s" txt (System.String.Join("\n", analysis))

