[<AutoOpen>]
module Common

let flip f x y = f y x
let random = System.Random()
let rand x = 1 + random.Next x
let thunk v _ = v
let thunk1 f arg _ = f arg
let thunk2 f arg1 arg2 _ = f arg1 arg2
let thunk3 f arg1 arg2 arg3 _ = f arg1 arg2 arg3
let tuple2 x y = x,y
let matchfail v = sprintf "No match found for %A. This is a bug." v |> invalidOp
let ignoreM (_, monad) = (), monad
exception BugException of msg: string
/// Placeholder while we're doing type-focused development, before implementation
let notImpl v = failwith $"Not implemented yet. Email Max if you want this feature. {v}"
let shouldntHappen arg =
    $"This shouldn't ever happen. If it does there's a bug. Details: {arg}" |> BugException |> raise
let inline breakHere() = System.Diagnostics.Debugger.Break()

type Any = obj
let viaAny<'t>() = box<'t>, unbox<'t>

let memoize f =
    let mutable cache = Map.empty
    fun x ->
        match cache |> Map.tryFind x with
        | Some v -> v
        | None ->
            let v = f x
            cache <- cache |> Map.add x v
            v

let emptyString = System.String.Empty
let toString x = x.ToString()
let betweenInclusive a b n = min a b <= n && n <= max a b
/// invoke f without requiring parens
let inv f = f()
let chooseRandom (options: _ seq) =
    options |> Seq.skip (random.Next (Seq.length options)) |> Seq.head
let chooseRandomExponentialDecay (acceptRate: float) fallback (options: _ seq) =
    let rec recur = function
    | option::rest ->
        if random.NextDouble() < acceptRate then option
        else recur rest
    // fall back if nothing else was accepted
    | [] -> options |> fallback
    recur (options |> List.ofSeq)
let chooseWeightedRandom (options: (int * 't) seq) =
    let choices = [
        for weight, item in options do
            yield! List.init weight (thunk item)
        ]
    chooseRandom choices

// iterate a mutable value
let iter (data: byref<_>) f =
    data <- f data
    data
/// iter and ignore the result
let iteri (data: byref<_>) f = data <- f data

let shuffleCopy =
    let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp
    fun a ->
        let a = Array.map id a // make a copy
        a |> Array.iteri (fun i _ -> swap a i (random.Next(i, Array.length a)))
        a // return the copy

module Tuple2 =
    let create x y = x,y
    let mapfst f (x,y) = (f x, y)
    let mapsnd f (x,y) = (x, f y)
    let ofList = function
        | [x;y] -> x,y
        | _ -> shouldntHappen "Tuple2.ofList: list must have exactly 2 elements"
    let toList (x,y) = [x;y]

module Tuple3 =
    let get1 (x,_,_) = x
    let get2 (_,x,_) = x
    let get get3 (_,_,x) = x

module Ctor =
    type AnonymousConstructor<'args, 'Type> = {
        create: 'args -> 'Type
        extract: 'Type -> 'args option
        }
    type Constructor<'args, 'Type> = {
        create: 'args -> 'Type
        extract: 'Type -> 'args option
        name: string
        }
        with
        static member (=>) (lhs: Constructor<_, 't1>, rhs: Constructor<'t1, _>) =
            {   create = rhs.create << lhs.create
                extract = rhs.extract >> Option.bind lhs.extract
                name =  $"{rhs.name} ({lhs.name})"
                }
        static member (=>) (lhs: Constructor<_, 't1>, rhs: AnonymousConstructor<'t1, _>) =
            {   create = rhs.create << lhs.create
                extract = rhs.extract >> Option.bind lhs.extract
                name = lhs.name
                }
        static member (=>) (lhs: AnonymousConstructor<_, 't1>, rhs: Constructor<'t1, _>) =
            {   create = rhs.create << lhs.create
                extract = rhs.extract >> Option.bind lhs.extract
                name = rhs.name
                }

    let ctor(create, extract): AnonymousConstructor<_,_> = { create = create; extract = extract }
    let namedCtor(name, create, extract) = { create = create; extract = extract; name = name }

type Polymorphic<'arg, 'result> =
    abstract member Apply: 'arg -> 'result

// generic place for overloaded operations like add. Can be extended (see below).
type Ops =
    static member add(key, value, data: Map<_,_>) = data |> Map.add key value
    static member addTo (data:Map<_,_>) = fun key value -> Ops.add(key, value, data)
    static member round<[<Measure>]'u>(x: float<'u>): float<'u> = System.Math.Round(float x) |> LanguagePrimitives.FloatWithMeasure
    static member roundUp<[<Measure>]'u>(x: float<'u>): float<'u> = System.Math.Ceiling(float x) |> LanguagePrimitives.FloatWithMeasure

module String =
    let oxfordJoin = function
        | _::_::_::_rest as lst -> // length 3 or greater
            match List.rev lst with
            | last::rest ->
                sprintf "%s, and %s" (System.String.Join(", ", List.rev rest)) last
            | _ -> shouldntHappen()
        | [a;b] -> sprintf "%s and %s" a b
        | [a] -> a
        | [] -> emptyString
    let join delimiter strings = System.String.Join((delimiter: string), (strings: string seq))
    let equalsIgnoreCase lhs rhs = System.String.Equals(lhs, rhs, System.StringComparison.InvariantCultureIgnoreCase)
    let containsIgnoreCase (lhs:string) (rhs:string) = lhs.ToLowerInvariant().Contains(rhs.ToLowerInvariant()) // note: lhs.Contains(rhs, System.StringComparison.InvariantCultureIgnoreCase) does not translate to JavaScript
    let firstWord input =
        match Option.ofObj input with
        | Some(v:string) -> v.Trim().Split(' ') |> Seq.head
        | None -> input
    let trim (s:string) = s.Trim()
    let isntWhitespace s = System.String.IsNullOrWhiteSpace(s) |> not
    // turn camel casing back into words with spaces, for display to user
    let uncamel = memoize (fun (str: string) ->
        let caps = ['A'..'Z'] |> Set.ofSeq
        let lower = ['a'..'z'] |> Set.ofSeq
        let mutable spaceNeededBefore = []
        let mutable inWord = true
        for i in 1..str.Length-1 do
            match str[i] with
            | ' ' -> inWord <- false
            // When multiple caps are in a row, no spaces should be used, except before the last one if it's followed by a lowercase.
            // E.g. MySSNNumber => My SSN Number, but MySSN => My SSN not My SS N
            | letter when caps.Contains letter && inWord && ((caps.Contains str[i-1] |> not) || i+1 < str.Length && lower.Contains str[i+1])->
                spaceNeededBefore <- i::spaceNeededBefore
            | letter when System.Char.IsLetterOrDigit letter -> inWord <- true
            | _ -> ()
        let rec recur workingCopy spacesNeeded =
            match spacesNeeded with
            | [] -> workingCopy
            | index::rest ->
                recur $"{workingCopy[0..(index-1)]} {workingCopy[index..]}" rest
        recur str spaceNeededBefore)

type System.Object with
    member this.ToUncameledString() = this.ToString() |> String.uncamel

module List =
    let join delimiter (lst: _ list) =
        match lst with
        | [] | [_] -> lst
        | head::tail ->
            head :: (tail |> List.collect (fun x -> [delimiter; x]))
    let wrap v = [v]
    let ofOption = function
        | None -> []
        | Some v -> [v]
    let every f =
        List.exists (f >> not) >> not
    let includesAny values =
        List.exists (fun v -> values |> List.contains v)
    let rec tryMapFold f state lst =
        match lst with
        | [] -> Ok state
        | h::t -> match f state h with
                    | Ok state' -> tryMapFold f state' t
                    | e -> e
    let rec maxBy' f (lst: _ List) = lst |> Seq.map f |> Seq.max
    let rec minBy' f (lst: _ List) = lst |> Seq.map f |> Seq.min
    let prefixes (lst: _ List) =
        let rec recur = function
            | [] -> []
            | h::t -> t::(recur t)
        lst::(recur lst)
module Map =
    let keys (m:Map<_,_>) = m |> Seq.map(fun (KeyValue(k,_)) -> k)
    let values (m:Map<_,_>) = m |> Seq.map(fun (KeyValue(_,v)) -> v)
    let addForce key f (m: Map<_,_>) =
        match m |> Map.tryFind key with
        | Some v ->
            let v' = f v
            if v = v' then m
            else m |> Map.add key v'
        | None ->
            m |> Map.add key (f Map.empty)
    let findForce key (m: Map<_,_>) =
        m |> Map.tryFind key |> Option.defaultValue Map.empty
    let (|Lookup|_|) key map =
        map |> Map.tryFind key

// Hit tip to https://gist.github.com/jwosty/5338fce8a6691bbd9f6f
// and to https://github.com/fsprojects/FSharpx.Extras/blob/da43a30aa1b2a08c86444777a5edb03027cf5c1c/src/FSharpx.Extras/ComputationExpressions/State.fs
// although I had to fix some bugs with the while loop support.
[<AutoOpen>]
module StateChangeMonad =
    type StateChange<'state, 'retval> = ('state -> 'retval * 'state)
    open System
    type State<'T, 'State> = 'State -> 'T * 'State

    let getState = fun s -> (s,s)
    let putState s = fun _ -> ((),s)
    let eval m s = m s |> fst
    let exec m s = m s |> snd
    let empty = fun s -> ((), s)
    let bind k m = fun s -> let (a, s') = m s in (k a) s'
    module private State =
        let inline run internalState f =
            f internalState
        let get = (fun s -> s, s)
        let set v = (fun _ -> (), v)
        let map f s =
            (fun s0 ->
                let retval, state = run s0 s
                f retval, state
                )
    type StateBuilder() =
        // this version of state builder: I have concerns about tail calls and stack depth. Maybe we shouldn't build Ribbit on top of the state monad after all...
        let bind k m = fun s -> let (a, s') = m s in (k a) s'
        let zero = (fun state -> (), state)
        member this.Return x : StateChange<'s, 'a> = (fun state -> x, state)
        member this.Zero () = zero
        member this.ReturnFrom stateful = stateful
        member this.Bind (m, f) = bind f m
        member this.Combine(m1, m2) =
            bind (fun () -> m2) m1
        member this.For (seq, f: 'a -> StateChange<'s, unit>) =
            seq
            |> Seq.map f
            |> Seq.reduce (fun x1 x2 -> this.Combine(x1, x2))
        // this form of Delay means "evaluate at runtime, not monad-construction time"
        member this.Delay (f: unit -> _ -> _) = bind f zero
        member this.While (condition, body: StateChange<'s, unit>) : StateChange<_, _> =
            fun state ->
                let rec loop state =
                    if condition() then
                        State.run state body |> fun ((), state) -> loop state
                    else (), state
                loop state
    let get() state = state, state
    let getF f state = f state, state
    let transform f state =
        let state = f state
        (), state
    let transform1 f state =
        let arg1, state = f state
        arg1, state
    let set v state = (), v
    let stateChange = StateBuilder()
    // run and discard unit result
    let runNoResult state stateChangeMonad =
        let _, state' = stateChangeMonad state
        // discard result
        state'
    let run state stateChangeMonad = stateChangeMonad state

let withState initialState monad =
    let result, _ = monad initialState
    result

let toState initialState monad =
    let _, finalState = monad initialState
    finalState

// intended to be as fast as a mutable data structure like an array for most purposes
//   but to have the sharing benefits an immutable structure like a Map as long as it's
//   used single threaded. Under the hood there's a fast, mutable data reference which
//   is updated by a string of commands, and surfaced via State references which secretly
//   contain the string of commands used to create them. As long as the current mutable
//   data reference shares a "prefix" of commands with whichever State is asking for
//   the current state, we can just make a quick mutation to the shared mutable state
//   and return it.
module Delta =
    type private Seed<'t, 'msg> = Seed of initial: (unit -> 't) * update: ('msg list -> 'msg -> 't -> 't)
    type DeltaDrivenState<'t, 'msg> =
        private { seed: Seed<'t, 'msg> ; current:'t ; past: ('msg list) }
    let create(initial: (unit -> 't), update: 'msg list -> 'msg -> 't -> 't) =
        { seed = Seed(initial, update); current = initial(); past = [] }
    let deref this = this.current
    let execute msg = function
        | { seed = Seed(_, update); past = past } as this ->
            { this with past = msg::past; current = this.current |> update past msg }
    let executeM msg state = (), execute msg state
    let getM f state =
        let inner = state.current
        f inner, state
    let transformM f state =
        let inner = state.current
        let commands = f inner
        let state = commands |> List.fold (flip execute) state
        (), state
    let rewind ix state =
        // TODO: it would be possible to make this enormously more efficient with incremental updates, instead of replaying everything from the beginning every time
        let (Seed (initial, update)) = state.seed
        let history = state.past |> List.rev
        let start = create(initial, update)
        history |> List.take ix |> List.fold (flip execute) start

module SharedDelta =
    open Microsoft.FSharp.Core

    type private Seed<'t, 'msg> = Seed of initial: (unit -> 't) * update: ('msg -> 't -> 't)
    type SharedDeltaDrivenState<'t, 'msg> =
        private { seed: Seed<'t, 'msg> ; shared:(('t * 'msg list) ref) ; past: ('msg list) ; queue: ('msg list) }
    let create(initial: (unit -> 't), update: 'msg -> 't -> 't) =
        let shared = ref (initial(), [])
        { seed = Seed(initial, update); shared = shared; past = []; queue = [] }
    let (|Deref|) (cell: _ ref) = cell.Value
    // returns current state and amortized-updated State reflecting the current state. Also sets shared state = this.
    let derefM : StateChange<SharedDeltaDrivenState<'t, 'msg>, 't> = function
    | { shared = Deref(current, sharedPast); past = past; queue = [] } as this when sharedPast = past ->
        current, this
    | { seed = Seed(init, update); shared = Deref(current, sharedPast) as shared; past = past; queue = queue } as this  ->
        let apply q state =
            List.foldBack update q state
        let current', past' =
            if sharedPast = past then
                (current |> apply queue), queue@past
            else
                // apply it from the beginning. This could be made more efficient at scale if necessary, by backtracking instead of starting from the beginning.
                let combined = queue@past
                (init() |> apply combined), combined
        shared.Value <- (current', past')
        current', { this with shared = shared; past = past'; queue = [] }
    let execute msg = function
    | { SharedDeltaDrivenState.queue = queue } as this ->
        { this with queue = msg::queue }
    let executeM msg state = (), execute msg state
    let getM f state =
        let inner, state = derefM state
        f inner, state
    let transformM f state =
        let inner, state = derefM state
        let commands = f inner
        let state = commands |> List.fold (flip execute) state
        (), state

module ResizeArray =
    let (|Lookup|_|) id (rows: _ ResizeArray) =
        if rows.Count > id then rows[id] |> Some else None

module FastList =
    type 't d = { reversedOrder: 't list; randomAccess: Map<int, 't>; mutable cached: 't list option  }
        with
        member lst.Add value = { reversedOrder = value::lst.reversedOrder; randomAccess = lst.randomAccess |> Map.add lst.reversedOrder.Length value; cached = None }
        // addM = monadic return value, return new index + new monad (new FastList). Used for doing things like linking to the newly-created list item.
        member lst.AddM value =
            let ix = lst.reversedOrder.Length
            ix, { reversedOrder = value::lst.reversedOrder; randomAccess = lst.randomAccess |> Map.add ix value; cached = None }
        member lst.inOrder() =
            match lst.cached with
            | Some l -> l
            | None ->
                let rev = lst.reversedOrder |> List.rev
                lst.cached <- Some rev
                rev
        member lst.Item with get ix = lst.randomAccess[ix]
        member lst.Length = lst.reversedOrder.Length

    let fresh() = { reversedOrder = []; randomAccess = Map.empty; cached = None }
    let add value (lst: 't d) = lst.Add value
    // addM = monadic return value, return new index + new monad (new FastList). Used for doing things like linking to the newly-created list item.
    let addM value (lst: 't d) = lst.AddM value
    let ofSeq values = values |> Seq.fold (flip add) (fresh())
    let length this = this.reversedOrder.Length

module Trie =
    // reader function is the key to the whole trie
    // reader takes a trie node and returns a value for that node, and an optional list of children of that node
    let read nav ixs trie =
        let rec recur ixs node =
            match nav node, ixs with
            | (value, _), [] ->
                Some value
            | (value, Some children), ix::rest ->
                let ix = if ix < 0 then List.length children + ix else ix // -1 means "last", etc.
                if 0 > ix || ix >= List.length children then
                    None // ignore out-of-bounds indexes
                else
                    recur rest children[ix]
            | (value, None), _ -> // ignore unusable indexes
                None
        recur ixs trie

    let replace nav navUpdate targetNodeUpdate ixs trie =
        let rec recur ixs node =
            match nav node, ixs with
            | (value, _), [] ->
                node |> targetNodeUpdate
            | (value, Some children), ix::rest ->
                let ix = if ix >= 0 then ix else List.length children + ix
                if 0 > ix || ix >= List.length children then
                    node // treat unusable indexes as no-op
                else
                    let updatedChild = (recur rest children[ix])
                    node |> navUpdate (ix, updatedChild)
            | (value, None), _ -> // treat unusable indexes as no-op
                node // treat unusable indexes as no-op
        recur ixs trie

let inline trace v =
#if DEBUG
    printfn "Trace: %A" v
#endif
    v

// log for dev purposes, for when exceptions aren't quite enough context
let inline devLog v =
#if DEBUG
    printfn "%s" v
#endif
    ()


