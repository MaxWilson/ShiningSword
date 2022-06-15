[<AutoOpen>]
module Common

open Optics
open type Optics.Operations

let flip f x y = f y x
let random = System.Random()
let rand x = 1 + random.Next x
let thunk v _ = v
let thunk1 f arg _ = f arg
let thunk2 f arg1 arg2 _ = f arg1 arg2
let thunk3 f arg1 arg2 arg3 _ = f arg1 arg2 arg3
let ignore1 f _ = f()
let tuple2 x y = x,y
let matchfail v = sprintf "No match found for %A. This is a bug." v |> invalidOp
exception BugException of msg: string
/// Placeholder while we're doing type-focused development, before implementation
let notImpl _ = failwith "Not implemented yet. Email Max if you want this feature."
let shouldntHappen arg =
    $"This shouldn't ever happen. If it does there's a bug. Details: {arg}" |> BugException |> raise
let emptyString = System.String.Empty
let toString x = x.ToString()
let betweenInclusive a b n = min a b <= n && n <= max a b
/// invoke f without requiring parens
let inv f = f()
let chooseRandom (options: _ seq) =
    options |> Seq.skip (random.Next (Seq.length options)) |> Seq.head
let chooseRandomExponentialDecay (acceptRate: float) (options: _ list) =
    let rec recur = function
    | option::rest ->
        if random.NextDouble() < acceptRate then option
        else recur rest
    // fall back to first value in list if nothing else was accepted
    | [] -> options.Head
    recur options

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

// generic place for overloaded operations like add. Can be extended (see below).
type Ops =
    static member add(key, value, data: Map<_,_>) = data |> Map.add key value
    static member addTo (data:Map<_,_>) = fun key value -> Ops.add(key, value, data)

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
    let firstWord input =
        match Option.ofObj input with
        | Some(v:string) -> v.Trim().Split(' ') |> Seq.head
        | None -> input
    let trim (s:string) = s.Trim()

module List =
    let join delimiter (lst: _ list) =
        match lst with
        | [] | [_] -> lst
        | head::tail ->
            head :: (tail |> List.collect (fun x -> [delimiter; x]))
    let ofOption = function
        | None -> []
        | Some v -> [v]
    let every f =
        List.exists (f >> not) >> not
    let rec tryMapFold f state lst =
        match lst with
        | [] -> Ok state
        | h::t -> match f state h with
                    | Ok state' -> tryMapFold f state' t
                    | e -> e

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

type IdGenerator = NextId of int
    with
    static member fresh = NextId 1
    static member newId (idGenerator_: Lens<'m, IdGenerator>) (model: 'm) =
        let mutable id' = 0
        let model' = model |> over idGenerator_ (fun (NextId id) -> id' <- id; NextId (id'+1))
        id', model'

module Queue =
    type 't d = 't list
    let append item queue = queue@[item]
    let empty = []
    let read (queue: _ d) = queue

type Ops with
    static member add(item, data: _ Queue.d) = Queue.append item data
    static member addTo (data:_ Queue.d) = fun item -> Ops.add(item, data)

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
        let (), state' = stateChangeMonad state
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
    type private Seed<'t, 'msg> = Seed of initial: (unit -> 't) * update: ('msg -> 't -> 't)
    type DeltaDrivenState<'t, 'msg> =
        private { seed: Seed<'t, 'msg> ; current:'t ; past: ('msg list) ; queue: ('msg list) }
    let create(initial: (unit -> 't), update: 'msg -> 't -> 't) =
        { seed = Seed(initial, update); current = initial(); past = []; queue = [] }
    let (|Deref|) (cell: _ ref) = cell.Value
    // returns current state and amortized-updated State reflecting the current state. Also sets shared state = this.
    let derefM : StateChange<DeltaDrivenState<'t, 'msg>, 't> = function
    | { current = current; past = past; queue = [] } as this ->
        current, this
    | { seed = Seed(init, update); current = current; past = past; queue = queue } as this  ->
        let apply q state =
            List.foldBack update q state
        let current', past' =
            (current |> apply queue), queue@past
        current', { this with current = current'; past = past'; queue = [] }
    let execute msg = function
    | { queue = queue } as this ->
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
