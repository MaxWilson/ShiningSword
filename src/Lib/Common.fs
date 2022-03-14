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
let matchfail v = sprintf "No match found for %A. This is a bug." v |> invalidOp
/// Placeholder while we're doing type-focused development, before implementation
let notImpl() = failwith "Not implemented yet. Email Max if you want this feature."
let shouldntHappen _ =
    System.Diagnostics.Debugger.Break()
    failwith "This shouldn't ever happen. If it does there's a bug."
let emptyString = System.String.Empty
let betweenInclusive a b n = min a b <= n && n <= max a b
/// invoke f without requiring parens
let inv f = f()
let chooseRandom (options: _ seq) =
    options |> Seq.skip (random.Next (Seq.length options)) |> Seq.head
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
[<AutoOpen>]
module StateMonad =
    type StateChange<'state, 'retval> = ('state -> 'retval * 'state)
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
        member this.Zero () = (fun state -> (), state)
        member this.Return x = (fun state -> x, state)
        member this.Bind (m, f) =
            (fun state ->
                let x, state = m state
                f x state)
        member this.Combine(m1: StateChange<'s, 'a>, m2: StateChange<'s, 'b>) =
            (fun state ->
                let _, state = State.run state m1
                State.run state m2)
        member this.For (seq, f: 'a -> StateChange<'s, 'b>) =
            seq
            |> Seq.map f
            |> Seq.reduce (fun x1 x2 -> this.Combine(x1, x2))
        member this.Delay f : StateChange<'a, 's> = f()
        member this.While (condition, body) =
            if condition() then this.Combine(body, this.While(condition, body))
            else this.Zero()
    let get() state = state, state
    let transform f state =
        let state = f state
        (), state
    let transform1 f state =
        let arg1, state = f state
        arg1, state
    let set v state = (), v
    let state = StateBuilder()
    // run and discard unit result
    let runNoResult state m =
        let (), state' = m state
        // discard result
        state'

let withState initialState monad =
    let result, _ = monad initialState
    result

let toState initialState monad =
    let _, finalState = monad initialState
    finalState
