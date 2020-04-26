[<AutoOpen>]
module Common

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
let betweenInclusive a b n = min a b <= n && n <= max a b

let chooseRandom (options: _ seq) =
    options |> Seq.skip (random.Next (Seq.length options)) |> Seq.head

module Optics =
    type 't LensValue = Return of 't | Ignore
    type 't LensInput = 't -> 't LensValue // in practice will either be a reader or a writer, analogous to Id or Const
    type LensOutput<'state> = 'state -> LensValue<'state>
    type PrismOutput<'state> = 'state -> LensValue<'state>
    type Lens<'state, 'value> = ('value LensInput -> LensOutput<'state>)
    type Prism<'state, 'value> = (('value option) LensInput -> PrismOutput<'state>)
    type Impl =
        static member compose(outerLens: Lens<_,_>, innerLens: Lens<_,_>): Lens<_,_> =
            (innerLens >> outerLens)
        static member read(lens: Lens<'state, 'value>, state: 'state): 'value =
            let mutable retval = Unchecked.defaultof<_>
            lens (fun v -> retval <- v; Ignore) state |> ignore
            retval
        static member readP(prism: Prism<'state, 'value>, state: 'state): 'value option =
            let mutable retval = None
            prism (fun v -> retval <- v; Ignore) state |> ignore
            retval
    let inline (=>)(outer: Lens<_,_>) (inner) = Impl.compose(outer,inner)
    let inline read (l: Lens<'state,'value>) (state: 'state) : 'value =
        Impl.read(l, state)
    let inline readP (p: Prism<'state,'value>) (state: 'state) : 'value option =
        Impl.readP(p, state)
    let inline over (l: Lens<'state,'value>) (f : 'value -> 'value) (state:'state): 'state =
        match l (f >> Return) state with
        | Return v -> v
        | Ignore -> shouldntHappen()
    let inline overP (l: Prism<'state,'value>) (f : 'value -> 'value) (state:'state): 'state =
        match l (Option.map f >> Return) state with
        | Return v -> v
        | Ignore -> shouldntHappen()
    let inline write (l: Lens<'state,'value>) (value:'value) (state: 'state) : 'state =
        over l (fun _ -> value) state
    let inline writeP (l: Prism<'state,'value>) (value:'value) (state: 'state) : 'state =
        overP l (fun _ -> value) state
    let inline lens (get: 'state -> 'value) (set: 'value -> 'state -> 'state) : Lens<_,_> =
        let lens : Lens<'state, 'value> =
            fun (f:LensInput<_>) s ->
                match (get s |> f : LensValue<_>) with
                | Return v -> Return (set v s)
                | Ignore -> Ignore
        lens
    let inline prism (get: 'state -> 'value option) (set: 'value -> 'state -> 'state) : Prism<_,_> =
        let prism : Prism<'state, 'value> =
            fun (f:LensInput<_>) s ->
                match get s |> f with
                | Return (Some v) -> Return (set v s)
                | Return None -> Return s
                | Ignore -> Ignore
        prism

let oxfordJoin = function
    | _::_::_::_rest as lst -> // length 3 or greater
        match List.rev lst with
        | last::rest ->
            sprintf "%s, and %s" (System.String.Join(", ", List.rev rest)) last
        | _ -> shouldntHappen()
    | [a;b] -> sprintf "%s and %s" a b
    | [a] -> a
    | [] -> "Nothing at all!" // shouldn't happen

let shuffleCopy =
    let swap (a: _[]) x y =
            let tmp = a.[x]
            a.[x] <- a.[y]
            a.[y] <- tmp
    fun a ->
        let a = Array.map id a // make a copy
        a |> Array.iteri (fun i _ -> swap a i (random.Next(i, Array.length a)))
        a // return the copy

// Lens code based on http://www.fssnip.net/7Pk/title/Polymorphic-lenses by Vesa Karvonen
module Lens =
    type LensValue<'T> = Option<'T>
    type Lens<'InState,'ValGet,'ValSet,'OutState> = ('ValGet -> LensValue<'ValSet>) -> 'InState -> LensValue<'OutState>
    type SimpleLens<'Outer, 'Inner> = Lens<'Outer, 'Inner, 'Inner, 'Outer>
    type RecursiveOptionLens<'t> = SimpleLens<'t, 't option>
    let get (l: Lens<'InState,'ValGet,'ValSet,'OutState>) s =
        let r = ref Unchecked.defaultof<_>
        s |> l (fun a -> r := a; None) |> ignore
        !r
    let over (l: Lens<'InState,'ValGet,'ValSet,'OutState>) f =
        l (f >> Some) >> Option.get
    let set (l: Lens<'InState,'ValGet,'ValSet,'OutState>) b = over l <| fun _ -> b
    let lens get set : Lens<'InState,'ValGet,'ValSet,'OutState> =
        fun f s ->
            ((get s |> f : LensValue<_>) |> Option.map (fun f -> set f s))

let emptyString = System.String.Empty
module String =
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
