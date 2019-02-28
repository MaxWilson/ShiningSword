module Common

let flip f x y = f y x
let random = System.Random()
let rand x = 1 + random.Next x
let thunk v _ = v
let thunk1 f arg _ = f arg
let thunk2 f arg1 arg2 _ = f arg1 arg2
let thunk3 f arg1 arg2 arg3 _ = f arg1 arg2 arg3
let ignore1 f _ = f()
type MatchFailException(msg) = inherit System.InvalidOperationException(msg)
let matchfail v = sprintf "No match found for %A. This is a bug." v |> MatchFailException |> raise
let notImpl() = failwith "Not implemented yet. Email Max if you want this feature."

let chooseRandom (lst: _ []) =
    lst.[random.Next lst.Length]

let shouldntHappen _ =
    System.Diagnostics.Debugger.Break()
    failwith "This shouldn't ever happen. If it does there's a bug"

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
type LensValue<'T> = Option<'T>
type Lens<'InState,'ValGet,'ValSet,'OutState> = ('ValGet -> LensValue<'ValSet>) -> 'InState -> LensValue<'OutState>
type SimpleLens<'Outer, 'Inner> = Lens<'Outer, 'Inner, 'Inner, 'Outer>
type RecursiveOptionLens<'t> = SimpleLens<'t, 't option>
module Lens =
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

module List =
    let join delimiter (lst: _ list) =
        match lst with
        | [] | [_] -> lst
        | head::tail ->
            head :: (tail |> List.collect (fun x -> [delimiter; x]))
    let ofOption = function
        | None -> []
        | Some v -> [v]

module Fraction =
    open System.Numerics

    let expn (base' : BigInteger) power = // workaround for ** not functioning right in Fable, https://github.com/fable-compiler/Fable/issues/1517
        List.init power (thunk base') |> List.fold (*) 1I

    let ratio precision (m:BigInteger) (n:BigInteger) = (m*(expn 10I precision)/n |> float)/(float (expn 10I precision))

    type Fraction = { numerator: BigInteger; denominator: BigInteger }
    let create n m = { numerator = n; denominator = m }
    let toFloat { numerator = n; denominator = m } = ratio 3 n m
    let toPercent { numerator = n; denominator = m } = ratio 1 (n*100I) m

module Tuple =
    let mapfst f (x,y) = (f x, y)
    let mapsnd f (x,y) = (f y, x)
    let lfst f = Lens.lens fst (fun v (_,x) -> (v,x)) f
    let lsnd f = Lens.lens snd (fun v (x,_) -> (x,v)) f
    let get1of3 (x, _, _) = x
    let get2of3 (_, x, _) = x
    let get3of3 (_, _, x) = x

module Result =
    let OkOnly = function
        | Ok(v) -> v
        | v -> matchfail v
