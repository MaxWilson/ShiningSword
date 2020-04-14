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
let notImpl() = failwith "Not implemented yet. Email Max if you want this feature."
let betweenInclusive a b n = min a b <= n && n <= max a b

let chooseRandom (lst: _ []) =
    lst.[random.Next lst.Length]

[<Sealed; AbstractClass>]
type Overloads =
    static member choose src = chooseRandom src
    static member choose src = chooseRandom (Array.ofList src)

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
