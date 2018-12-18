module Common

let flip f x y = f y x
let random = System.Random()
let rand x = 1 + random.Next x
let thunk v _ = v
let thunk1 f arg _ = f arg
let thunk2 f arg1 arg2 _ = f arg1 arg2
let thunk3 f arg1 arg2 arg3 _ = f arg1 arg2 arg3
let matchfail v = failwithf "No match found for %A. This is a bug." v

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

type Lens<'InState,'ValGet,'ValSet,'OutState> = ('ValGet -> Option<'ValSet>) -> 'InState -> Option<'OutState>
type SimpleLens<'Outer, 'Inner> = Lens<'Outer, 'Inner, 'Inner, 'Outer>
type RecursiveOptionLens<'t> = SimpleLens<'t, 't option>
module Lens =
  let get (l: Lens<'InState,'ValGet,'ValSet,'OutState>) s =
    let r = ref Unchecked.defaultof<_>
    s |> l (fun a -> r := a; None) |> ignore
    !r
  let over (l: Lens<'InState,'ValGet,'ValSet,'OutState>) f =
    l (f >> Some) >> function Some t -> t | _ -> failwith "Impossible"
  let set (l: Lens<'InState,'ValGet,'ValSet,'OutState>) b = over l <| fun _ -> b
  let lens get set : Lens<'InState,'ValGet,'ValSet,'OutState> =
    fun f s ->
      ((get s |> f : Option<_>) |> Option.map (fun f -> set f s))


let emptyString = System.String.Empty
module String =
  let join delimiter strings = System.String.Join((delimiter: string), (strings: string seq))
  let equalsIgnoreCase lhs rhs = System.String.Equals(lhs, rhs, System.StringComparison.InvariantCultureIgnoreCase)
  let firstWord input =
    match Option.ofObj input with
    | Some(v:string) -> v.Trim().Split(' ') |> Seq.head
    | None -> input
