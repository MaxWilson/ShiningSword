module Domain.Dice

open Common
open Domain

type Dice<'externalProperty> =
    | Modifier of int
    | Dice of number: int * kind: int
    | External of 'externalProperty
    | Binary of Dice<'externalProperty> * ArithmeticOperator * Dice<'externalProperty>
    | Min of Dice<'externalProperty> * Dice<'externalProperty>
    | Max of Dice<'externalProperty> * Dice<'externalProperty>

type Fulfiller<'externalProperty> = 'externalProperty -> Dice<'externalProperty> option // will attempt to resolve a structure with 'holes' in it to the same type of structure but with fewer 'holes' in it

#nowarn "40" // we're not doing anything funny at initialization-time, like calling functions in the ctor that rely on uninitialized members, so we don't need the warning
let instantiate (fulfiller:Fulfiller<_>) (dice:Dice<_>) : {| dice: Dice<_>; hasExternalReferences: bool |} =
    let mutable hasExternalReferences = false; // ready only if there are no more External references
    let rec walk =
        let replaceIfChanged v ctor lhs rhs =
            // speculative perf optimization: don't allocate new objects unless something has changed
            let lhs' = walk lhs
            let rhs' = walk rhs
            if lhs = lhs' && rhs = rhs' then v
            else ctor(lhs', rhs')
        function
        | (Modifier _ | Dice _) as n -> n
        | Binary(lhs, op, rhs) as v -> replaceIfChanged v (fun (l,r) -> Binary(l,op,r)) lhs rhs
        | Min(lhs, rhs) as v -> replaceIfChanged v Min lhs rhs
        | Max(lhs, rhs) as v -> replaceIfChanged v Max lhs rhs
        | External ref as original ->
            match fulfiller ref with
            | Some n -> n
            | _ ->
                hasExternalReferences <- true
                original
    {| dice = walk dice; hasExternalReferences = hasExternalReferences |}

let rec sample = function
    | External r -> failwithf "Bug alert! External references should have already been removed by instantiate before sampling occurs, but found reference to %A." r
    | Modifier n -> n
    | Dice(n, dSize) -> [1..n] |> List.map (thunk1 rand dSize) |> List.sum
    | Binary(d1, Plus, d2) -> (sample d1) + (sample d2)
    | Binary(d1, Minus, d2) -> (sample d1) + (sample d2)
    | Min(d1, d2) -> min (sample d1) (sample d2)
    | Max(d1, d2) -> max (sample d1) (sample d2)

let resolveSynchronously fulfiller dice =
    let rec loop dice =
        match instantiate fulfiller dice with
        | r when r.hasExternalReferences = false -> r.dice
        | r -> loop r.dice
    loop dice

let rec toString = function
    | Modifier n -> n.ToString()
    | Dice(n,d) -> sprintf "%dd%d" n d
    | External e -> sprintf "%A" e
    | Binary(l, op, r) -> (toString l) + (match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/") + (toString r)
    | Min(l, r) -> sprintf "min(%s,%s)" (toString l) (toString r)
    | Max(l, r) -> sprintf "max(%s,%s)" (toString l) (toString r)

module Parse =
    open Packrat
    let (|Mod|_|) = function
        | Int(n, rest) -> Some(Modifier n, rest)
        | _ -> None
    let (|Term|_|) (|External|_|) =
        let rec (|Term|_|) =
            let (|D|_|) = pack <| function
                | Int(n, Str "d" (Int(d, rest))) -> Some(Dice(n, d), rest)
                | Str "d" (Int(d, rest)) -> Some(Dice(1, d), rest)
                | _ -> None
            pack <| function
            | Term(d, OWS(Str "+" (OWS (Term(d', rest))))) -> Some(Binary(d, Plus, d'), rest)
            | Term(d, OWS(Str "-" (OWS (Term(d', rest))))) -> Some(Binary(d, Minus, d'), rest)
            | Term(d, OWS(Str "*" (OWS (Int(n, rest))))) -> Some(Binary(d, Times, Modifier n), rest)
            | Term(d, OWS(Str "/" (OWS (Int(n, rest))))) -> Some(Binary(d, Divide, Modifier n), rest)
            | External(e, rest) -> Some(External e, rest)
            | Str "min(" (Term(d, Str "," (Term(d', Str ")" rest)))) -> Some(Min(d, d'), rest)
            | Str "max(" (Term(d, Str "," (Term(d', Str ")" rest)))) -> Some(Max(d, d'), rest)
            | D(d, Str "a" rest) -> Some(Max(d,d), rest)
            | D(d, Str "d" rest) -> Some(Min(d,d), rest)
            | D(d, rest) -> Some(d, rest)
            | Mod(d, rest) -> Some(d, rest)
            | _ -> None
        (|Term|_|)

