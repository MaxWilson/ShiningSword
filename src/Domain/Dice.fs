module Domain.Dice

open Common

type Dice<'externalProperty> =
    | Modifier of int
    | Dice of number: int * kind: int
    | External of 'externalProperty
    | Sum of Dice<'externalProperty> * Dice<'externalProperty>
    | Min of Dice<'externalProperty> * Dice<'externalProperty>
    | Max of Dice<'externalProperty> * Dice<'externalProperty>

type Fulfiller<'externalProperty> = 'externalProperty -> Dice<'externalProperty> // will resolve a structure with 'holes' in it to the same type of structure but with fewer 'holes' in it

let instantiate fulfiller (dice:Dice<_>) : {| dice: Dice<_>; readyToRoll: bool |} =
    {| dice = dice; readyToRoll = false |}

let sample = function
    | External _ -> shouldntHappen()
    | Modifier n -> n

#nowarn "40" // we're not doing anything funny at initialization-time, like calling functions in the ctor that rely on uninitialized members, so we don't need the warning
module Parse =
    open Packrat
    let (|Mod|_|) = function
        | Int(n, rest) -> Some(Modifier n, rest)
        | _ -> None
    let (|D|_|) = function
        | Int(n, Str "d" (Int(d, rest))) -> Some(Dice(n, d), rest)
        | Str "d" (Int(d, rest)) -> Some(Dice(1, d), rest)
        | _ -> None
    type RosterAdaptor = {
                isValidNamePrefix: (string -> bool)
                tryNamePrefix: string -> Id list
                tryId: Id -> string option
                tryName: string -> Id option
            }
    let (|Term|_|) (|External|_|) =
        let rec (|Term|_|) =
            pack <| function
            | Term(d, Str "+" (Term(d', rest))) -> Some(Sum(d, d'), rest)
            | External(e, rest) -> Some(External e, rest)
            | Str "min(" (Term(d, Str "," (Term(d', Str ")" rest)))) -> Some(Min(d, d'), rest)
            | Str "max(" (Term(d, Str "," (Term(d', Str ")" rest)))) -> Some(Max(d, d'), rest)
            | D(d, rest) -> Some(d, rest)
            | Mod(d, rest) -> Some(d, rest)
            | _ -> None
        (|Term|_|)
    module External =
        let (|Roster|_|) =
            Packrat.ExternalContextOf<RosterAdaptor>
        let (|ValidName|_|) =
            // in this case we need to do something a little tricky: detect names by prefix
            pack <| function
            | Roster(roster) as ctx ->
                match ctx with
                | LongestSubstringWhere (roster.isValidNamePrefix) 30 (name, ctx) ->
                    match roster.tryNamePrefix name with
                    | [id] -> Some (id, ctx) // allow shortened names like El for Eladriel, as long as they are unambiguous (only one match)
                    | matches ->
                        // if there are multiple matches, try for an exact match. Don't want to disregard "skeleton 1" just because "skeleton 10" exists.
                        matches |> List.tryPick(fun id -> match roster.tryId id with Some v when String.equalsIgnoreCase v name -> Some(id, ctx) | _ -> None)
                | _ -> None
            | _ -> None
        let (|ValidNames|_|) =
            let checkForIds ctx = function
                | [] -> None
                | ids -> Some(ids, ctx)
            let (|Wildcard|_|) (roster: RosterAdaptor) = function
                | Chars alphawhitespace (prefix, Char('*', ctx)) ->
                    roster.tryNamePrefix prefix |> checkForIds ctx
                | _ -> None
            let (|Range|_|) (roster: RosterAdaptor) = function
                | Chars alphawhitespace (prefix, Int(start, Str "-" (Int(finish, ctx)))) ->
                    [
                        for i in start..finish do
                            let name = (prefix + (i.ToString()))
                            match roster.tryName name with
                            | Some id -> yield id
                            | None -> ()
                        ] |> checkForIds ctx
                | _ -> None
            // in this case we need to do something a little tricky: detect names by prefix
            pack <| function
            | Roster(roster) as ctx ->
                match ctx with
                | Wildcard roster (ids, ctx) -> Some(ids, ctx)
                | Range roster (ids, ctx) -> Some(ids, ctx)
                | ValidName(id, ctx) -> Some([id], ctx)
                | _ -> None
            | _ -> None
        let (|External|_|) (createReference: (Id * string) -> 't) =
            let (|CombatantProperty|_|) = pack <| function
                | ValidName(id, Str "'s" (Word(propertyName, ctx))) -> Some((id, propertyName) |> createReference, ctx)
                | ValidName(id, Str "." (Word(propertyName, ctx))) -> Some((id, propertyName) |> createReference, ctx)
                | _ -> None
            (|CombatantProperty|_|)
