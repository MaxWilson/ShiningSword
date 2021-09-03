module Domain.Parse.Ribbit

open Packrat

type RosterAdaptor = {
    isValidNamePrefix: (string -> bool)
    tryNamePrefix: string -> Id list
    tryId: Id -> string option
    tryName: string -> Id option
}

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
