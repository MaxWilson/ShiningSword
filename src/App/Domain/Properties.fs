module Domain.Properties

open Common
type Property = { name: string }

type RosterAdaptor = {
            isValidNamePrefix: (string -> bool)
            tryNamePrefix: string -> Id list
            tryId: Id -> string option
            tryName: string -> Id option
        }

module Parse =
    open Packrat
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
    let (|PropertyReference|_|) (createReference: (Id * string) -> 't) = pack <| function
            | ValidName(id, Str "'s" (Word(propertyName, ctx))) -> Some((id, propertyName) |> createReference, ctx)
            | ValidName(id, Str "." (Word(propertyName, ctx))) -> Some((id, propertyName) |> createReference, ctx)
            | _ -> None
    let (|PropertyMultiReference|_|) (createReference: (Id * string) -> 't) = pack <| function
            | ValidNames(ids, Str "'s" (Word(propertyName, ctx))) -> Some(ids |> List.map (fun id -> (id, propertyName) |> createReference), ctx)
            | ValidNames(ids, Str "." (Word(propertyName, ctx))) -> Some(ids |> List.map (fun id -> (id, propertyName) |> createReference), ctx)
            | _ -> None
