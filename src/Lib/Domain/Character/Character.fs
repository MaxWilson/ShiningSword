[<AutoOpen>]
module Domain.Character.Core

open Core.DerivedTraits

type Origin = { ruleSystem: string; nationalOrigin: string; startingLevel: int; statRollMethod: string }

// turn camel casing back into words with spaces, for display to user
let uncamel (str: string) =
    let caps = ['A'..'Z'] |> Set.ofSeq
    let lower = ['a'..'z'] |> Set.ofSeq
    let mutable spaceNeededBefore = []
    let mutable inWord = true
    for i in 1..str.Length-1 do
        match str[i] with
        | ' ' -> inWord <- false
        // When multiple caps are in a row, no spaces should be used, except before the last one if it's followed by a lowercase.
        // E.g. MySSNNumber => My SSN Number, but MySSN => My SSN not My SS N
        | letter when caps.Contains letter && inWord && ((caps.Contains str[i-1] |> not) || i+1 < str.Length && lower.Contains str[i+1])->
            spaceNeededBefore <- i::spaceNeededBefore
        | letter when System.Char.IsLetterOrDigit letter -> inWord <- true
        | _ -> ()
    let rec recur workingCopy spacesNeeded =
        match spacesNeeded with
        | [] -> workingCopy
        | index::rest ->
            recur $"{workingCopy[0..(index-1)]} {workingCopy[index..]}" rest
    recur str spaceNeededBefore

