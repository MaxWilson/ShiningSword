// this is a fast prototype of what it might be like to generate chars based on templates

#load "Common.fs"
open Common

#r """\\maxwBlizzard\shared\icm\Newtonsoft.Json.dll"""

type Datum = | Number of int | Text of string
type CharData = Map<string, Datum>

let rollStats() =
    let roll() =
        List.init 4 (thunk1 rand 6) |> List.sortDescending |> List.take 3 |> List.sum
    List.init 6 (ignore1 roll)


rollStats()
