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

type Stat = Str | Dex | Con | Int | Wis | CharData

type Gender = Male | Female
type Constraint =
    | AtLeast of Stat * int
    | Gender of Gender

type NameGen = Gender -> string

type Template = {
    name: string
    nameGen: NameGen
    statPriorities: (Stat * int) list
    constraints: Constraint list
    }

type Stats = {
    str: int
    dex: int
    con: int
    int: int
    wis: int
    cha: int
    }

type CharSheet = {
    name: string
    gender: Gender
    template: Template
    stats: Stats
    }

rollStats()

let instantiate (template: Template) (nameGen: NameGen option) stats : CharSheet option =
    let statsSatisfice (pri: (Stat * int) list) stats =
        let pri = [(Str, 1); (Dex, 2); Con, 2; Str, 3]
        let statsInOrderByPriority =
            pri
            |> Seq.distinctBy fst // break repeats arbitrarily
            |> Seq.sortBy(ignore1 random.Next)
            |> Seq.sortBy snd
            |> Seq.map fst
            |> List.ofSeq
        let statIndex = function
        let assignedStats =
            statsInOrderByPriority
            |>

        ()
    let genderSatisfice() =
        template.constraints |> List.tryPick (function Gender g -> Some g | _ -> None) |> Option.defaultWith (fun () -> [Male; Female].[rand 2])
    let gender = genderSatisfice()
    let stats = statsSatisfice template stats
    match stats with
    | Some stats ->
        { name = (defaultArg nameGen template.nameGen) gender
          gender = gender
          stats = stats
          template = template
          }
    | None -> None
