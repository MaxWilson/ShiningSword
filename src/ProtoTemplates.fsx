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

type Stat = Str | Dex | Con | Int | Wis | Cha

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

let satisfice (template: Template) (nameGen: NameGen option) stats : CharSheet option =
    let statsSatisfice (pri: (Stat * int) list) (stats: _ list) =
        let augment stats =
            stats
            |> flip List.append ([Str; Dex; Con; Int; Wis; Cha] |> Seq.filter(fun st -> List.contains st stats = false) |> Seq.sortBy (ignore1 random.Next) |> List.ofSeq)
            |> List.take 6
        let statsInOrderByPriority =
            pri
            |> Seq.distinctBy fst // break repeats arbitrarily
            |> Seq.sortBy(ignore1 random.Next) // break ties
            |> Seq.sortBy snd
            |> Seq.map fst
            |> List.ofSeq
            |> augment
        // at this point there should be exactly six labels in some order, [Str; Dex; Con; Int; Wis; Cha]
        // we pull the stats off the front of the stat stack in this order, and assign them
        if statsInOrderByPriority.Length = stats.Length then
            let empty = { str = 0; dex = 0; con = 0; int = 0; wis = 0; cha = 0 }
            let assign state = function
            | Str, n -> { state with str = n }
            | Dex, n -> { state with dex = n }
            | Con, n -> { state with con = n }
            | Int, n -> { state with int = n }
            | Wis, n -> { state with wis = n }
            | Cha, n -> { state with cha = n }
            let stats =
                statsInOrderByPriority
                |> flip List.zip (stats |> List.sortDescending)
                |> List.fold assign empty
            Some stats
        else
            None
    let genderSatisfice() =
        template.constraints |> List.tryPick (function Gender g -> Some g | _ -> None) |> Option.defaultWith (fun () -> [Male; Female] |> chooseRandom)
    let gender = genderSatisfice()
    let stats = statsSatisfice template.statPriorities stats
    match stats with
    | Some stats ->
        { name = (defaultArg nameGen template.nameGen) gender
          gender = gender
          stats = stats
          template = template
          }
        |> Some
    | None -> None

let defaultNames = function
    | Male -> ["Varic the Red"; "Durmstrong"; "Sad Sam"] |> chooseRandom
    | Female -> ["Mad Marta"; "Slashing Sally"; "Dynamo"] |> chooseRandom
let templates = [
    { name = "Holy Warrior"; nameGen = defaultNames; statPriorities = [Cha, 1; Str, 2; Con, 2; Wis, 3]; constraints = [AtLeast(Cha, 16); AtLeast(Wis, 12); AtLeast(Str, 15)] }
    { name = "Monklock"; nameGen = defaultNames; statPriorities = [Cha, 1; Dex, 2; Con, 3; Wis, 2]; constraints = [AtLeast(Cha, 16); AtLeast(Wis, 15); AtLeast(Dex, 15)] }
    { name = "Eldritch Knight Sharpshooter"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2; Int, 3]; constraints = [AtLeast(Dex, 15); AtLeast(Con, 12);] }
    { name = "Valentinian"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2; Int, 3]; constraints = [AtLeast(Dex, 15); AtLeast(Con, 12);] }
    { name = "Thief"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2]; constraints = [AtLeast(Dex, 13)] }
    { name = "Thug"; nameGen = defaultNames; statPriorities = [Str, 1; Con, 2]; constraints = [AtLeast(Str, 13)] }
    { name = "Dregs"; nameGen = defaultNames; statPriorities = []; constraints = [] }
    ]

let generate() =
    let stats = rollStats()
    let templates = templates |> Seq.sortBy(fun t -> List.init (1 + t.constraints.Length) (thunk1 rand 6))
    templates |> Seq.pick (fun t -> satisfice t None stats)

[for x in 1..10 -> x, generate()]
