// this is a fast prototype of what it might be like to generate chars based on templates

#load "Common.fs"
open Common

#r """\\maxwBlizzard\shared\icm\Newtonsoft.Json.dll"""

type Datum = | Number of int | Text of string
type CharData = Map<string, Datum>

let roll3d6() =
    let roll() =
        List.init 3 (thunk1 rand 6) |> List.sum
    List.init 6 (ignore1 roll)
let roll4d6k3() =
    let roll() =
        List.init 4 (thunk1 rand 6) |> List.sortDescending |> List.take 3 |> List.sum
    List.init 6 (ignore1 roll)

type Stat = Str | Dex | Con | Int | Wis | Cha

type Gender = Male | Female
type Constraint =
    | AtLeast of Stat * int
    | Gender of Gender
    | Mythmaker

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
    xp: int
    mythmaker: bool
    }

let lookup (s: Stats) = function
    | Str -> s.str
    | Dex -> s.dex
    | Con -> s.con
    | Int -> s.int
    | Wis -> s.wis
    | Cha -> s.cha

let satisfice (template: Template) (nameGen: NameGen option) stats mythic : CharSheet option =
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
            let violations =
                template.constraints |> Seq.filter (function
                | AtLeast(stat, minimum) ->
                    let v = lookup stats stat
                    v < minimum
                | Mythmaker ->
                    mythic = false // violation if not a mythmaker
                | _ -> false // otherwise not a violation
                )
            if violations |> Seq.isEmpty then
                Some stats
            else None
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
          xp = 0
          mythmaker = mythic
          }
        |> Some
    | _ -> None

let defaultNames = function
    | Male -> ["Varic the Red"; "Durmstrong"; "Sad Sam"] |> chooseRandom
    | Female -> ["Mad Marta"; "Slashing Sally"; "Dynamo"] |> chooseRandom

let templates = [
    { name = "Holy Warrior"; nameGen = defaultNames; statPriorities = [Cha, 1; Str, 2; Con, 2; Wis, 3]; constraints = [AtLeast(Cha, 16); AtLeast(Wis, 12); AtLeast(Str, 15); Mythmaker] }
    { name = "Shadow Ninja"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 3; Wis, 2]; constraints = [AtLeast(Wis, 16); AtLeast(Dex, 16); ] }
    { name = "Monklock"; nameGen = defaultNames; statPriorities = [Cha, 1; Dex, 2; Con, 3; Wis, 2]; constraints = [AtLeast(Cha, 16); AtLeast(Wis, 15); AtLeast(Dex, 15); Mythmaker] }
    { name = "Nimble Cataphract"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2]; constraints = [AtLeast(Dex, 15); AtLeast(Con, 12);] }
    { name = "Armored Cataphract"; nameGen = defaultNames; statPriorities = [Str, 1; Con, 2]; constraints = [AtLeast(Str, 15); AtLeast(Con, 12);] }
    { name = "Eldritch Knight Sharpshooter"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2; Int, 3]; constraints = [AtLeast(Dex, 15); AtLeast(Con, 12); Mythmaker] }
    { name = "Valentinian"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2; Int, 3]; constraints = [AtLeast(Dex, 15); AtLeast(Con, 12); Mythmaker] }
    { name = "Thief"; nameGen = defaultNames; statPriorities = [Dex, 1; Con, 2]; constraints = [AtLeast(Dex, 13)] }
    { name = "Thug"; nameGen = defaultNames; statPriorities = [Str, 1; Con, 2]; constraints = [AtLeast(Str, 13)] }
    { name = "Dregs"; nameGen = defaultNames; statPriorities = []; constraints = [] }
    ]

let instantiate guaranteedMythic stats =
    let templates = templates |> Seq.sortByDescending(fun t -> List.init (1 + t.constraints.Length) (thunk1 rand 6))
    let mythic = guaranteedMythic || rand 100 <= 10
    templates |> Seq.pick (fun t -> satisfice t None stats mythic)

let mutable gp = 3000
let deduct price =
    gp <- gp - price

let mutable roster : CharSheet list = []

let add charSheet =
    roster <- roster @ [charSheet]
    charSheet

let levelOf xp =
    1 + (sqrt ((xp / 100) |> float) |> int)

let report (charSheet: CharSheet) =
    let stats = charSheet.stats
    let stats = sprintf "Str: %d Dex: %d Con: %d Int: %d Wis: %d Cha: %d" stats.str stats.dex stats.con stats.int stats.wis stats.cha
    printfn "%s: %s\n%s" charSheet.name (sprintf "Level %d %A %s" (levelOf charSheet.xp) charSheet.gender charSheet.template.name) stats

let cheap() =
    deduct 100
    roll3d6() |> instantiate false |> add |> report
    printfn "%d gp left" gp

let elite() =
    deduct 700
    roll4d6k3() |> instantiate false |> add |> report
    printfn "%d gp left" gp

let mythic() =
    deduct 1000
    roll4d6k3() |> instantiate true |> add |> report
    printfn "%d gp left" gp

let day gpEarned xpEarned =
    gp <- gp + gpEarned
    roster <- [for r in roster -> { r with xp = r.xp + (xpEarned / roster.Length)}]
    let expenses = ([for r in roster -> levelOf r.xp * (if r.mythmaker then 200 else 100)]) |> List.sum
    deduct expenses
    printfn "Earned %d gp and %d XP\nPaid %d gp\nBalance: %d gp in purse" gpEarned xpEarned expenses gp
    for r in roster do
        report r


cheap()
elite()
mythic()
day 5000 1000
