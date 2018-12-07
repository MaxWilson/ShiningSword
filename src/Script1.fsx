#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#r @"\tmp\Fable.JsonConverter.dll"
#load @"Common.fs"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Abstractions\DataStorage.fs"
#load @"Model\Types.fs"
#load @"Model\Tables.fs"

open System
open Common
open Interaction
open Model.Types
open Model.Tables

let consoleResolve q = printfn "%A" q; Console.ReadLine()

DataStorage.showRaw "thingTracker" |> Eventual.resolveSynchronously consoleResolve
type Thing = { name: string; instances: (string * int) list }
DataStorage.list<Thing> "thingTracker"
DataStorage.load<Thing> "thingTracker" "Fasting"
type Class = Fighter | Wizard
type PC = { name: Name; XP: int; HP: int; rolls: int[]; levels: (Class list); description: Description }
let combatBonus stat = (stat/2) - 5 // based on 5E tables
let computeHP con classList =
    let bonus = combatBonus con
    let dieSize characterClass = match characterClass with Fighter -> 10 | Wizard -> 6
    classList |> Seq.mapi (fun l cl -> if l = 0 then (dieSize cl) + bonus else (dieSize cl)/2 + 1 + bonus) |> Seq.sum
let computeLevel xp =
    (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
let stats() =
    Array.init 6 (fun _ -> Seq.init 4 (thunk1 rand 6) |> Seq.sortDescending |> Seq.take 3 |> Seq.sum)
let PC name characterClass xp =
    let stats = stats()
    let levels = List.init (computeLevel xp) (thunk characterClass)
    { name = name; XP = xp; HP = (computeHP stats.[2] levels); rolls = stats; levels = levels; description = "" }
let PCTag = "ShiningSwordPC"
let savePC pc = DataStorage.save PCTag pc.name pc
let loadPC name = DataStorage.load<PC> PCTag name
PC "Vaughn Shawnessey" Fighter 3000 |> savePC
loadPC "Vaughn Shawnessey"

let exec finalAction =
    Eventual.continueWith finalAction
    >> Eventual.resolveSynchronously consoleResolve

let show pc =
    sprintf "Name: %s\n%dth level %A (%d XP)\nStr %d Dex %d Con %d Int %d Wis %d Cha %d HP %d " pc.name (pc.levels.Length) (pc.levels.[0]) pc.XP pc.rolls.[0] pc.rolls.[1] pc.rolls.[2] pc.rolls.[3] pc.rolls.[4] pc.rolls.[5] pc.HP

(PC "Vlad" Wizard ((rand 20)*(rand 20)*(rand 20) * 100)) |> show |> printfn "%s"

loadPC "Vaughn Shawnessey" |> exec (show >> printfn "%s")

let calculate mtable (monsters: Name seq) =
    let costs = monsters |> Seq.map (fun m -> Math.Pow((mtable m |> snd |> float) / 100., (2./3.)))
    (Math.Pow(Seq.sum costs, 1.5) |> int) * 100

let normalize template =
    [|for (name, i) in template do
        for i in 1..i do
            yield name
        |]

let makeEncounter (mtable: Name -> float * int) templates (maxCR: int) (xpBudget: int) =
    let rec generate() =
        let template : (Name * int) list = templates maxCR
        let template = normalize template
        let rec addMonster accum =
            let precost = calculate mtable accum
            if precost >= xpBudget then
                accum
            else
                let monster = template.[random.Next(template.Length)]
                let monsters' = monster::accum
                let postcost = calculate mtable monsters'
                if postcost <= xpBudget then
                    addMonster monsters'
                else // probabilistically add the final monster, or not
                    let overage = postcost - xpBudget
                    let overageRatio = (float overage) / (float xpBudget)
                    if random.NextDouble() < overageRatio then
                        monsters'
                    else
                        accum
        match addMonster [] with
        | [] ->
            generate() // this template was too tough to allow even one monster--choose a different template
        | candidate ->
            candidate
    let lst = generate()
    lst |> List.groupBy id |> List.map (fun (k, vs) -> k, List.length vs) |> List.sortByDescending snd

let monsters = [
    "Hobgoblin", 0.5
    "Orc", 0.5
    "Orog", 2.
    "Orc War Chief", 4.
    "Beholder", 13.
    "Frost Giant", 8.
    "Fire Giant", 9.
    "Skeleton", 0.25
    "Zombie", 0.5
    "Goblin", 0.25
    "Flameskull", 4.
    "Githyanki Warrior", 3.
    "Yeti", 3.
    "Young White Dragon", 6.
    "Young Red Dragon", 10.
    "Adult Red Dragon", 17.
    ]
let lookup monsters name = monsters |> List.find (fst >> (=) name) |> fun (_, cr) -> Model.Tables.monsterCR |> Array.pick (function { CR = cr'; XPReward = xp } when cr' = cr -> Some(cr, xp) | _ -> None)
let templates = [|
    ["Orc", 10; "Orc War Chief", 1]
    ["Beholder", 1; "Hobgoblin", 20]
    ["Fire Giant", 1; "Hobgoblin", 8; "Skeleton", 4]
    ["Orc", 10; "Orog", 1]
    ["Skeleton", 3; "Zombie", 2]
    ["Orc", 6; "Skeleton", 4]
    ["Githyanki Warrior", 6; "Yeti", 3]
    ["Young White Dragon", 1]
    ["Young Red Dragon", 1]
    ["Adult Red Dragon", 1]
    ["Frost Giant", 1; "Yeti", 2]
    |]
let rec getTemplate monsters (templates: (string * int) list[]) maxCR =
    let t = templates.[random.Next(templates.Length)]
    if t |> List.exists (fun (name, _) -> (lookup monsters name |> fst) |> int > maxCR) then
        getTemplate monsters templates maxCR
    else
        t
let mutable earned = 0
let N = 4 // number of ideal PCs
for gateNumber in 1..100 do
    let level = (computeLevel earned)
    let mutable cost = 0
    printfn "==================Gate %d=================" gateNumber
    for _ in 1..3 do
        let budget = 4 * (xpBudgets.[level-1].deadly)
        let e = makeEncounter (lookup monsters) (getTemplate monsters templates) level budget
        let c = (calculate (lookup monsters) (normalize e))
        let xpEarned' = e |> Seq.sumBy (fun (name, i) -> i * (lookup monsters name |> snd))
        earned <- earned + xpEarned'/N
        printfn "\t%A %d (earned %d each)" e c (xpEarned'/N)
        cost <- cost + c
    printfn "Total cost for gate #%d (level %d): %d/%d XP\n" gateNumber level cost (4 * (xpBudgets.[level-1].daily))
    printfn "Earned %d XP so far (level %d)" earned (computeLevel earned)
    printfn "===========================================\n"
