#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"

// This script is purely for messing around with procedural generation and stuff.
(*

Let's try generating combinations of rooms, room features, and monsters.

High-level concept: assemble
Assemble: modules UNDER constraints

*)

type 't Constraint = 't -> bool
type 't Offering = 't List
type Assemble<'choiceModule, 'choiceAssembly> = 'choiceModule Offering -> ('choiceModule*'choiceAssembly) Constraint -> 'choiceAssembly
let simpleAssemble zero aggregate suffice: Assemble<'m, 'asm> =
    // using None = Backtrack, Some = valid result
    fun modules constraints ->
        let rec loop(asm, count) =
            match modules |> List.filter (fun m -> constraints(m, asm)) with
            | [] -> loop(zero, count+1)
            | validChoices ->
                let choice = chooseRandom validChoices
                let asm' = aggregate asm choice
                if suffice(asm, choice, asm', count) then asm'
                else
                    loop(asm', count+1)
        loop(zero, 0)
 

let prepend rest head = (head::rest)
simpleAssemble [] prepend (fun (_, _, asm, count) -> (List.sum asm) >= 10 || count > 20)
    [1..10] (fun (m, asm) -> m + (List.sum asm) <= 10)

module DMGish =
    open type System.Math
    type Advancement = { level: int; XPReq: int; proficiencyBonus: int }
    let levelAdvancement =
        [|
        // Level, XP required, proficiency bonus
        1, 0, +2
        2, 300, +2
        3, 900, +2
        4, 2700, +2
        5, 6500, +3
        6, 14000, +3
        7, 23000, +3
        8, 34000, +3
        9, 48000, +4
        10, 64000, +4
        11, 85000, +4
        12, 100000, +4
        13, 120000, +5
        14, 140000, +5
        15, 165000, +5
        16, 195000, +5
        17, 225000, +6
        18, 265000, +6
        19, 305000, +6
        20, 355000, +6
        |] |> Array.map (fun (level, xpReq, proficiencyBonus) -> { level = level; XPReq = xpReq; proficiencyBonus = proficiencyBonus })

    type XPBudget = { level: int; easy: int; medium: int; hard: int; deadly: int; daily: int }
    let xpBudgets =
        [|
        // Level, easy, medium, hard, deadly, daily
        1, 25, 50, 75, 100, 300
        2, 50, 100, 150, 200, 600
        3, 75, 150, 225, 400, 1200
        4, 125, 250, 375, 500, 1700
        5, 250, 500, 750, 1100, 3500
        6, 300, 600, 900, 1400, 4000
        7, 350, 750, 1100, 1700, 5000
        8, 450, 900, 1400, 2100, 6000
        9, 550, 1100, 1600, 2400, 7500
        10, 600, 1200, 1900, 2800, 9000
        11, 800, 1600, 2400, 3600, 10500
        12, 1000, 2000, 3000, 4500, 11500
        13, 1100, 2200, 3400, 5100, 13500
        14, 1250, 2500, 3800, 5700, 15000
        15, 1400, 2800, 4300, 6400, 18000
        16, 1600, 3200, 4800, 7200, 20000
        17, 2000, 3900, 5900, 8800, 25000
        18, 2100, 4200, 6300, 9500, 27000
        19, 2400, 4900, 7300, 10900, 30000
        20, 2800, 5700, 8500, 12700, 40000
        |] |> Array.map (fun (level, easy, medium, hard, deadly, daily) -> { level = level; easy = easy; medium = medium; hard = hard; deadly = deadly; daily = daily})
    type MonsterCR = { CR: float; XPReward: int }
    let monsterCR =
        [|
        // CR, XP
        0., 10
        0.125, 25
        0.25, 50
        0.5, 100
        1., 200
        2., 450
        3., 700
        4., 1100
        5., 1800
        6., 2300
        7., 2900
        8., 3900
        9., 5000
        10., 5900
        11., 7200
        12., 8400
        13., 10000
        14., 11500
        15., 13000
        16., 15000
        17., 18000
        18., 20000
        19., 22000
        20., 25000
        21., 33000
        22., 41000
        23., 50000
        24., 62000
        25., 75000
        26., 90000
        27., 105000
        28., 120000
        29., 135000
        30., 155000
        |] |> Array.map (fun (cr, xp) -> { CR = cr; XPReward = xp })
    let xpValue =
        let xps = monsterCR |> Array.map(fun { CR = cr; XPReward = xp } -> cr, xp) |> Map.ofArray
        fun cr -> xps.[cr]

    let sscalculate (monsters: float seq) =
        let costs = monsters |> Seq.map (fun cr -> Pow((xpValue cr |> float) / 100., (2./3.)))
        (Pow(Seq.sum costs, 1.5) * 100. |> Round |> int)
    let dmgcalculate partySize (monsters: float seq) =
        let costs = monsters |> Seq.sumBy (fun cr -> xpValue cr)
        let mult =
            let mults = [0.5; 1.; 1.5; 2.; 2.5; 3.; 4.; 5.]
            let ix =
                match monsters |> Seq.length with
                | 1 -> 1
                | 2 -> 2
                | n when n <= 6 -> 3
                | n when n <= 10 -> 4
                | n when n <= 14 -> 5
                | _ -> 6
            match partySize with
            | n when n < 3 -> mults.[ix+1]
            | n when n <= 6 -> mults.[ix]
            | _ -> mults.[ix-1]
        (float costs) * mult |> Round |> int

let levelOf xp = DMGish.levelAdvancement |> Array.findBack (fun a -> xp >= a.XPReq)
open DMGish

type Monster = string * float
let xpOf (monsters: Monster list) =
    monsters |> List.map snd |> dmgcalculate 4
let countUp items = items |> List.groupBy id |> List.map (fun (item, copies) -> (copies |> List.length), item)
type Budget = Under | On | Over
let dmgBudget (nPC: int, pcLevel:int) monsters =
    let cost = (monsters |> List.map snd |> dmgcalculate nPC)
    let levelData = xpBudgets.[pcLevel - 1]
    let threshold = nPC * levelData.hard
    let budget = nPC * levelData.deadly
    if cost < threshold then Under
    elif cost >= budget then Over
    else On
let ssBudget (nPC: int, pcLevel:int) monsters =
    let cost = (monsters |> List.map snd |> sscalculate)
    let levelData = xpBudgets.[pcLevel - 1]
    let threshold = nPC * levelData.hard
    let budget = nPC * levelData.deadly
    if cost < threshold then Under
    elif cost >= budget then Over
    else On
let makeHardEncounter budgetCalc (monsters: Monster Offering) (nPC: int, pcLevel:int) =
    simpleAssemble [] prepend (fun (monsters, _, monsters', count) -> count > 1000 || budgetCalc (nPC, pcLevel) monsters = On) 
        monsters (fun ((monster, cr), encounter) ->
            cr <= float (pcLevel + 2)
                && ((monster, cr)::encounter) |> List.groupBy fst |> List.length <= 3 // don't want more than 3 types of monsters
                && (budgetCalc (nPC, pcLevel) ((monster, cr)::encounter)) <> Over)
    |> List.map fst
    |> countUp
let menagerie = [
    "Goblin", 0.25
    "Mind Flayer", 7.
    "Hill Giant", 5.
    "Star Spawn Grue", 0.25
    "Star Spawn Mangler", 5.
    "Star Spawn Hulk", 10.
    "Drow Warrior", 0.25
    "Angry Peasant Zombie", 0.5
    "Young White Dragon", 6.
    "Young Red Dragon", 10.
    "Beholder", 13.
    ]
makeHardEncounter dmgBudget menagerie (4, 10)
makeHardEncounter ssBudget menagerie (4, 10)
makeHardEncounter dmgBudget menagerie (4, 20)
makeHardEncounter ssBudget menagerie (4, 20)
