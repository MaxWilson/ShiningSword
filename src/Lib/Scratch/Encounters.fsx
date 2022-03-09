#if INTERACTIVE
#else
module Encounters
#endif

let rand = System.Random()
type MonsterId = MonsterId of string
type RollSpec = StaticNumber of int | RollSpec of n:int * d:int * (RollSpec option)
let d6 = RollSpec(1, 6, None)
type Entry = { title: string; weight: float; monsters: {| probability: float; monster: MonsterId; number: RollSpec |} list }
type Stats = { name: string; pluralName: string option; xp: int }
    with
    static member Create(name, pluralName, xp) = { name = name; pluralName = Some pluralName; xp = xp }
    static member Create(name, xp) = { name = name; pluralName = None; xp = xp }
let monster name =
    {| probability = 1.; monster = MonsterId name; number = StaticNumber 1 |}
let monsters name roll =
    {| probability = 1.; monster = MonsterId name; number = RollSpec roll |}
let aFew name = monsters name (2,4,None)
let some name = monsters name (3,6,None)
let many name = monsters name (3,10,None)

let rec roll = function
    | StaticNumber n -> n
    | RollSpec(n, d, rest) ->
        let rolls = [for _ in 1..n -> 1+rand.Next d] |> List.sum
        rolls +
            match rest with
            | None -> 0
            | Some rest -> roll rest

type Difficulty =
    | Easy
    | Moderate
    | Challenging
    | Hard
    | Deadly
    | Insane
    | Epic
    | Legendary
    | Ultimate

let chooseOne (entries: Entry list) =
    let weightedEntries = entries |> List.map (fun e -> e, e.weight)
    let sums = List.scan (+) 0. (weightedEntries |> List.map snd) |> List.tail
    let pick = rand.NextDouble() * (sums |> List.last)
    let index = sums |> List.findIndex ((<) pick)
    entries.[index]

let stats =
    [
        Stats.Create("Goblin", 50)
        Stats.Create("Bandit", 100)
        Stats.Create("Orc", 100)
        Stats.Create("Wolf", 100)
        Stats.Create("Warg", 200)
        Stats.Create("Werewolf", 450)
        Stats.Create("Intellect Devourer", 450)
        Stats.Create("Ogre", 450)
        Stats.Create("Troll", 1400)
        Stats.Create("Ogre Mage", "Ogre Magi", 2000)
        Stats.Create("Illithid", 2900)
        Stats.Create("Vampire", 3900)
        Stats.Create("Beholder", 10000)
        Stats.Create("Hive Mother", 20000)
        Stats.Create("Young White Dragon", 3900)
        Stats.Create("Young Red Dragon", 5000)
        Stats.Create("Adult White Dragon", 9000)
        Stats.Create("Adult Red Dragon", 13000)
        Stats.Create("Ancient White Dragon", 20000)
        Stats.Create("Ancient Red Dragon", 30000)
    ]
    |> List.map(fun s -> MonsterId s.name, s)
    |> dict
let describe (entry: Entry) =
    let describeNumber rollSpec =
        let rec recur min max = function
            | StaticNumber n when min <> max -> sprintf "%d-%d" (min+n) (max+n)
            | StaticNumber n -> (min+n).ToString()
            | RollSpec(n, d, rest) ->
                recur (min + n) (max + n * d) (match rest with None -> StaticNumber 0 | Some rest -> rest)
        recur 0 0 rollSpec
    let describeMonster rollSpec monsterId =
        let stats = stats.[monsterId]
        let isPlural = rollSpec = StaticNumber 1 |> not
        match stats.pluralName with
        | Some plural when isPlural -> plural
        | None when isPlural -> stats.name + "s"
        | _ -> stats.name
    let monsterGroups =
        entry.monsters
        |> List.map (fun group -> $"{describeNumber group.number} {describeMonster group.number group.monster}")
    $"""{entry.title}: {System.String.Join(" and ", monsterGroups)}"""
    
let index = function
    | Easy -> [
        { title = "You meet a troll under a bridge"; weight = 0.2; monsters = [monster "Troll"]}
        { title = "Some bandits try to rob you"; weight = 2; monsters = [monsters "Bandit" (2,4,None)]}
        { title = "You defend children from an ogre"; weight = 1; monsters = [monster "Ogre"]}
        { title = "Goblin wolf riders attack the town"; weight = 1; monsters = [aFew "Goblin"; aFew "Wolf"]}
        ]        
    | Moderate -> [
        { title = "You meet a troll under a bridge"; weight = 0.2; monsters = [monster "Troll"; aFew "Goblin"]}
        { title = "Some bandits try to rob you"; weight = 2; monsters = [monsters "Bandit" (3,6,None)]}
        { title = "You defend children from an ogre"; weight = 1; monsters = [monster "Ogre Mage"]}
        { title = "Goblin wolf riders attack the town"; weight = 1; monsters = [some "Goblin"; aFew "Wolf"; monster "Ogre"]}        
        ]
    | Challenging -> [
        { title = "The town priest turns out to be a werewolf"; weight = 1; monsters = [monster "Werewolf"]}
        ]
    | Hard -> [
        { title = "Orcs attack the town"; weight = 1; monsters = [monsters "Orc" (10,10,None); monsters "Warg" (3,10,None)]}
        ]
    | Deadly -> [
        { title = "The town priest turns out to be a vampire"; weight = 1; monsters = [monster "Vampire"]}
        { title = "The town priest turns out to be a mind flayer"; weight = 1; monsters = [monster "Illithid"]}
        ]
    | Insane -> [
        { title = "The town council turns out to all be in cahoots with mind flayers"; weight = 1; monsters = [monsters "Illithid" (1, 4, None); monsters "Intellect Devourer" (2,4,None)]}
        { title = "You raid a dragon's treasure hoard"; weight = 1; monsters = [monster "Adult Red Dragon"]}
        { title = "You must rescue a good friend from the clutches of a beholder"; weight = 1; monsters = [monster "Beholder"]}
        ]
    | Epic -> [
        { title = "You raid a dragon's treasure hoard"; weight = 1; monsters = [monster "Ancient Red Dragon"]}
        { title = "You must rescue a good friend from the deptch of a beholder tyrant ship"; weight = 1; monsters = [monster "Hive Mother"; monsters "Beholder" (1, 12, None)]}
        ]
    | Legendary -> [
        { title = "You raid a dragon's treasure hoard"; weight = 1; monsters = [monster "Ancient White Dragon"; monsters "Young White Dragon" (1, 12, None)]}
        { title = "You raid a dragon's treasure hoard"; weight = 1; monsters = [monster "Ancient Red Dragon"; monsters "Young Red Dragon" (1, 8, None)]}
        ]
    | Ultimate -> [
        { title = "You raid a dragon's treasure hoard"; weight = 1; monsters = [monster "Ancient White Dragon"; monsters "Young White Dragon" (1, 12, None); monsters "Beholder" (1,12,None)]}
        { title = "Space ogre invasion!"; weight = 1; monsters = [monsters "Ogre Mage" (10, 20, None)]}
        ]

let enumerateUnion<'t>() =
    Microsoft.FSharp.Reflection.FSharpType.GetUnionCases(typeof<'t>)
    // turn the reflection info back into the actual union case
    |> Array.map (fun info -> Microsoft.FSharp.Reflection.FSharpValue.MakeUnion(info, [||]) :?> 't)
    
let checkIndex() =    
    let violations =
        [for difficulty in enumerateUnion<Difficulty>() do
            let entries = index difficulty
            for e in entries do
                for m in e.monsters do
                    if stats.ContainsKey m.monster |> not then
                        m.monster
            ]
        |> List.distinct
    violations
checkIndex()

let generateEncounter difficulty =
    index difficulty |> chooseOne
let generateUpTo difficulty =
    let difficulties = enumerateUnion<Difficulty>()
    let maxIndex = difficulties |> Seq.findIndex ((=) difficulty)
    difficulties[rand.Next(maxIndex)]
        |> generateEncounter
generateEncounter Ultimate |> describe
generateUpTo Ultimate |> describe
