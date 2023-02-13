module Domain.Character.DungeonFantasy

open Domain

[<AutoOpen>]
module Stats =
    type Stat = ST | DX | IQ | HT | Will | Per | HP | FP | Move | SpeedTimesFour | Dodge
        with
        member this.Get (stats: Stats) =
            let baseline =
                match this with
                | ST | DX | IQ | HT -> 10
                | Will | Per -> IQ.Get stats
                | HP -> ST.Get stats
                | FP -> HT.Get stats
                | SpeedTimesFour -> (DX.Get stats + HT.Get stats)
                | Move -> SpeedTimesFour.Get stats / 4
                | Dodge -> (SpeedTimesFour.Get stats / 4) + 3
            match stats |> Stats.Get |> Map.tryFind this with
            | Some v -> baseline + v
            | None -> baseline
    and Stats = Stats of Map<Stat, int>
        with
        static member Create x = Stats (Map.ofSeq x)
        static member Get (Stats stats) = stats
        static member (+) (Stats lhs, Stats rhs) =
            let rec combine accum = function
                | [] -> accum
                | KeyValue(k,v)::rest ->
                    match accum |> Map.tryFind k with
                    | Some exist ->
                        let accum = accum |> Map.add k (exist + v)
                        combine accum rest
                    | None ->
                        let accum = accum |> Map.add k v
                        combine accum rest
            combine rhs (lhs |> List.ofSeq) |> Stats

type Profession = Bard | Cleric | Knight | MartialArtist | Swashbuckler | Wizard
type WeaponType = Unarmed | Rapier | Broadsword | Bow | Maingauche | Knife | Spear | Polearm | Staff
type Trait =
    | HighPainThreshold
    | CombatReflexes
    | EnhancedParry of WeaponType

type Skill =
    | Weapon of WeaponType
    | Stealth
    | Camouflage
    | Observation

type Package = {
    name: string
    stats: Stats
    traits: Trait list
    }
    with
    static member Create(name, ?stats, ?traits) =
        {
        name = name
        stats = Stats.Create (stats |> Option.defaultValue [])
        traits = traits |> Option.defaultValue []
        }

let human = Package.Create("Human")
let catfolk = Package.Create("Catfolk", [ST, -1; DX, +1; Per, +1; Move, -1])
let dwarf = Package.Create("Dwarf", [HT, +1; FP, +3; Move, -1])
let elf = Package.Create("Elf", [ST, -1; DX, +1; Move, +1])
let halfElf = Package.Create("Half-elf", [DX, +1])
let halfOgre = Package.Create("Half-ogre", [ST, +4; HT, +1; IQ, -1; Will, +1])
let coleopteran = Package.Create("Coleopteran", [ST, +1; IQ, -1; Per, +1])
let monk = Package.Create("Martial Artist", [ST, +1; DX, +6; HT, +2; Will, +1; Move, +1])
let swash = Package.Create("Swashbuckler", [ST, +1; DX, +5; HT, +3])
let wizard = Package.Create("Wizard", [DX, +2; IQ, +5; HT, +1; Per, -3])
let professions = Map.ofList [MartialArtist, monk; Swashbuckler, swash; Wizard, wizard]
let races = [human; catfolk; dwarf; elf; halfElf; halfOgre; coleopteran]

let debug() =
    for profession in professions.Values do
        for race in races do
            let stats = race.stats + profession.stats
            let sex = chooseRandom [Male; Female]
            let nation, name = makeNameAnyNation sex
            printfn $"\n{name}, {sex} {race.name} {profession.name} from {nation}"
            for stat in [ST; DX; IQ; HT; Will; Per; HP; FP; Move; SpeedTimesFour; Dodge] do
                match stat with
                | SpeedTimesFour ->
                    printf  $"Speed: {(SpeedTimesFour.Get stats |> float) / 4.0}  "
                | _ ->
                    printf  $"{stat}: {stat.Get stats}  "

let rollStats() =
    [for stat in [ST;DX;IQ;HT] do
        // in general we want strong deviations like IQ 6 and IQ 14 to be rare
        let rec exponentialHalt accum stepSize rate =
            if random.NextDouble() <= rate && abs accum <= 5 then
                exponentialHalt (accum + stepSize) stepSize rate
            else
                accum
        stat, exponentialHalt 0 (chooseRandom [-1; +1]) 0.5
        ] |> Stats.Create

type Character = {
    header: RoleplayingData
    profession: Profession
    baseRolls: Stats
    race: string
    stats: Stats
    traits: Trait list
    }

let createRandom randomizeStats =
    let prof = chooseRandom professions.Keys
    let stats = if randomizeStats then rollStats() else Stats.Create []
    let race = chooseRandom races
    let sex = chooseRandom [Male; Female]
    let nation, name = makeNameAnyNation sex
    let rp = {
        RoleplayingData.name = name
        sex = sex
        nationalOrigin = nation
        }
    {   header = rp
        profession = prof
        baseRolls = stats
        race = race.name
        stats = race.stats + professions[prof].stats + stats
        traits = race.traits @ professions[prof].traits
        }

let changeProfession char prof =
    let race = races |> List.find (fun r -> r.name = char.race)
    { char
        with
        profession = prof;
        stats = race.stats + professions[prof].stats + char.baseRolls
        traits = race.traits @ professions[prof].traits}
