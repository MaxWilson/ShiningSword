module Domain.Character.DungeonFantasy

open Domain

[<AutoOpen>]
module Stats =
    type Stat = ST | DX | IQ | HT | Will | Per | SZ | HP | FP | Move | SpeedTimesFour | Dodge
        with
        member this.Get (stats: Stats) =
            let baseline =
                match this with
                | ST | DX | IQ | HT -> 10
                | Will | Per -> IQ.Get stats
                | SZ -> 0
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
let halfling = Package.Create("Halfling", [ST, -3; DX, +1; HT, +1; SZ, -2; HP, +2; Move, -1])
let monk = Package.Create("Martial Artist", [ST, +1; DX, +6; HT, +2; Will, +1; Move, +1])
let swash = Package.Create("Swashbuckler", [ST, +1; DX, +5; HT, +3])
let wizard = Package.Create("Wizard", [DX, +2; IQ, +5; HT, +1; Per, -3])
let professions = Map.ofList [MartialArtist, monk; Swashbuckler, swash; Wizard, wizard]
let races = [10, human; 1, catfolk; 2, dwarf; 1, elf; 2, halfElf; 2, halfOgre; 1, coleopteran; 1, halfling]

type RandomizationMethod = NonRandom | Exponential | Average3d6
let rollStats method =
    match method with
    | NonRandom ->
        [] |> Stats.Create
    | Exponential ->
        [for stat in [ST;DX;IQ;HT] do
            // in general we want strong deviations like IQ 6 and IQ 14 to be rare
            let rec exponentialHalt accum stepSize rate =
                if random.NextDouble() <= rate && abs accum <= 5 then
                    exponentialHalt (accum + stepSize) stepSize rate
                else
                    accum
            stat, exponentialHalt 0 (chooseRandom [-1; +1]) 0.5
            ] |> Stats.Create
    | Average3d6 ->
        [for stat in [ST;DX;IQ;HT] do
            // average 3d6, 3d6 should make values above 14 or below 6 quite rare, which
            // fits conventional wisdom on GURPS vs. D&D: each +1 in GURPS is
            // akin to +2 in D&D.
            stat, (List.init 6 (thunk1 rand 6) |> List.sum) / 2 - 10
            ] |> Stats.Create

type Character = {
    header: RoleplayingData
    profession: Profession
    baseRolls: Stats
    race: string
    stats: Stats
    traits: Trait list
    }

type Constraints = {
    randomizationMethod: RandomizationMethod
    race: Package option
    sex: Sex option
    nationPreference: string option
    }
    with
    static member fresh = {
        randomizationMethod = NonRandom
        race = None
        sex = None
        nationPreference = None
        }

let createRandom (c: Constraints) =
    let prof = chooseRandom professions.Keys
    let stats = rollStats c.randomizationMethod
    let race = c.race |> Option.defaultValue (chooseWeightedRandom races)
    let sex = c.sex |> Option.defaultValue (chooseRandom [Male; Female])
    let nation, name =
        match c.nationPreference with
        | None ->
            makeNameAnyNation sex
        | Some nation ->
            // try again
            match makeName nation sex with
            | Some name -> nation, name
            | None ->
                // Maybe user specified an invalid combination. Give up on nation preference.
                match makeName nation sex with
                | Some name -> nation, name
                | None -> makeNameAnyNation sex

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

let changeProfession (char: Character) prof =
    let race = races |> List.find (fun (_, r) -> r.name = char.race) |> snd
    { char
        with
        profession = prof;
        stats = race.stats + professions[prof].stats + char.baseRolls
        traits = race.traits @ professions[prof].traits}
