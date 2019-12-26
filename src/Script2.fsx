#load "Common.fs"
open Common

(* Okay, so... we need a way to store character stats.
We need: stats, levels, persistent decisions, equipment, and current resource states
We need a way to model CLASSES as well as individual characters.

*)

type Number = int
type Stat = Str | Dex | Con | Int | Wis | Cha
type Stats = {
    Str: Number
    Dex: Number
    Con: Number
    Int: Number
    Wis: Number
    Cha: Number
}

let stat stat =
    match stat with
    | Str -> Lens.lens (fun c -> c.Str) (fun v c -> { c with Str = v })
    | Dex -> Lens.lens (fun c -> c.Dex) (fun v c -> { c with Dex = v })
    | Con -> Lens.lens (fun c -> c.Con) (fun v c -> { c with Con = v })
    | Int -> Lens.lens (fun c -> c.Int) (fun v c -> { c with Int = v })
    | Wis -> Lens.lens (fun c -> c.Wis) (fun v c -> { c with Wis = v })
    | Cha -> Lens.lens (fun c -> c.Cha) (fun v c -> { c with Cha = v })

type Race = Human | Elf | Dwarf | Halfling | HalfElf
type SpellList = Wizards | Druids | Clerics | Bards | Warlocks | Paladins | Rangers
type FeatureTag =
    | HeavyArmorDamageResistance
    | Faster
    | ExtraHP
    | SecondWind
    | ActionSurge
    | ExtraAttack
    | Indomitable
type Feature =
    | Race of Race
    | Subrace of string
    | HeavyArmorMaster
    | ASI of Stat * int
    | GreatWeaponMaster
    | Sharpshooter
    | DefensiveDuelist
    | CharmResist
    | NoSleep
    | ExtraCantrip of SpellList
    | Feat
    | PoisonResist
    | MediumArmorProficiency
    | Darkvision
    | ArcheryStyle
    | DefenseStyle
    | DuelingStyle
    | ChooseSplitASI
    | ChooseFullASI
    | FeatOrASI
    | QuantizedFeature of FeatureTag * int

let descriptions =
    let times = function 1 -> "Once" | 2 -> "Twice" | n -> sprintf "%d times" n
    function
    | QuantizedFeature(HeavyArmorDamageResistance, n) ->
        sprintf "Damage resistance %d vs. nonmagical damage while wearing heavy armor" n
    | ASI(ability, bonus) ->
        sprintf "+%d to %s" bonus (ability.ToString())
    | Race race ->
        sprintf "You are a %A" race
    | Subrace subrace ->
        sprintf "You are a %A" subrace
    //| ClassLevel(cl, n) ->
    //    sprintf "You are a level %d or higher %A" n cl
    | QuantizedFeature(Faster, n) ->
        sprintf "Your speed increases by %d' per round" n
    | CharmResist ->
        "You have advantage on saving throws against charm effects"
    | NoSleep ->
        "You do not need to sleep, nor can magic put you to sleep"
    | QuantizedFeature(ExtraHP, n) ->
        sprintf "You gain %d additional HP per level" n
    | PoisonResist -> "You have advantage on saving throws against poison, and you are resistant to poison damage"
    | MediumArmorProficiency -> "You are proficient in the use of medium armors like breastplates and half-plate"
    | Darkvision -> "You can see normally in shadows or dim light up to 60' away, and you can see dimly in darkness up to 60' away"
    | QuantizedFeature(SecondWind, n) -> sprintf "Once per short rest you can regain 1d10+%d HP as a bonus action" n
    | QuantizedFeature(ActionSurge, n) -> sprintf "%s per short rest you can take two actions in a single turn" (times n)
    | QuantizedFeature(ExtraAttack, n) -> sprintf "When you take the Attack action you can attack %d times instead of only once" (n+1)
    | QuantizedFeature(Indomitable, n) -> sprintf "%s per long rest you can re-attempt a failed saving throw" (times n)
    | v -> sprintf "%A" v

type Consequent =
    | Grants of Feature // grants this other feature automatically
    | GrantsAll of Feature list // grants all of these features automatically
    | Choose of Feature list // grants ONE of the choices
    | ChooseN of N: int * Feature list // grants N of the choices

let featureGraph : (Feature * Consequent) [] = [|
    Race Dwarf, GrantsAll [ASI(Con, +2); PoisonResist; Darkvision]
    Race Dwarf, Choose [Subrace "Hill dwarf"; Subrace "Mountain dwarf"]
    Subrace "Hill dwarf", Grants (ASI(Wis, +1))
    Subrace "Hill dwarf", Grants (QuantizedFeature(ExtraHP, 1))
    Subrace "Mountain dwarf", Grants (ASI(Str, +2))
    Subrace "Mountain dwarf", Grants MediumArmorProficiency
    Race Elf, Choose [Subrace "Wood elf"; Subrace "High elf"]
    Race Elf, Grants (ASI(Dex, +2))
    Race Elf, Grants CharmResist
    Race Elf, Grants NoSleep
    Race Elf, Grants Darkvision
    Subrace "Wood elf", Grants (QuantizedFeature(Faster, 10))
    Subrace "Wood elf", Grants (ASI(Wis, +1))
    Subrace "High elf", Grants (ASI(Int, +1))
    Subrace "High elf", Grants (ExtraCantrip Wizards)
    Race HalfElf, Grants (ASI (Cha, +2))
    Race HalfElf, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1)])
    Race HalfElf, Grants CharmResist
    Race HalfElf, Grants NoSleep
    Race Human, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1); ASI (Cha, +1)])
    Race Human, Grants Feat
    Feat, Choose [Sharpshooter; GreatWeaponMaster; HeavyArmorMaster; DefensiveDuelist]
    HeavyArmorMaster, Grants (ASI (Str, +1))
    ChooseSplitASI, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1); ASI (Cha, +1)])
    ChooseFullASI, Choose [ASI (Str, +2); ASI (Dex, +2); ASI (Con, +2); ASI (Int, +2); ASI (Wis, +2); ASI (Cha, +2)]
    FeatOrASI, Choose [Feat; ChooseFullASI; ChooseSplitASI]
    |]



type Trait = {
    name: string
    description: string
}

type Level = {
    hitDieSize: Number
    traits: Consequent list
}

type CharacterClass = {
    name: string
    savingThrowProficiencies: Stat * Stat
    levels: Level list
}

type Comparison = AtLeast | AtMost
type Prereq = StatPrereq of Stat * Comparison * Number | RacePrereq of Race

type Template = {
    prereqs: Prereq list
    name: string
    priorities: (Stat * Number) list
    build: CharacterClass
}

type Charsheet = {
    name: string
    stats: Stats
    characterClass: CharacterClass
    xp: Number
    level: Number
    AC: Number
    HP: Number
    features: Feature list
}

let fighter = {
    CharacterClass.name = "Fighter"
    savingThrowProficiencies = (Str, Con)
    levels = [
        for i in 1..20 do
            match i with
            | 4 | 6 | 8 | 12 | 14 | 16 | 19 ->
                [Grants FeatOrASI]
            | 5 -> [Grants (QuantizedFeature(ExtraAttack, 1))]
            | 11 -> [Grants (QuantizedFeature(ExtraAttack, 2))]
            | 20 -> [Grants (QuantizedFeature(ExtraAttack, 3))]
            | _ -> []
        ] |> List.mapi (fun i traits -> { hitDieSize = 10; traits = Grants (QuantizedFeature(SecondWind, i+1))::traits })
}

type FixedPointOrOffer<'t, 'offer> = FixedPoint of 't | Offer of 'offer

module Feature =
    // ensure that only the highest-level version of any quantized feature is retained
    let consolidate (features: Feature list) =
        features |> List.filter (function QuantizedFeature(tag, n) -> not (features |> List.exists(function QuantizedFeature(tag', n') when tag = tag' -> n' > n | _ -> false)) | _ -> true)
                    |> List.fold (fun accum item -> match item with QuantizedFeature(tag, n) -> (if accum |> List.contains item then accum else item::accum) | _ -> item::accum) []
    let collectGrants (offers: Consequent list) =
        offers |> List.collect(function Grants f -> [f] | GrantsAll fs -> fs | _ -> [])
open Feature

module Choice =
    let enumerate<'t, 'option> (getChoices: 't -> (int * 'option list) option) (chaining: 'option -> 't list) (roots: 't list) =
        ()
    let validate<'t, 'option> (getChoices: 't -> (int * 'option list) option) (chaining: 'option -> 't list) (roots: 't list) =
        ()

fighter.levels |> List.collect(fun l -> l.traits |> collectGrants) |> consolidate
