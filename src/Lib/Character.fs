module Domain.Character

open DerivedTraits

type Stat = Str | Dex | Con | Int | Wis | Cha
    with static member All = [Str;Dex;Con;Int;Wis;Cha]
type Trait =
    | PC
    | Race
    | Human
    | Elf
    | Dwarf
    | WoodElf
    | HighElf
    | DrowElf
    | HillDwarf
    | MountainDwarf
    | StatMod of Stat * int
    | Feat
    | GWM
    | Tough
    | Lucky
    | Mobile
    | HeavyArmorMaster
    | BonusWizardCantrip
    | Cantrip of string
    | MaskOfTheWild
    | Faster of int
    | SunlightSensitivity
    | ImprovedDarkvision
    | SwordBowBonus of int
    | ShieldProficiency
    | LightArmorProficiency
    | MediumArmorProficiency
    | HeavyArmorProficiency
    | ExtraHPPerLevel of int
type Sex = Male | Female | Neither
type CharacterSheet = {
    name: string
    sex: Sex
    Str: int
    Dex: int
    Con: int
    Int: int
    Wis: int
    Cha: int
    originalRolls: int list
    // Storing the derivation instead of just the end result makes it easier to do things like add new traits on levelling up
    traits: Setting<Trait>
    }

let feats = [GWM;Tough;Lucky;Mobile;HeavyArmorMaster]
let rules =
    [
        PC, { fresh [Race] with elideFromDisplayAndSummary = true; autopick = true }
        Race ==> [Elf; Human; Dwarf]
        let stats = [Str;Dex;Con;Int;Wis;Cha]
        Human, { fresh (stats |> List.map (fun x -> StatMod (x,1))) with numberAllowed = 2; mustBeDistinct = true }
        confer Human [Feat]
        Feat ==> feats
        confer Elf [SwordBowBonus 1; StatMod (Dex,2)]
        Elf ==> [HighElf; WoodElf; DrowElf]
        confer HighElf [BonusWizardCantrip]
        confer WoodElf [MaskOfTheWild; Faster 5]
        confer DrowElf [ImprovedDarkvision; SunlightSensitivity]
        BonusWizardCantrip ==> [Cantrip "Fire Bolt"; Cantrip "Minor Illusion"; Cantrip "Blade Ward"; Cantrip "Toll the Dead"]
        confer Dwarf [StatMod (Con, 2); Faster -5]
        Dwarf ==> [HillDwarf; MountainDwarf]
        confer HillDwarf [ExtraHPPerLevel 1; StatMod(Wis, 1)]
        confer MountainDwarf [MediumArmorProficiency]
        ]
    |> rulesOf

// turn camel casing back into words with spaces, for display to user
let uncamel (str: string) =
    let caps = ['A'..'Z'] |> Set.ofSeq
    let lower = ['a'..'z'] |> Set.ofSeq
    let mutable spaceNeededBefore = []
    let mutable inWord = true
    for i in 1..str.Length-1 do
        match str[i] with
        | ' ' -> inWord <- false
        // When multiple caps are in a row, no spaces should be used, except before the last one if it's followed by a lowercase.
        // E.g. MySSNNumber => My SSN Number, but MySSN => My SSN not My SS N
        | letter when caps.Contains letter && inWord && ((caps.Contains str[i-1] |> not) || i+1 < str.Length && lower.Contains str[i+1])->
            spaceNeededBefore <- i::spaceNeededBefore
        | letter when System.Char.IsLetterOrDigit letter -> inWord <- true
        | _ -> ()
    let rec recur workingCopy spacesNeeded =
        match spacesNeeded with
        | [] -> workingCopy
        | index::rest ->
            recur $"{workingCopy[0..(index-1)]} {workingCopy[index..]}" rest
    recur str spaceNeededBefore

