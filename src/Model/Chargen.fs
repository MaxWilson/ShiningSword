module Model.Chargen
open Model.Types


type Feature =
    | Race of Race
    | Subrace of string
    | ClassLevel of CharClass * int
    | HeavyArmorMaster
    | ASI of AbilityScore * int
    | GreatWeaponMaster
    | Sharpshooter
    | DefensiveDuelist
    | HeavyArmorDamageResistance of N:int
    | Faster of int
    | CharmResist
    | NoSleep
    | ExtraCantrip of SpellList
    | Feat

let descriptions = function
    | HeavyArmorDamageResistance n ->
        sprintf "Damage resistance %d vs. nonmagical damage while wearing heavy armor" n
    | ASI(ability, bonus) ->
        sprintf "+%d to %s" bonus (ability.ToString())
    | Race race ->
        sprintf "You are a %A" race
    | Subrace subrace ->
        sprintf "You are a %A" subrace
    | ClassLevel(cl, n) ->
        sprintf "You are a level %d or higher %A" n cl
    | Faster n ->
        sprintf "Your speed increases by %d' per round" n
    | CharmResist ->
        "You have advantage on saving throws against charm effects"
    | NoSleep ->
        "You do not need to sleep, nor can magic put you to sleep"
    | v -> sprintf "%A" v

type Consequent =
    | Grants of Feature // grants this other feature automatically
    | Choose of Feature list // grants ONE of the choices
    | ChooseN of N: int * Feature list // grants N of the choices

let featureGraph : (Feature * Consequent) [] = [|
    Race Elf, Choose [Subrace "Wood elf"; Subrace "High elf"]
    Race Elf, Grants (ASI(Dex, +2))
    Race Elf, Grants CharmResist
    Race Elf, Grants NoSleep
    Subrace "Wood elf", Grants (Faster 10)
    Subrace "Wood elf", Grants (ASI(Wis, +1))
    Subrace "High elf", Grants (ASI(Int, +1))
    Subrace "High elf", Grants (ExtraCantrip Wizard)
    Race HalfElf, Grants (ASI (Cha, +2))
    Race Human, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1)])
    Race HalfElf, Grants CharmResist
    Race HalfElf, Grants NoSleep
    Race Human, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1); ASI (Cha, +1)])
    Race Human, Grants Feat
    Feat, Choose [Sharpshooter; GreatWeaponMaster; HeavyArmorMaster; DefensiveDuelist]
    HeavyArmorMaster, Grants (ASI (Str, +1))
    |]
    
