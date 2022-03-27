/// A module for dynamically building interactive wizards that minimize the number of unnecessary questions they ask.
/// Key concepts:
///   DerivationRule: used to create DerivationRules
///   Choice: specific choice 
///   DerivationInstance: set of choices already made or in the process of being made, against a specific set of DerivationRules
///   Summary: from instance, yield a summary of traits
///   Setting<Trait>: a list of Traits and how they were derived. DerivationRules must be stored separately, as must the summarization logic.
module AutoWizard

type Choice<'trait0> =
    { options: 'trait0 list; numberAllowed: int; mustBeDistinct: bool; elideFromDisplayAndSummary: bool; autopick: bool }

let fresh options = { options = options; numberAllowed = 1; mustBeDistinct = false; elideFromDisplayAndSummary = false; autopick = false }
type DerivationRule<'trait0> = Rule of key:'trait0 * choice:Choice<'trait0> 
type DerivationRules<'trait0 when 'trait0: comparison> = Map<'trait0, Choice<'trait0> list>
type DerivationInstance<'trait0 when 'trait0: comparison> = Map<'trait0, int list list>
type Setting<'trait0 when 'trait0: comparison> = { instance: DerivationInstance<'trait0>; summary: 'trait0 list }

let (==>) (trait0: 'trait0) (options: 'trait0 list) =
    trait0, fresh options
let confer (trait0: 'trait0) (options: 'trait0 list) =
    trait0, { fresh options with numberAllowed = options.Length; autopick = true }

module Sample =
    type Stat = Str | Dex | Con | Int | Wis | Cha
    type Traits =
        | PC
        | Race
        | Elf
        | WoodElf
        | HighElf
        | DrowElf
        | Human
        | ASI of Stat * int
        | Feat
        | GWM
        | Tough
        | Lucky
        | Mobile
        | WizardCantrip
        | Cantrip of string
        | MaskOfTheWild
        | Faster of int
        | SunlightSensitivity
        | ImprovedDarkvision
        | SwordBowBonus of int
    let feats = [GWM;Tough;Lucky;Mobile]
    let rules = [
        PC, { fresh [Race] with elideFromDisplayAndSummary = true }
        Race ==> [Elf; Human]
        Human ==> feats
        let stats = [Str;Dex;Con;Int;Wis;Cha]
        Human, { fresh (stats |> List.map (fun x -> ASI (x,1))) with numberAllowed = 2; mustBeDistinct = true }
        confer Elf [SwordBowBonus 1]
        Elf ==> [HighElf; WoodElf; DrowElf]
        confer WoodElf [MaskOfTheWild; Faster 5]
        confer DrowElf [ImprovedDarkvision; SunlightSensitivity]
        ]

