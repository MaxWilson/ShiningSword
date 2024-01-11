[<AutoOpen>]
module UI.DFRPG.Chargen.Core
open Feliz
open Fable.React
type Stuff = Foo | Bar
type Weapon = Sword | Bow

[<StructuredFormatDisplay "DisplayString">]
type Trait = WeaponMaster of Weapon | CombatReflexes | Skill of string * bonus:int
    with
    member this.DisplayString =
        match this with
        | Skill(skill, bonus) -> $"{skill} %+d{bonus}"
        | otherwise -> String.uncamel (string otherwise)

type Multimap<'key, 'value when 'key:comparison and 'value: comparison> = Map<'key, Set<'value>>
type OrderedMultimap<'key, 'value when 'key:comparison and 'value: comparison> = Map<'key, List<'value>>
open Menus
type style = Feliz.style
let blank = OfferConfigCore.blank
type Op with
    static member skill (name: string, bonus: int) = Op.trait' ({ inner = OfferConfigCore.blank; toString = Some (fun (t: Trait) -> t.DisplayString) }, Skill(name, bonus))
    static member skill (name: string, levels: int list) = Op.level (name, { ctor = (fun bonus -> Skill(name, bonus)); toString = fun skill -> skill.DisplayString }, levels)

let advantage (advantage: Trait) =
        // unlike using trait' directly, this will use DisplayString so e.g. CombatReflexes gets uncameled
        Op.trait' ({ inner = OfferConfigCore.blank; toString = Some (fun (t: Trait) -> t.DisplayString) }, advantage)

open type Op

let label txt = { inner = { blank with label = Some txt }; toString = None }
let swash = [
    skill("Climbing", 1) |> promote
    skill("Stealth", [1..3]) |> promote
    budget(thunk 20, [
        advantage CombatReflexes
        skill("Acrobatics", [1..3])
        ])
    let mainWeapons = ["Rapier"; "Broadsword"; "Polearm"; "Two-handed sword"]
    let weaponsAt (bonus: int) = mainWeapons |> List.map (fun name -> skill(name, bonus))
    eitherN [
        either(label "Sword!", weaponsAt +5) |> promote
        and'(label "Sword and Dagger", [either(weaponsAt +4); skill("Main-gauche", +1)])
        and'(label "Sword and Shield", [either(weaponsAt +4); skill("Shield", +2)])
        ]
    eitherN [
        skill("Fast-draw (Sword)", +2) |> promote
        and'([skill("Fast-draw (Sword)", +1); skill("Fast-draw (Dagger)", +1)])
        ]
    ]

type DFRPGCharacter = { // stub
    traits: Trait Set
    }
    with static member fresh = { traits = Set.empty }

type Msg = RefreshedOutput of DFRPGCharacter
type Model = {
    currentOutput: DFRPGCharacter Option
    selections: Map<Key, MaybeLevel>
    }

let init _ = { currentOutput = None; selections = Map.empty }
let update msg model =
    match msg with
    | RefreshedOutput output -> { model with currentOutput = Some output }
