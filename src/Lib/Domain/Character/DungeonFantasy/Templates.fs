module Domain.Character.DungeonFantasy.Templates
open Domain.Ribbit.Properties
open Domain.Character.DungeonFantasy.TraitsAndAttributes
open Fable.Core

type ChoiceType = Leveled | Selection
type 't ChoiceOption = ChoiceType * ('t list)
type LabeledChoiceOption = (Any * string) ChoiceOption
type 'trait1 Choose =
    abstract generate: Polymorphic<Constructor<Any, 'trait1> * LabeledChoiceOption, Any> -> Any

type Choose<'arg, 'trait1>(ctor: Constructor<'arg, 'trait1>, values: 'arg ChoiceOption, ?formatter: string * 'arg -> string) =
    let packArg, unpackArg = viaAny<'arg>()
    let boxedCtor: Constructor<Any, 'trait1> = { name = ctor.name; create = (unpackArg >> ctor.create); extract = ctor.extract >> Option.map packArg }
    let boxedValuesAndLabels: (Any * string) list =
        [   for v in snd values do
                packArg v, match formatter with Some format -> format(ctor.name.Value, v) | None -> $"{v}"
            ]
    interface 'trait1 Choose with
        member this.generate (f: Polymorphic<Constructor<Any, 'trait1> * LabeledChoiceOption, Any>) =
            f.Apply(boxedCtor, (fst values, boxedValuesAndLabels))

type 'trait1 Choose2D =
    abstract generate: Polymorphic<Constructor<Any * Any, 'trait1> * LabeledChoiceOption * LabeledChoiceOption, Any> -> Any

type Choose2D<'arg1, 'arg2, 'trait1>(ctor: Constructor<'arg1 * 'arg2, 'trait1>, values1: 'arg1 ChoiceOption, values2: 'arg2 ChoiceOption, ?formatter1: string * 'arg1 -> string, ?formatter2: string * 'arg2 -> string) =
    let packArg1, unpackArg1 = viaAny<'arg1>()
    let packArg2, unpackArg2 = viaAny<'arg2>()
    let boxedCtor: Constructor<Any * Any, 'trait1> = { name = ctor.name; create = (fun (arg1, arg2) -> ctor.create(unpackArg1 arg1, unpackArg2 arg2)); extract = (fun packed -> match ctor.extract packed with Some (arg1, arg2) -> Some (packArg1 arg1, packArg2 arg2) | None -> None) }
    let boxedValuesAndLabels1: (Any * string) list =
        [   for v in snd values1 do
                packArg1 v, match formatter1 with Some format -> format(ctor.name.Value, v) | None -> $"{v}"
            ]
    let boxedValuesAndLabels2: (Any * string) list =
        [   for v in snd values2 do
                packArg2 v, match formatter2 with Some format -> format(ctor.name.Value, v) | None -> $"{v}"
            ]
    interface 'trait1 Choose2D with
        member this.generate (f: Polymorphic<Constructor<Any * Any, 'trait1> * LabeledChoiceOption * LabeledChoiceOption, Any>) =
            f.Apply(boxedCtor, (fst values1, boxedValuesAndLabels1), (fst values2, boxedValuesAndLabels2))

// Each template OneResult can be in one of several states:
// selected-and-ready, selected-but-incomplete, and not selected.
// So for example a Binary: Combat Reflexes can only be
// selected or unselected, but a Choose: Sense of Duty [Adventuring Companions; Nature]
// needs more data before it can be ready.
// A Template.Many can hold multiple Template.OneResults and allow more than
// one to be selected-and-ready.
type Metadata = { label: string option; keySegment: string option }
    with
    static member fresh = { label = None; keySegment = None }
    static member label' v = { Metadata.fresh with label = Some v; keySegment = Some v }
    static member labelOnly v = { Metadata.fresh with label = Some v }
    static member key' v = { Metadata.fresh with keySegment = Some v }
type 't Many =
    | Aggregate of Metadata * 't Many list
    | ChoosePackage of (Metadata * 't Many) list
    | GrantItems of 't Many
    | Items of Metadata * 't OneResult list
    | Budget of int * Metadata * 't OneResult list
    | NestedBudgets of totalBudget:int * Metadata * suggestions:(int option * Metadata * 't OneResult list) list
and 't OneResult =
    | Binary of Metadata * 't
    | Choose of Metadata * 't Choose
    | Choose2D of Metadata * 't Choose2D
    | ChooseWithStringInput of Metadata * Constructor<string, 't> * placeholder:string
    | Grant of Metadata * 't OneResult
    | ChooseOneFromHierarchy of Metadata * 't OneHierarchy
and 't OneHierarchy =
    | Const of Metadata * 't
    | Constructed of Metadata * 't Choose

type Create =
    static member binary v = Binary(Metadata.key' (v.ToString()), v)
    static member chooseOne (ctor: Constructor<_,_>, options, ?formatter) = Choose(Metadata.key' ctor.name.Value, new Choose<_,_>(ctor, (Selection, options), ?formatter=formatter))
    static member chooseLevels(ctor: Constructor<_,_>, options, ?formatter) = Choose(Metadata.key' ctor.name.Value, new Choose<_,_>(ctor, (Leveled, options), ?formatter=formatter))
    static member choose2D(ctor: Constructor<_,_>, arg1Options, arg2Options, ?formatter1, ?formatter2) = Choose2D(Metadata.key' ctor.name.Value, new Choose2D<_,_,_>(ctor, (Selection, arg1Options), (Selection, arg2Options), ?formatter1=formatter1, ?formatter2=formatter2))
    static member choose2D(ctor: Constructor<_,_>, arg1Options: _ ChoiceOption, arg2Options: _ ChoiceOption, ?formatter1, ?formatter2) = Choose2D(Metadata.key' ctor.name.Value, new Choose2D<_,_,_>(ctor, arg1Options, arg2Options, ?formatter1=formatter1, ?formatter2=formatter2))
    static member leveled options = (Leveled, options)
    static member selection options = (Selection, options)
    static member chooseWithStringInput(ctor: Constructor<_,_>, placeholderText) = ChooseWithStringInput(Metadata.key' ctor.name.Value, ctor, placeholderText)
    static member grant (choice: 't OneResult) = Grant(Metadata.fresh, choice)
    static member chooseOneFromHierarchy keySegment v = ChooseOneFromHierarchy(Metadata.key' keySegment, v)
    static member leaf v = Leaf(Metadata.fresh, v)
    static member interior v = Interior(Metadata.fresh, v)

    static member aggregate choices = Aggregate(Metadata.fresh, choices)
    static member aggregate (label: string) = fun choices -> Aggregate(Metadata.label' label, choices)
    static member items choices = Items(Metadata.fresh, choices)
    static member items (label: string) = fun choices -> Items(Metadata.label' label, choices)
    static member grantAll (choices: 't OneResult list) = GrantItems(Create.items choices)
    static member grantAll (label: string) = fun choices -> GrantItems(Items(Metadata.labelOnly label, choices))
    static member budget budget (label: string) choices = Budget(budget, Metadata.label' label, choices)
    static member nestedBudgets totalBudget label (suggestions: (int option * _ OneResult list) list) = NestedBudgets(totalBudget, Metadata.label' label, suggestions |> List.map (fun (budget, choices) -> budget, Metadata.fresh, choices))
    static member nestedBudgets' totalBudget label (suggestions: (int option * string * _ OneResult list) list) = NestedBudgets(totalBudget, Metadata.label' label, suggestions |> List.map (fun (budget, label, choices) -> budget, Metadata.label' label, choices))
    static member choosePackage aggregates = ChoosePackage(aggregates)

open Data
module Menus =
    open TraitsAndAttributes.Ctor
    let tuple2bind1 name arg1 = namedCtor(name, (fun arg2 -> arg1, arg2), function (_, arg2) -> Some arg2)
    let StatBonus stat =
        (tuple2bind1 $"Extra {stat}" stat)
            => (ctor(StatBonus, function StatBonus(stat, n) -> Some (stat, n) | _ -> None))
    type Convert =
        static member Trait (t: Trait) = Chosen.Trait t
        static member Trait (ctor: Constructor<_,_>) = ctor => Common.Ctor.ctor(Trait, function Trait t -> Some t | _ -> None)
    open type Convert
    let thunktor v = ctor(thunk v, function v2 when v = v2 -> Some () | _ -> None)
    let severity = [Mild; Moderate; Serious; Severe]
    open type Create
    let allDisadvantages =
        [   chooseLevels (Chummy |> Trait, [ChummyLevel.Standard; Gregarious])
            chooseLevels (CompulsiveCarousing |> Trait, severity)
            chooseLevels (CompulsiveSpending |> Trait, severity)
            chooseLevels (Greed |> Trait, severity)
            chooseLevels (Impulsiveness |> Trait, severity)
            chooseLevels (Jealousy |> Trait, severity)
            chooseLevels (Lecherousness |> Trait, severity)
            binary(OneEye |> Trait)
            chooseLevels (Overconfidence |> Trait, severity)
            chooseLevels (SenseOfDuty |> Trait, [AdventuringCompanions])
            chooseLevels (ShortAttentionSpan |> Trait, severity)
            chooseLevels (Trickster |> Trait, severity)
            binary (Wounded |> Trait)
            ]
    let Speed =
        namedCtor("Extra Speed", (fun n -> Data.StatBonus(SpeedTimesFour, n*4)), function Data.StatBonus(stat, n) -> Some (n/4) | _ -> None)
    let showBonuses = (fun (ctorName, n) -> $"%+d{n}")
    open type Create
    let swash = aggregate [
        let swashMeleeWeapons = [Broadsword; Rapier; Saber; Shortsword; Smallsword; MainGauche]
        let label' = Metadata.label'
        grantAll "Automatic" [
            binary (CombatReflexes |> Trait)
            chooseLevels (Luck |> Trait, [Standard])
            choose2D (Ctor.EnhancedParry |> Trait, leveled [1], selection swashMeleeWeapons, formatter1=(fun (_,arg) -> $"%+d{arg}"))
            chooseWithStringInput (WeaponBond |> Trait, "Describe")
            chooseOne ({ (OneWeapon => WeaponMaster) with name = Some "Weapon Master" } |> Trait, [Broadsword; Rapier; Saber; Shortsword; Smallsword; MainGauche])
            ]
        budget 60 "Advantages" [
            chooseLevels(StatBonus HP, [1..6], showBonuses)
            chooseLevels(StatBonus DX, [1..3], showBonuses)
            chooseLevels(Speed, [1..3], showBonuses)
            binary(Trait Ambidexterity)
            chooseLevels(Appearance |> Trait, [Attractive;Beautiful;VeryBeautiful])
            chooseLevels(ArmorFamiliarity |> Trait, [1..4])
            chooseLevels(Charisma |> Trait, [1..4], showBonuses)
            binary(Trait Daredevil)
            chooseLevels(EnhancedBlock |> Trait, [1..3])
            binary(Trait.EnhancedDodge 1 |> Trait)
            choose2D(EnhancedParry |> Trait, leveled [2..3], selection swashMeleeWeapons)
            binary(Trait EnhancedTimeSense)
            binary(Trait EveryOnesACritical)
            chooseLevels(ExtraAttack |> Trait, [1..2])
            chooseLevels(Luck |> Trait, [Extraordinary; Ridiculous])
            binary(Trait GreatVoid)
            binary(Trait PerfectBalance)
            binary(Trait RapierWit)
            chooseLevels(Serendipity |> Trait, [1..3])
            chooseWithStringInput(SignatureGear |> Trait, "Describe")
            binary(Trait SpringingAttack)
            chooseLevels(StrikingST |> Trait, [1..2], showBonuses)
            chooseWithStringInput(TrademarkMove |> Trait, "Describe maneuver, weapon, hit locations, Rapid or Deceptive Strike")
            ChooseOneFromHierarchy(label' "Weapon Master", Interior(label' "Weapon master", [
                Leaf(label' "(All)", Trait.WeaponMaster(WeaponMasterFocus.All) |> Trait)
                Leaf(label' "(Swords)", Trait.WeaponMaster(WeaponMasterFocus.Swords) |> Trait)
                Leaf(label' "(Fencing Weapons)", Trait.WeaponMaster(WeaponMasterFocus.FencingWeapons) |> Trait)
                Interior(label' "(Weapon of choice)", [
                    for weapon in swashMeleeWeapons do
                        Leaf(label' $"({weapon})", Trait.WeaponMaster(WeaponMasterFocus.OneWeapon(weapon)) |> Trait)
                    ])
                Interior(label' "Two-weapon", [
                    for weapon in swashMeleeWeapons do
                        for weapon2 in [Shortsword; Smallsword; MainGauche; Knife] do
                        if weapon <> weapon2 then
                            Leaf(label' $"({weapon} and {weapon2})", Trait.WeaponMaster(WeaponMasterFocus.TwoWeapon(weapon, weapon2)) |> Trait)
                    ])
                ]))
            ]
        nestedBudgets -50 "Disadvantages" [
            Some -15,
                [   chooseOne (CodeOfHonor |> Trait, [Gentlemans; Outlaws])
                    chooseLevels (tuple2bind1 "Become best swordsman in the world" BecomeBestSwordsman => Obsession |> Trait, severity)
                    chooseOne (Vow |> Trait, [UseOnlyWeaponOfChoice; NeverRefuseAChallengeToCombat; ChallengeEverySwordsmanToCombat; NeverWearArmor], (fun (_, v) -> v.ToUncameledString()))
                    ]
            Some -35,
                [   chooseLevels (Chummy |> Trait, [ChummyLevel.Standard; Gregarious])
                    chooseLevels (CompulsiveCarousing |> Trait, severity)
                    chooseLevels (CompulsiveSpending |> Trait, severity)
                    chooseLevels (Greed |> Trait, severity)
                    chooseLevels (Impulsiveness |> Trait, severity)
                    chooseLevels (Jealousy |> Trait, severity)
                    chooseLevels (Lecherousness |> Trait, severity)
                    binary(OneEye |> Trait)
                    chooseLevels (Overconfidence |> Trait, severity)
                    chooseLevels (SenseOfDuty |> Trait, [AdventuringCompanions])
                    chooseLevels (ShortAttentionSpan |> Trait, severity)
                    chooseLevels (Trickster |> Trait, severity)
                    binary (Wounded |> Trait)
                    ]
            None, allDisadvantages
            ]
        ]

module Templates =
    open type Domain.Ribbit.Properties.Create
    let apply reason char (stat, value) =
        let delta = [Delta(value, reason)]
        match stat with
        | ST -> { char with ST = plus(char.ST, delta) }
        | DX -> { char with DX = plus(char.DX, delta) }
        | IQ -> { char with IQ = plus(char.IQ, delta) }
        | HT -> { char with HT = plus(char.HT, delta) }
        | Will -> { char with Will = plus(char.Will, delta) }
        | Per -> { char with Per = plus(char.Per, delta) }
        | SM -> { char with SM = plus(char.SM, delta) }
        | HP -> { char with HP = plus(char.HP, delta) }
        | FP -> { char with FP = plus(char.FP, delta) }
        | Move -> { char with Move = plus(char.Move, delta) }
        | SpeedTimesFour -> { char with SpeedTimesFour = plus(char.SpeedTimesFour, delta) }
        | Dodge -> { char with Dodge = plus(char.Dodge, delta) }

    type 't Package = {
        name: 't
        stats: (StatAddress * int) list
        traits: Trait list
        }
        with
        member this.apply stats =
            this.stats |> List.fold (apply (this.name.ToString() |> String.uncamel)) stats
        member this.displayName =
            this.name.ToString() |> String.uncamel
        static member Create(name, ?stats, ?traits) =
            {
            name = name
            stats = defaultArg stats []
            traits = traits |> Option.defaultValue []
            }

    let professions = Map.ofList [
        Barbarian, Package.Create<Profession>(Barbarian, [ST, +7; DX, +3; HT, +3; HP, +5; SpeedTimesFour, -2])
        Bard, Package.Create<Profession>(Bard, [ST, +1; DX, +2; IQ, +4; HT, +1; SpeedTimesFour, +1])
        Cleric, Package.Create<Profession>(Cleric, [ST, +2; DX, +2; IQ, +4; HT, +2])
        Druid, Package.Create<Profession>(Druid, [ST, +1; DX, +2; IQ, +4; HT, +3; SpeedTimesFour, -1])
        HolyWarrior, Package.Create<Profession>(HolyWarrior, [ST, +3; DX, +3; IQ, +2; HT, +3; Will, +2; SpeedTimesFour, -2])
        Knight, Package.Create<Profession>(Knight, [ST, +4; DX, +4; HT, +3; SpeedTimesFour, -3])
        MartialArtist, Package.Create<Profession>(MartialArtist, [ST, +1; DX, +6; HT, +2; Will, +1; Move, +1])
        Scout, Package.Create<Profession>(Scout, [ST, +1; DX, +4; IQ, +1; HT, +2; Per, +3; SpeedTimesFour, +2])
        Swashbuckler, Package.Create<Profession>(Swashbuckler, [ST, +1; DX, +5; HT, +3])
        Thief, Package.Create<Profession>(Thief, [ST, +1; DX, +5; IQ, +3; HT, +1; Per, +1; SpeedTimesFour, -2; Move, +1])
        Wizard, Package.Create<Profession>(Wizard, [DX, +2; IQ, +5; HT, +1; Per, -3; SpeedTimesFour, +1])
        ]
    let races = [
        10, Package<Race>.Create(Human)
        1, Package.Create<Race>(Catfolk, [ST, -1; DX, +1; Per, +1])
        2, Package.Create<Race>(Dwarf, [HT, +1; FP, +3; Move, -1])
        1, Package.Create<Race>(Elf, [ST, -1; DX, +1; Move, +1])
        2, Package.Create<Race>(HalfElf, [DX, +1])
        1, Package.Create<Race>(Gnome, [SM, -1; FP, +3; Move, -1])
        2, Package.Create<Race>(HalfOrc, [HT, +1; HP, +1])
        2, Package.Create<Race>(HalfOgre, [ST, +4; HT, +1; IQ, -1; Will, +1])
        1, Package.Create<Race>(Coleopteran, [ST, +1; IQ, -1; Per, +1])
        1, Package.Create<Race>(Halfling, [ST, -3; DX, +1; HT, +1; SM, -2; HP, +2; Move, -1])
        ]

    let menusFor = function
        | Swashbuckler -> Menus.swash
        | v -> Create.aggregate $"{v} Placeholder" []
