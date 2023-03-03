module Domain.Character.DungeonFantasy.Templates
open Domain.Ribbit.Properties
open Domain.Character.DungeonFantasy.TraitsAndAttributes
open Fable.Core

[<Mangle>]
type OutputBuilder<'choice, 'reactElement> = // for clairty, might as well name the elements for their intended role, even though 'reactElement is not strictly required to be a ReactElement
    // individual traits
    abstract grant: 'choice -> 'reactElement
    abstract binary: 'choice -> 'reactElement
    abstract binary: 'choice * string -> 'reactElement
    abstract chooseWithStringInput: string * (string -> 'choice) -> 'reactElement
    abstract chooseLevels: (('arg -> 'choice) * 'arg list) -> 'reactElement
    abstract choose2D: (('arg1 * 'arg2 -> 'choice) * 'arg1 list * 'arg2 list) -> 'reactElement
    abstract grantOne: (('arg -> 'choice) * 'arg list) -> 'reactElement
    abstract grantWithStringInput: string * (string -> 'choice) -> 'reactElement

    // aggregations
    abstract aggregate: 'reactElement list -> 'reactElement
    abstract chooseUpToBudget: int -> 'reactElement list -> 'reactElement

module _Stats = // Implementation detail: would be private if it didn't get used in Package, which is public
                //   but other parts of code outside this file should view ST, etc. as properties, not as addresses
    type StatAddress = ST | DX | IQ | HT | Will | Per | SM | HP | FP | Move | SpeedTimesFour | Dodge
    open type Create

open _Stats

module Menus =
    type Chosen =
        | StatBonus of StatAddress * int
        | Trait of Trait
    let StatBonus stat n = StatBonus(stat, n)
    let swash (b: OutputBuilder<_,'reactElement>) = b.aggregate [
        let swashMeleeWeapons = [Broadsword; Rapier; Saber; Shortsword; Smallsword; MainGauche]
        b.aggregate [
            b.grant (Trait CombatReflexes)
            b.grant (Luck Standard |> Trait)
            b.grantOne(Tuple2.create 1 >> EnhancedParry >> Trait, swashMeleeWeapons)
            b.grantWithStringInput("Describe", WeaponBond >> Trait)
            b.grantOne((fun weapon -> WeaponMaster(notImpl "oneWeapon(weapon)") |> Trait), swashMeleeWeapons)
            ]
        b.chooseUpToBudget 60 [
            b.chooseLevels(StatBonus HP, [1..6])
            b.chooseLevels(StatBonus DX, [1..3])
            b.chooseLevels(StatBonus SpeedTimesFour, [4..4..12])
            b.binary(Trait Ambidexterity)
            b.chooseLevels(Appearance >> Trait, [Attractive;Beautiful;VeryBeautiful])
            b.chooseLevels(ArmorFamiliarity >> Trait, [1..4])
            b.chooseLevels(Charisma >> Trait, [1..4])
            b.chooseLevels(Charisma >> Trait, [1..4])
            b.binary(Trait Daredevil)
            b.chooseLevels(EnhancedBlock >> Trait, [1..3])
            b.binary(EnhancedDodge 1 |> Trait)
            b.choose2D(EnhancedParry >> Trait, [2..3], swashMeleeWeapons)
            b.binary(Trait EnhancedTimeSense)
            b.binary(Trait EveryOnesACritical)
            b.chooseLevels(ExtraAttack >> Trait, [1..2])
            b.chooseLevels(Luck >> Trait, [Extraordinary; Ridiculous])
            b.binary(Trait GreatVoid)
            b.binary(Trait PerfectBalance)
            b.binary(Trait RapierWit)
            b.binary(Trait Serendipity)
            b.chooseWithStringInput("Describe", SignatureGear >> Trait)
            b.binary(Trait SpringingAttack)
            b.chooseLevels(StrikingST >> Trait, [1..2])
            b.chooseWithStringInput("Describe maneuver, weapon, hit locations, Rapid or Deceptive Strike",
                TrademarkMove >> Trait)
            notImpl "Weapon master: 2 weapon, fencing, swords"
            ]
        ]

module Templates =
    open type Create
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

    let menusFor builder = function
        | Swashbuckler -> Menus.swash builder
        | v -> notImpl v
