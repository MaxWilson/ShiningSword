module Domain.Character.DungeonFantasy.Templates
open Domain.Ribbit.Properties
open Domain.Character.DungeonFantasy.TraitsAndAttributes
open Fable.Core

[<Mangle>]
type OutputBuilder<'inputElement, 'outputElement> =
    // individual traits
    abstract binary: 'inputElement -> 'outputElement
    abstract binary: 'inputElement * string -> 'outputElement
    abstract chooseLevels: (int * int) -> 'outputElement

    // aggregations
    abstract aggregate: 'outputElement list -> 'outputElement
    abstract chooseUpToBudget: int -> 'outputElement list -> 'outputElement

module _Stats = // Implementation detail: would be private if it didn't get used in Package, which is public
                //   but other parts of code outside this file should view ST, etc. as properties, not as addresses
    type StatAddress = ST | DX | IQ | HT | Will | Per | SM | HP | FP | Move | SpeedTimesFour | Dodge
    open type Create

    let clear (char: Stats.Attributes) =
        {   Stats.fresh with
                ST = char.ST.clear
                DX = char.DX.clear
                IQ = char.IQ.clear
                HT = char.HT.clear
                SM = char.SM.clear }

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
let clear = _Stats.clear
open _Stats

module Menus =
    let HP = StatAddress.HP
    let swash (b: OutputBuilder<_,_>) = b.aggregate [
        b.chooseUpToBudget 60 [
            b.chooseLevels(1,6)
            ]
        b.binary(CombatReflexes)
        b.binary(EnhancedParry(Rapier), "Enhanced Parry (Rapier)")
        b.binary(HighPainThreshold)
        ]

module Templates =
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
