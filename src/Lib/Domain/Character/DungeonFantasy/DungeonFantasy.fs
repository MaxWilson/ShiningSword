[<AutoOpen>]
module Domain.Character.DungeonFantasy.Core

open Domain
open Domain.Ribbit.Properties
open Domain.Character

[<AutoOpen>]
module Stats =
    open type Create
    type Attributes = {
        ST: int Primary
        DX: int Primary
        IQ: int Primary
        HT: int Primary
        Will: int Secondary
        Per: int Secondary
        SZ: int Primary
        HP: int Secondary
        FP: int Secondary
        Move: int Secondary
        SpeedTimesFour: int Secondary
        Dodge: int Secondary
        }
    let freshFrom(st, dx, iq, ht) =
        {   ST = primary st
            DX = primary dx
            IQ = primary iq
            HT = primary ht
            Will = secondary()
            Per = secondary()
            SZ = primary 0
            HP = secondary()
            FP = secondary()
            Move = secondary()
            SpeedTimesFour = secondary()
            Dodge = secondary()
            }
    let fresh = freshFrom(10, 10, 10, 10)
    let ST char = Eval.eval char.ST
    let DX char = Eval.eval char.DX
    let IQ char = Eval.eval char.IQ
    let HT char = Eval.eval char.HT
    let Will char = Eval.evalAndCondense (IQ char, Some "IQ", char.Will)
    let Per char = Eval.evalAndCondense (IQ char, Some "IQ", char.Per)
    let SZ char = Eval.eval char.SZ
    let HP char = Eval.evalAndCondense(ST char, Some "ST", char.HP)
    let FP char = Eval.evalAndCondense(HT char, Some "HT", char.FP)
    let Speed char : float RValue =
        let ht = HT char
        let dx = DX char
        {   baseValue = ((Eval.sum ht + Eval.sum dx) |> float) / 4.
            description = Some $"(HT {ht |> Eval.sum} + DX {dx |> Eval.sum})/4"
            modifiers =
                char.SpeedTimesFour.modifiers |> List.map Eval.eval
                |> List.map (Tuple2.mapfst (float >> (flip (/) 4.)))
                }
    let Move char: int RValue =
        let speed = Speed char |> Eval.sum
        {   baseValue = speed |> int
            description = Some "Speed, rounded down"
            modifiers = char.Move.modifiers |> List.map Eval.eval
            }
    let Dodge char: int RValue =
        let speed = Speed char |> Eval.sum
        {   baseValue = speed |> int
            description = Some "Speed, rounded down"
            modifiers = char.Dodge.modifiers |> List.map Eval.eval
            }

type Race = Human | Catfolk | Dwarf | Elf | HalfElf | HalfOgre | Coleopteran | Halfling
type Profession = Barbarian | Bard | Cleric | Druid | HolyWarrior | Knight | MartialArtist | Scout | Swashbuckler | Thief | Wizard
type WeaponType = Unarmed | Rapier | Broadsword | Bow | MainGauche | Knife | Spear | Polearm | Staff
type Trait =
    | HighPainThreshold
    | CombatReflexes
    | EnhancedParry of WeaponType

type Skill =
    | Weapon of WeaponType
    | Stealth
    | Camouflage
    | Observation

module Templates =
    module Impl =
        type StatAddress = ST | DX | IQ | HT | Will | Per | SZ | HP | FP | Move | SpeedTimesFour | Dodge
        open type Create

        let clear (char: Stats.Attributes) =
            {   Stats.fresh with
                    ST = char.ST.clear
                    DX = char.DX.clear
                    IQ = char.IQ.clear
                    HT = char.HT.clear
                    SZ = char.HT.clear }

        let apply reason char (stat, value) =
            let delta = [Delta(value, reason)]
            match stat with
            | ST -> { char with ST = plus(char.ST, delta) }
            | DX -> { char with DX = plus(char.DX, delta) }
            | IQ -> { char with IQ = plus(char.IQ, delta) }
            | HT -> { char with HT = plus(char.HT, delta) }
            | Will -> { char with Will = plus(char.Will, delta) }
            | Per -> { char with Per = plus(char.Per, delta) }
            | SZ -> { char with SZ = plus(char.SZ, delta) }
            | HP -> { char with HP = plus(char.HP, delta) }
            | FP -> { char with FP = plus(char.FP, delta) }
            | Move -> { char with Move = plus(char.Move, delta) }
            | SpeedTimesFour -> { char with SpeedTimesFour = plus(char.SpeedTimesFour, delta) }
            | Dodge -> { char with Dodge = plus(char.Dodge, delta) }
    open Impl

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

    let human = Package<Race>.Create(Human)
    let catfolk = Package.Create<Race>(Catfolk, [ST, -1; DX, +1; Per, +1])
    let dwarf = Package.Create<Race>(Dwarf, [HT, +1; FP, +3; Move, -1])
    let elf = Package.Create<Race>(Elf, [ST, -1; DX, +1; Move, +1])
    let halfElf = Package.Create<Race>(HalfElf, [DX, +1])
    let halfOgre = Package.Create<Race>(HalfOgre, [ST, +4; HT, +1; IQ, -1; Will, +1])
    let coleopteran = Package.Create<Race>(Coleopteran, [ST, +1; IQ, -1; Per, +1])
    let halfling = Package.Create<Race>(Halfling, [ST, -3; DX, +1; HT, +1; SZ, -2; HP, +2; Move, -1])
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
    let races = [10, human; 1, catfolk; 2, dwarf; 1, elf; 2, halfElf; 2, halfOgre; 1, coleopteran; 1, halfling]

open Templates

type RandomizationMethod = NonRandom | Exponential | Average3d6
let rollStats method =
    match method with
    | NonRandom ->
        Stats.freshFrom(10, 10, 10, 10)
    | Exponential ->
        let rec exponentialHalt accum stepSize rate =
                if random.NextDouble() <= rate && abs accum <= 5 then
                    exponentialHalt (accum + stepSize) stepSize rate
                else
                    accum
        let gen() = 10 + exponentialHalt 0 (chooseRandom [-1; +1]) 0.5
        Stats.freshFrom(gen(), gen(), gen(), gen())
    | Average3d6 ->
        let gen() = (List.init 6 (thunk1 rand 6) |> List.sum) / 2
        Stats.freshFrom(gen(), gen(), gen(), gen())

type Character = {
    id: string
    header: RoleplayingData
    profession: Profession
    canChangeRace: bool
    race: Race
    stats: Stats.Attributes
    traits: Trait list
    }

type 't Constraint = Arbitrary | Specific of 't

type Constraints = {
    randomizationMethod: RandomizationMethod
    race: Race Package Constraint option
    sex: Sex Constraint
    nationPreference: string option
    }
    with
    static member fresh = {
        randomizationMethod = NonRandom
        race = None
        sex = Arbitrary
        nationPreference = None
        }

let materialize fallback = function Arbitrary -> fallback() | (Specific v) -> v
let createRandom (c: Constraints) =
    let prof = chooseRandom professions.Keys
    let stats = rollStats c.randomizationMethod
    let race =
        match c.race with
        | None | Some Arbitrary -> chooseWeightedRandom races
        | Some (Specific r) -> r
    let sex = c.sex |> materialize (thunk1 chooseRandom [Male; Female])
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
    {   id = System.Guid.NewGuid().ToString()
        header = rp
        profession = prof
        race = race.name
        canChangeRace = c.race.IsNone // can only change race if it was unconstrained originally, i.e. nonrandom stats
        stats = stats |> Templates.Impl.clear |> race.apply |> professions[prof].apply
        traits = race.traits @ professions[prof].traits
        }

let resetStatsAndTraits (char: Character) =
    let race = Templates.races |> List.find (fun (_, r) -> r.name = char.race) |> snd
    let prof = professions[char.profession]
    { char
        with
        stats = char.stats |> Templates.Impl.clear |> race.apply |> prof.apply
        traits = race.traits @ prof.traits}

let changeProfession (char: Character) prof =
    { char with profession = prof } |> resetStatsAndTraits

let changeRace (char: Character) race =
    { char with race = race } |> resetStatsAndTraits

