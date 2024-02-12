module Domain.DFRPG.Templates
open Menus

module Flags =
    open Flags
    let allFlags = { flags = [] }
    let flag name = Flag(name, allFlags)
    type FlagOutput = Literal of FlagKey | Bonus of FlagKey * int
    let toString (f:FlagOutput) =
            match f with
            | Literal s -> s
            | Bonus(s, n) -> $"%+d{n} {s}"
    let ST = flag "ST"
    let DX = flag "DX"
    let IQ = flag "IQ"
    let HT = flag "HT"
    let Ambi = flag "Ambidexterity"
    let CombatReflexes = flag "Combat Reflexes"
    let ETS = flag "Enhanced Time Sense"
    let HPT = flag "High Pain Threshold"
    let Luck = flag "Luck"
open Flags


type Op with
    static member adv (flag:Flag) =
        Op.trait' ({ blank() with toString = Some Flags.toString }, Literal flag.Key)
    static member bonus (flag:Flag) n =
        Op.trait' ({ blank() with toString = Some Flags.toString }, Bonus(flag.Key, n))
    static member bonusRange (flag:Flag) (ns: int list) =
        let spec : LevelSpec<int, FlagOutput> = { toString = toString; ctor = fun n -> Bonus(flag.Key, n) }
        Op.level(flag.Key, spec, ns)
    static member chooseN n (options: _ OptionOffer list) =
        Op.eitherN(blank(), n, options)
open type Op
let swashbuckler: FlagOutput ListOffer list = [
    Op.and' [
        bonus ST 1
        bonus DX 5
        bonus HT 3
        adv CombatReflexes
        adv Luck
        ]
    Op.chooseN 4 [
        bonusRange ST [1..6]
        bonusRange DX [1..3]
        adv Ambi
        adv ETS
        ]
    ]