module Domain.DFRPG.Templates
open Menus

module Flags =
    open Flags
    let allFlags = { flags = [] }
    let flag name = Flag(name, allFlags)

    let combatReflexes = flag "Combat Reflexes"
    let highPainThreshold = flag "High Pain Threshold"
    let ST = flag "ST"
    let DX = flag "DX"
    let IQ = flag "IQ"
    let HT = flag "HT"
    let luck = flag "Luck"
open Flags

type FlagOutput = Literal of FlagKey | Bonus of FlagKey * int

type Op with
    static member adv (flag:Flag) = Op.trait' (Literal flag.Key)
    static member bonus (flag:Flag) n = Op.trait' (Bonus(flag.Key, n))
open type Op
let swashbuckler: FlagOutput ListOffer list = [
    Op.and' [ bonus ST 1; bonus DX 5; bonus HT 3; adv combatReflexes; adv luck ]
    ]