module Flags

type FlagType = {
    mutable flags: Flag list
    }

and Flag(name: string, type': FlagType) as this =
    do type'.flags <- this :: type'.flags
    member this.Values = name
let allFlags = { flags = [] }
let flag name = Flag(name, allFlags)

let combatReflexes = flag "Combat Reflexes"
let highPainThreshold = flag "High Pain Threshold"