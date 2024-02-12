module Flags

// Normally we'd rely on namespacing to avoid conflicts, but in this case I anticipate opening the Flags module in most namespaces,
// and I don't want it to conflict with Menu keys or any other key regardless of the order of opens, so I'm choosing to call it a FlagKey.
// I'm still going to call everything else by a namespaced name though, Flag.Type and Flag.Values and so on.
type FlagKey = string
/// the actual flag values: the instance data, like "is this particular thing a Weapon Master for anything?"
type Values = {
    binaryFlags: Set<FlagKey>
    numericValues: Map<FlagKey, int>
    }

type Type = {
    mutable flags: Flag list
    }

// the data flags to check, like Combat Reflexes and Weapon Master. NOT the actual per-creature instance values, which are under Flag.Values instead.
and Flag(name: string, type': Type) as this =
    do type'.flags <- this :: type'.flags
    member this.Key = name
    member this.Values = name
