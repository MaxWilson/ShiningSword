module Domain.Character.DungeonFantasy.Templates

open Fable.Core

[<Mangle>]
type OutputBuilder<'inputElement, 'outputElement> =
    abstract binary: 'inputElement -> 'outputElement
    abstract binary: 'inputElement * string -> 'outputElement
    abstract aggregate: 'outputElement list -> 'outputElement

let swash (b: OutputBuilder<_,_>) = b.aggregate [
   b.binary(CombatReflexes)
   b.binary(EnhancedParry(Rapier), "Enhanced Parry (Rapier)")
   b.binary(HighPainThreshold)
   ]
