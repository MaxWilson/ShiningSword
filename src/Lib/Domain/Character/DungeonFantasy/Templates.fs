module Domain.Character.DungeonFantasy.Templates

type OutputBuilder<'inputElement, 'outputElement> =
    abstract binary: 'inputElement -> 'outputElement
    abstract binaryL: 'inputElement * string -> 'outputElement
    abstract aggregate: 'outputElement list -> 'outputElement

let swash (b: OutputBuilder<_,_>) = b.aggregate [
   b.binary(CombatReflexes)
   b.binaryL(EnhancedParry(Rapier), "Enhanced Parry (Rapier)")
   b.binary(HighPainThreshold)
   ]
