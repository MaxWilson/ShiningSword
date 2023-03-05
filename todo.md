For a battle, we have several phases:

Populate (from stat blocks or CharSheets)
Declare actions
Execute actions
Apply effects
Show results
Apply ad-hoc effects
Export to CharSheets

Known bugs/feature gaps:

* Trademark Move should have constraints to make you specify maneuver and (for each attack) weapon, hit location, and whether you're using Deceptive Strike, Dual-Weapon Attack, or Rapid Strike. Or maybe it should just specify an attack routine by name.

* Signature Gear and Weapon Bond should be tied to gear in some specific way, maybe by a GUID.

* WeaponMaster is technically supposed to be allowed to be tied to weapon types, not weapon skills, e.g. using a Naginata with either Two-Handed Sword or Polearm is supposed to be legal, or a knife with Main-gauche or Thrown Weapons (Knife).

* RefineN/ChooseN needs to uncheck options when a new one is selected, so
that we don't go over the max of N.

* Budgets: should they be implemented at the visual layer, the data layer, or both? It seems like the visual layer is more flexible, for allowing budget overages and such, and I think DataBuilder still guarantees that only things currently displayed on the screen wind up in the final data, so visual layer enforcement should still work.
