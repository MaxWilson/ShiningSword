module UI.DFRPG.Chargen

type Stuff = Foo | Bar
type Weapon = Sword | Bow
type Trait = WeaponMaster of Weapon | CombatReflexes | Skill of string * int

let swash() = [
    notImpl "Need to represent the choices for a swashbuckler, with constraints and nesting and budgets such"
    ]

let init _ = Foo
let update msg1 model = model
