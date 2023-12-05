module UI.DFRPG.Chargen

type Stuff = Foo | Bar
type Weapon = Sword | Bow
type Trait = WeaponMaster of Weapon | CombatReflexes | Skill of string * int

type Constraint = unit -> bool // TODO: find the right type
let budget(points): Constraint = notImpl()
let chooseN(n): Constraint = notImpl()

type Offer = Offer of Trait | OneOf of Offer list
    with
    static member skill(name, points) = Offer(Skill(name, points))
    static member skill(name, pointRange) = pointRange |> List.map (fun points -> Offer(Skill(name, points))) |> OneOf
    static member either(offerings) = offerings |> OneOf

type Offers(constrain: Constraint, offers) =
    member this.renderedOfferings() = notImpl()

let swash() = [
    Offers(budget 20, [
        Offer.skill("Acrobatis", 2)
        Offer.skill("Acrobatis", [1;2;4])
        Offer.either([
            Offer.skill("Rapier", 20)
            Offer.skill("Broadsword", 20)
            ])
        ])
    Offers(notImpl "Need to represent the choices for a swashbuckler, with constraints and nesting and budgets such", notImpl())
    ]

let init _ = Foo
let update msg1 model = model
