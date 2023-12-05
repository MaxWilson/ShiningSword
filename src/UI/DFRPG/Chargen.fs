module UI.DFRPG.Chargen

type Stuff = Foo | Bar
type Weapon = Sword | Bow
type Trait = WeaponMaster of Weapon | CombatReflexes | Skill of string * bonus:int

type Offer = Offer of Trait | OneOf of Offer list
    with
    static member skill(name, bonus) = Offer(Skill(name, bonus))
    static member skill(name, bonusRange) = bonusRange |> List.map (fun bonus -> Offer(Skill(name, bonus))) |> OneOf
    static member either(offerings) = offerings |> OneOf

type Offers = ChooseN of int * Offer list | ChooseBudget of int * Offer list
    with
    member this.renderedOfferings() = notImpl()

let swash() = [
    ChooseBudget(20, [
        Offer.skill("Acrobatics", 2)
        Offer.skill("Acrobatics", [1..3])
        Offer.either([
            Offer.skill("Rapier", 20)
            Offer.skill("Broadsword", 20)
            ])
        ])
    ]

type Model = {
    template: Offers list
    }

let init _ = { template = swash() }
let update msg1 model = model
