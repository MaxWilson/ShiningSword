#if INTERACTIVE
#I __SOURCE_DIRECTORY__
#I ".."
#I @"..\Core"
#load "Common.fs"
#else
module Scratch
#endif

let rand = System.Random()
type Rand = Static of int | Rand of nDice: int * diceSize: int * rest: Rand option
    with
        member this.roll() =
            let rec roll accum = function
                | Static x -> accum + x
                | Rand (nDice, diceSize, rest) ->
                    let v = List.init nDice (fun _ -> 1 + rand.Next diceSize) |> List.sum
                    match rest with
                    | None -> accum + v
                    | Some rest -> roll (accum + v) rest
            roll 0 this
        static member roll (this: Rand) = this.roll()
        static member create (nDice, diceSize) = Rand(nDice, diceSize, None)
        static member create (nDice, diceSize, bonus) = Rand(nDice, diceSize, Some (Static bonus))

type Datum = Text of string | Number of float | Bool of bool | Nested of string | Rand of Rand
and Row = Map<string, Datum>
and World = {
    mutable world: Map<string, Row>
    }
    with
    static member fresh = { world = Map.empty }
type RowUpdate = { id: string; data: (string * Datum) list }
let updatedRow data row =
    let mutable row = row
    for (key, value) in data do
        row <- Map.add key value row
    row
let assign (data: RowUpdate list) (world: World) : unit =
    for { id = id; data = rowData } in data do
        let row = world.world |> Map.tryFind id |> Option.defaultValue Map.empty
        let row' = updatedRow rowData row
        world.world <- world.world |> Map.add id row'
type World with
    static member freshFrom rows =
        let w = World.fresh
        assign rows w
        w
type Constructor<'args, 'Type> = {
    create: 'args -> 'Type
    extract: 'Type -> 'args option
    }

let ctor<'args, 'Type>(create, extract) : Constructor<'args, 'Type> = { create = create; extract = extract }
type Property<'Type>(name: string, ctor: Constructor<'Type, Datum>) =
    member this.Name = name
    member this.Ctor = ctor
let textProp name = Property(name, ctor(Text, function Text x -> Some x | _ -> None))
let numProp name = Property(name, ctor(Number, function Number x -> Some x | _ -> None))
let randProp name = Property(name, ctor(Rand, function Rand x -> Some x | _ -> None))
let Name = textProp "ST"
let ST = numProp "ST"
let DX = numProp "DX"
let IQ = numProp "IQ"
let HT = numProp "HT"
let HP = numProp "HP"
let DR = numProp "DR"
let Damage = randProp "Damage"
let lookup (prop: 't Property) id world = world.world[id][prop.Name] |> prop.Ctor.extract
let damageOf =
    let baseDmg(n, d, bonus) b = Rand.create(n, d, bonus + b)
    function
        | 10 -> baseDmg(1,6,0)
        | 20 -> baseDmg(3,6,2)
        | v -> notImpl v
let damage st bonus = Damage, (damageOf st bonus)
type DataBuilder() = // syntactic sugar for setting property values
    member this.Yield (prop: Property<'Type>, data) =
         [prop.Name, prop.Ctor.create data]
    member this.Zero() = []
    member this.Combine (a, b) = a @ b
    member this.Delay f = f()
let data = DataBuilder()
let w =
    World.freshFrom [
        { id = "Uncle Bob"; data = data { ST, 10; IQ, 16; HP, 13; damage 10 2; HT, 12 } }// explicitly setting damage probably isn't quite right, should be derived from ST and equipment
        { id = "Ogre 1"; data = data { ST, 20; IQ, 8; HP, 30; DR, 4; damage 20 0 } }
        ]

let apply = function Some(r: Rand) -> r.roll() | _ -> notImpl()
w |> lookup Damage "Uncle Bob" |> apply
w |> lookup Damage "Ogre 1" |> apply

