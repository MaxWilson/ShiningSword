module Ribbit

type Datum = Text of string | Number of float | Bool of bool | Nested of string | Formula of (string -> World -> Datum)
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
let Name x = "ST", Text x
let st x = "ST", Number x
let iq x = "IQ", Number x
let hp x = "HP", Number x
let DR x = "DR", Number x
let num = function | Number x -> x | _ -> failwith "Expected number"
let lookup prop id world = world.world[id][prop]
let damage = "Damage", Formula (lookup "ST")
let w =
    World.freshFrom [
        { id = "Uncle Bob"; data = [st 10; iq 16; hp 13; damage] } // explicitly setting damage probably isn't quite right
        { id = "Ogre 1"; data = [st 20; iq 8; hp 30; DR 4; damage] }
        ]
// this is barely a concept sketch, without even a real damage formula or rolls
let apply id world = function Formula f -> f id world
w |> lookup "Damage" "Uncle Bob" |> apply "Uncle Bob" w |> num

