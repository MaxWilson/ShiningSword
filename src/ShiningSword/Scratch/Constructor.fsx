// This script is for experimenting with explicit constructors that can also be used for pattern matching.
#I __SOURCE_DIRECTORY__
#I ".."
#I "..\Core"
#load @"Optics.fs"
#load @"Common.fs"

type Constructor<'args, 'Type> = {
    create: 'args -> 'Type
    extract: 'Type -> 'args option
    }

let ctor<'args, 'Type>(create, extract) : Constructor<'args, 'Type> = { create = create; extract = extract }

type Employee =
    | Minion of id: int
    | Boss of name: string * salary: int

type BusinessComponent =
    | Employee of Employee
    | Equipment of description: string

let minion = { create = Minion; extract = function Minion id -> Some id | _ -> None }
let boss = { create = Boss; extract = function Boss (name, salary) -> Some (name, salary) | _ -> None }
let employee = ctor(Employee, function Employee e -> Some e | _ -> None)
let equipment = ctor(Equipment, function Equipment d -> Some d | _ -> None)

module Tuple2 =
    let ctor() = ctor((fun (a, b) -> (a, b)), Some)

let (=>) (lhs: Constructor<_, 't1>) (rhs: Constructor<'t1, _>) =
    ctor(rhs.create << lhs.create, rhs.extract >> Option.bind lhs.extract)

let assets =
    [ Minion 1; Minion 2; Minion 3; Boss("Gru", 40000) ] |> List.map employee.create
    |> List.append [ Equipment "Laser"; Equipment "Computer" ]

for e in assets do
    let (|Minion|_|) = (minion => employee).extract
    let (|Boss|_|) = (boss => employee).extract
    let (|Equipment|_|) = equipment.extract
    printfn "%s" <|
        match e with
        | Minion id -> $"Minion {id}"
        | Boss(name, salary) -> $"Boss {name} with salary {salary}"
        | Equipment(txt) -> txt
        | _ -> shouldntHappen()

