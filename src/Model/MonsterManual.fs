module Model.MonsterManual

let menagerie = [
    "Orc [male:Mordor] hp:15 ac:13 attacks: +4 for d12+3"
    "Gargoyle 'Gargoyle' hp: 52 ac:15 attacks: +4 for d8+2 +4 for d8+2"
    "Skeleton 'Skeleton' hp: 13 ac:13 attacks: +4 for d6+2"
    "Zombie 'Zombie' hp: 22 ac:8 attacks: +4 for d6+2"
    ]

let lookup name =
    // ugly, inefficient hack: lookup in list using StartsWith. TODO: find something more elegant
    match menagerie |> List.tryFind(fun t -> t.StartsWith name) with
    | Some t -> Battle.Parse.statblock t
    | _ -> Battle.Parse.statblock "Unknown 'Unknown' ac:13 attacks: +4 for d12+3"
