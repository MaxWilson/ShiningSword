module UI.Global

type OldPage =
  | Home
  | Counter
  | About

let describeAC fullInfo ac =
    let descr =
        if ac <= 8 then "Target practice"
        elif ac <= 11 then "Unarmored"
        elif ac <= 13 then "Lightly armored"
        elif ac <= 15 then "Moderately armored"
        elif ac <= 17 then "Heavily armored"
        elif ac <= 20 then "Very heavily armored"
        else "Walking fortress"
    if fullInfo then sprintf "%d (%s)" ac descr else descr

let describeStatus hp maxHp =
    match hp with
    | hp when hp <= 0 -> "Dead"
    | hp when hp <= (maxHp / 4) -> "Badly wounded"
    | hp when hp <= (maxHp * 3 / 4) -> "Wounded"
    | hp when hp < maxHp -> "Barely wounded"
    | _ -> "OK"
