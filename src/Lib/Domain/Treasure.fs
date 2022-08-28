module Domain.Treasure
open Domain.Random
open Domain.Metrics

// helper type so I can control the display order instead of being alphabetical
type Currency = Copper | Silver | Electrum | Gold | Platinum | Gems | Jewelry
// this doesn't properly belong in ribbit but because treasure is part of the aftermath of the combat,
// and because it's convenient to put TreasureType in the monster stat blocks, I'll allow it for now
type TreasureType = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z

let treasureValue treasureTypes =
    let mutable loot = []
    let incr description amt =
        loot <- loot @ [amt, description]
    let money multiplier description probability (roll: RollSpec) =
        if random.NextDouble() <= probability then (roll.roll() * multiplier) |> incr description
    let copper = money 1 Copper 1
    let silver = money 1 Silver 1
    let electrum = money 1 Electrum 1
    let gold = money 1 Gold 1
    let platinum = money 5 Platinum 1
    let copper1000 = money 1000 Copper
    let silver1000 = money 1000 Silver
    let electrum1000 = money 1000 Electrum
    let gold1000 = money 1000 Gold
    let platinum100 = money 100 Platinum
    let gems = money 1 Gems
    let jewelry = money 1 Jewelry
    let roll n d = RollSpec.create(n,d)
    let rollb n d (b:int) = RollSpec.create(n,d,b)
    for treasureType in treasureTypes do
        match treasureType with
        | A ->
            roll 1 6 |> copper1000 0.25
            roll 1 6 |> silver1000 0.3
            roll 1 6 |> electrum1000 0.35
            roll 1 10 |> gold1000 0.4
            roll 1 4 |> platinum100 0.25
            roll 4 10 |> gems 0.6
            roll 3 10 |> jewelry 0.5
        | B ->
            roll 1 8 |> copper1000 0.50
            roll 1 6 |> silver1000 0.25
            roll 1 4 |> electrum1000 0.25
            roll 1 3 |> gold1000 0.25
            roll 1 8 |> gems 0.3
            roll 1 4 |> jewelry 0.2
        | C ->
            roll 1 12 |> copper1000 0.20
            roll 1 6 |> silver1000 0.3
            roll 1 4 |> electrum1000 0.10
            roll 1 6 |> gems 0.25
            roll 1 3 |> jewelry 0.25
        | D ->
            roll 1 8 |> copper1000 0.10
            roll 1 2 |> silver1000 0.15
            roll 1 8 |> electrum1000 0.15
            roll 1 6 |> gold1000 0.5
            roll 1 10 |> gems 0.3
            roll 1 6 |> jewelry 0.25
        | E ->
            roll 1 10 |> copper1000 0.05
            roll 1 12 |> silver1000 0.25
            roll 1 6 |> electrum1000 0.25
            roll 1 8 |> gold1000 0.25
            roll 1 12 |> gems 0.15
            roll 1 2 |> jewelry 0.10
        | F ->
            roll 1 20 |> silver1000 0.1
            roll 1 12 |> electrum1000 0.15
            roll 1 10 |> gold1000 0.4
            roll 1 8 |> platinum100 0.35
            roll 2 10 |> gems 0.2
            roll 1 10 |> jewelry 0.1
        | G ->
            roll 10 4 |> gold1000 0.5
            roll 1 20 |> platinum100 0.5
            roll 5 4 |> gems 0.3
            roll 1 10 |> jewelry 0.25
        | H ->
            roll 5 4 |> copper1000 0.25
            roll 1 100 |> silver1000 0.4
            roll 10 4 |> electrum1000 0.4
            roll 10 6 |> gold1000 0.55
            roll 5 10 |> platinum100 0.25
            roll 1 100 |> gems 0.5
            roll 1 40 |> jewelry 0.5
        | I ->
            roll 3 6 |> platinum100 0.3
            roll 2 10 |> gems 0.55
            roll 1 12 |> jewelry 0.5
        | J -> roll 3 8 |> copper
        | K -> roll 3 6 |> silver
        | L -> roll 2 6 |> electrum
        | M -> roll 2 4 |> gold
        | N -> roll 1 6 |> platinum
        | O ->
            roll 1 4 |> copper1000 0.25
            roll 1 3 |> silver1000 0.20
        | P ->
            roll 1 6 |> silver1000 0.3
            roll 1 2 |> electrum1000 0.25
        | Q -> roll 1 4 |> gems 0.5
        | R ->
            roll 2 4 |> gold1000 0.4
            roll 10 6 |> platinum100 50
            roll 4 8 |> gems 0.55
            roll 1 12 |> jewelry 0.45
        | S -> roll 2 4 |> jewelry 1.
        | T -> roll 1 4 |> jewelry 1.
        | U ->
            roll 10 8 |> gems 0.9
            roll 5 6 |> jewelry 0.8
        | V -> ()
        | W ->
            roll 5 6 |> gold1000 0.6
            roll 1 8 |> platinum100 0.15
            roll 10 8 |> gems 0.6
            roll 5 8 |> jewelry 0.5
        | X -> ()
        | Y -> roll 2 6 |> gold1000 0.7
        | Z ->
            roll 1 3 |> copper1000 0.3
            roll 1 4 |> silver1000 0.25
            roll 1 4 |> electrum1000 0.25
            roll 1 4 |> gold1000 0.3
            roll 1 6 |> platinum100 0.3
            roll 10 6 |> gems 0.55
            roll 5 6 |> jewelry 0.5
    let sumsByCurrency =
        loot |> List.groupBy snd |> List.map (fun (currency, items) -> currency, items |> List.sumBy fst)
    let totalValueInCopper =
        sumsByCurrency |> List.map (function
        | Copper, v -> v
        | Silver, v -> 10 * v
        | Gold, v -> 100 * v
        | Electrum, v -> 50 * v
        | Platinum, v -> 500 * v
        | Gems, v -> 10000 * v // TODO: use actual gem and jewelry values instead of just 100 gp per gem and 500 gp per jewelry piece
        | Jewelry, v -> 50000 * v)
        |> List.sum

    if totalValueInCopper > 100 then
        let descriptions = sumsByCurrency |> List.sortBy fst |> List.map (function currency, n -> $"{n} {currency}") |> String.oxfordJoin
        (totalValueInCopper / 100 * 1<gp>), descriptions
    else
        0<gp>, "nothing of value"
