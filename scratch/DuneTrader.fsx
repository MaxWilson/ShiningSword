#load "SketchUI.fsx"
open SketchUI
let r = System.Random()
type City = Urik | Balik | Tyr
type Goods = Spice | Lumber | Metal
type Model = {
    city: City
    money: int
    goods: Map<Goods, int>
    }
type Msg =
    | Goto of City
    | Buy of Goods * int
    | Sell of Goods * int
let price = function
    | Lumber, (Tyr | Balik) -> 10
    | Lumber, Urik -> 17
    | Spice, (Tyr | Urik) -> 15
    | Spice, Balik -> 10
    | Metal, Tyr -> 30
    | Metal, (Balik | Urik) -> 45

let bulk = function
    | Lumber -> 10
    | Spice -> 5
    | Metal -> 1

exception UserError of string
let cant msg = UserError $"Can't because {msg}" |> raise

let init _ = { city = Tyr; money = 100; goods = [(Spice, 10); (Lumber, 5)] |> Map.ofList }
let update msg model =
    match msg with
    | Goto city ->
        if city = model.city then model
        else
            let cost = model.goods |> Seq.sumBy (fun (KeyValue(good, amt)) -> amt * bulk good)
            { model with city = city; money = model.money - cost }
    | Buy (good, amount) ->
        let cost = price (good, model.city) * amount
        if cost > model.money then cant $"That costs {cost} and you only have {model.money}"
        { model with money = model.money - cost; goods = model.goods |> Map.change good (function None -> Some amount | Some x -> Some (x + amount)) }
    | Sell (good, amount) ->
        match model.goods.TryFind good with
        | Some qty ->
            if qty < amount then cant $"You only have {qty} of {good}"
            { model with money = model.money + price (good, model.city) * amount; goods = model.goods |> Map.add good (qty - amount) }
        | None ->
            cant $"You don't have any {good}"

let view model =
    let goods = model.goods |> Map.toList |> List.sortBy fst |> List.map (fun (good, amt) -> $"{good}: {amt}") |> String.oxfordJoin
    $"""
    City: {model.city}
    Money: {model.money}
    Stuff: {goods}
    """

let send', state = connect init update view
let send msg = try send' msg with UserError msg -> printfn "%s" msg
let buy good amount = send (Buy (good, amount))
let sell good amount = send (Sell (good, amount))
let travel city = send (Goto city)

printfn "%s" (view state.Value)

sell Lumber 5
buy Metal 10
buy Metal 5
sell Spice 10
buy Metal 5
travel Balik
sell Metal 10
buy Spice 20
buy Lumber 20
travel Tyr
sell Spice 20
sell Lumber 20
buy Metal 8
travel Balik
sell Metal 8
buy Spice 20
travel Tyr
sell Spice 20
travel Balik
buy Spice 35
travel Tyr
sell Spice 35
buy Metal 10
travel Balik
sell Metal 10
// Metal is the only profitable material! Everything else costs too much to move between cities!
travel Tyr
buy Metal 10
travel Balik
sell Metal 10
travel Tyr
buy Metal 10
travel Balik
sell Metal 10
travel Tyr
buy Metal 20
travel Balik
sell Metal 20
// there should also be fixed costs of travelling, so that travelling empty isn't free