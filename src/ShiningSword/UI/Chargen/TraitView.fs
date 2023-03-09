module UI.Chargen.TraitView
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Fable.React
open Feliz
open Fable.Core
open UI
open Domain.Character.DungeonFantasy.TraitsAndAttributes.Data

type Key = string list
type TraitMsg =
    | Queue of Key
    | QueueData of Key * string
    | Unqueue of Key
    | ClearStrictlyUnder of Key // clears all subkeys. E.g. if WeaponMaster (Two Weapon) is partially selected, eliminate TwoWeapon and any weapon choices under it but not WeaponMaster itself

// Essentially a trait has three stats: 1. Already exists on the underlying character,
// 2. being added (in queue), or 3. not present at all. E.g. Swashbuckler who's going
// up a level already has Combat Reflexes, but may be adding Enhanced Time Sense
// for 30 points, and doesn't have Kiai at all. ReactBuilder is for adding stuff to
// the queue, while DataBuilder is for extracting all of the stuff in the queue and
// dispatching it to the underlying character.

type DataBuilder(char: Character, prefix: Key, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
    let extend entry = DataBuilder(char, entry::prefix, queue, dispatch)
    let keyOf (prefix: Key) (pick: 't) =
        (pick.ToString())::prefix
    let has key = queue.ContainsKey key
    let fulfilled key options = options |> List.tryFind (fun pick -> has(keyOf key pick))
    let fulfilledFilter key options = options |> List.filter (fun pick -> has(keyOf key pick))

    interface OutputBuilder<Chosen, Chosen list> with
        member _.aggregate label f = List.concat (extend label |> f)
        member _.binary(value: Chosen, label: string): Chosen list =
            [if has (keyOf prefix value) then value]
        member _.binary(value: Chosen): Chosen list =
            [if has (keyOf prefix value) then value]
        member _.choose2D(ctor, options1, options2) =
            [   let key = keyOf prefix ctor.name.Value
                if has key then
                    match (fulfilled key options1), (fulfilled key options2) with
                    | Some v1, Some v2 -> ctor.create(v1, v2)
                    | _ -> ()
                ]
        // similar to chooseOne but can default to lowest value
        member _.chooseLevels(ctor, levels) =
            [   let key = keyOf prefix ctor.name.Value
                if has key then
                    match (fulfilled key levels) with
                    | Some v -> ctor.create(v)
                    | None -> ctor.create(levels[0])
                ]
        member _.chooseOne(ctor, options) =
            [   let key = keyOf prefix ctor.name.Value
                if has key then
                    match (fulfilled key options) with
                    | Some v -> ctor.create(v)
                    | _ -> ()
                ]
        member _.chooseOneFromHierarchy(ctor, options) =
            [   let key = keyOf prefix ctor.name.Value
                if has key then
                    let fulfilledParam = function
                        | Multi.Const v when has (keyOf key v) -> Some v
                        | Multi.One(ctor1, options1) when has (keyOf key ctor1.name.Value) ->
                            match fulfilled (keyOf key ctor1.name.Value) options1 with
                            | Some v -> Some (ctor1.create v)
                            | None -> None
                        | Multi.DistinctTwo(ctor1, options1) when has (keyOf key ctor1.name.Value) ->
                            match fulfilledFilter (keyOf key ctor1.name.Value) options1 with
                            | v1::v2::_ -> Some (ctor1.create(v1, v2))
                            | _ -> None
                        | _ -> None
                    match (options |> List.tryPick fulfilledParam) with
                    | Some v -> ctor.create(v)
                    | None -> ()
                ]
        // visually but not semantically distinct from aggregate
        member _.chooseUpToBudget budget label optionsFunc = List.concat (extend label |> optionsFunc)
        // visually but not semantically distinct from aggregate
        member _.chooseUpToBudgetWithSuggestions budget label optionsFunc = List.concat (extend label |> optionsFunc |> List.collect snd)
        member _.chooseWithStringInput(ctor, placeholder) =
            [   let key = keyOf prefix ctor.name.Value
                if has key then
                    ctor.create(queue.[key])
                ]
        member _.grant(value) = [value]
        member _.grantOne(ctor, options) =
            [   let key = keyOf prefix ctor.name.Value
                match (fulfilled key options) with
                | Some v -> ctor.create(v)
                | _ when options.Length = 1 -> ctor.create(options[0]) // default to first
                | _ -> ()
                ]
        member _.grantWithStringInput(ctor, label) =
            [   let key = keyOf prefix ctor.name.Value
                let stringArg = queue |> Map.tryFind key |> Option.defaultValue ""
                ctor.create stringArg
                ]

type CostBuilder(char: Character, prefix: Key, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
    let extend entry = CostBuilder(char, entry::prefix, queue, dispatch)
    let dataBuilder(): OutputBuilder<_,_> = DataBuilder(char, prefix, queue, dispatch)
    let keyOf (prefix: Key) (pick: 't) =
        (pick.ToString())::prefix
    let has key = queue.ContainsKey key
    let fulfilled key options = options |> List.tryFind (fun pick -> has(keyOf key pick))
    let fulfilledFilter key options = options |> List.filter (fun pick -> has(keyOf key pick))

    interface OutputBuilder<Chosen, int> with
        member _.aggregate label f = notImpl()
        member _.binary(value: Chosen, label: string) = cost value
        member _.binary(value: Chosen) = cost value
        member _.choose2D(ctor, options1, options2) =
            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
            match dataBuilder().choose2D(ctor, options1, options2) with
            | [v] -> cost v
            | _ -> List.allPairs options1 options2 |> List.minBy' (ctor.create >> cost)

        // similar to chooseOne but can default to lowest value
        member _.chooseLevels(ctor, levels) =
            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
            match dataBuilder().chooseLevels(ctor, levels) with
            | [v] -> cost v
            | _ -> levels |> List.minBy' (ctor.create >> cost)
        member _.chooseOne(ctor, options) =
            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
            match dataBuilder().chooseLevels(ctor, options) with
            | [v] -> cost v
            | _ -> options |> List.minBy' (ctor.create >> cost)
        member _.chooseOneFromHierarchy(ctor, options) =
            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
            match dataBuilder().chooseOneFromHierarchy(ctor, options) with
            | [v] -> cost v
            | _ ->
                let expand = function
                    | Multi.Const v -> [ctor.create v]
                    | Multi.One(ctor1, options) -> options |> List.map (ctor1.create >> ctor.create)
                    // for cost-evaluation purposes we are going to ignore the "distinct" requirement, e.g. we will check the cost on WeaponMaster(TwoWeapon(Rapier, Rapier)) because it's not worth excluding it.
                    | Multi.DistinctTwo(ctor1, options) -> options |> List.allPairs options |> List.map (ctor1.create >> ctor.create)
                options |> List.minBy' (expand >> List.minBy' cost)
        // visually but not semantically distinct from aggregate
        member _.chooseUpToBudget budget label optionsFunc = notImpl()
        // visually but not semantically distinct from aggregate
        member _.chooseUpToBudgetWithSuggestions budget label optionsFunc = notImpl()
        member _.chooseWithStringInput(ctor, placeholder) = (ctor.create "") |> cost
        member _.grant(value) = cost value
        member _.grantOne(ctor, options) = options |> List.minBy' (ctor.create >> cost)
        member _.grantWithStringInput(ctor, label) = (ctor.create "") |> cost

type ReactBuilder(char: Character, prefix: Key, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
    let extend entry = ReactBuilder(char, entry::prefix, queue, dispatch)
    let dataBuilder() : OutputBuilder<_,_> = DataBuilder(char, prefix, queue, dispatch)
    let keyOf (prefix: Key) (pick: 't) =
        (pick.ToString())::prefix
    let has key = queue.ContainsKey key
    let fulfilled key options = options |> List.tryFind (fun pick -> has(keyOf key pick))
    let fulfilledFilter key options = options |> List.filter (fun pick -> has(keyOf key pick))

    let chkId (key:Key) (v:'t) =
        ("chk" + (key |> String.concat "-") + (v.ToString())).Replace(" ", "")

    let checkboxBase(isChecked, label: string, cost: int option, key, uniqueId, toggleFunc, childrenFunc) =
        let chkId = defaultArg uniqueId (chkId key label)
        let toggle select =
            if select then dispatch (Queue key)
            else dispatch (Unqueue key)
        class' "potentialChoice" Html.div [
            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked isChecked; prop.readOnly true; prop.onChange (match toggleFunc with Some f -> f key | None -> toggle)]
            let txt = match cost with Some cost -> $"{label} [{cost}]" | _ -> label
            Html.label [prop.text txt; prop.htmlFor chkId]
            if isChecked then
                yield! childrenFunc()
            ]
    let checkbox(isChecked, label, cost, key, uniqueId, childrenFunc) =
        checkboxBase(isChecked, label, cost, key, uniqueId, None, childrenFunc)
    let checkboxWithExplicitToggle(isChecked, label, cost, key, uniqueId, toggle, childrenFunc) =
        checkboxBase(isChecked, label, cost, key, uniqueId, Some toggle, childrenFunc)

    let binary (isGrant, cost, prefix: Key, (label:string option), value) =
        match value with
        | Data.Trait trait1 as value ->
            let label = label |> Option.defaultWith (fun _ -> trait1 |> traitName)
            let key = keyOf prefix value
            let isQueued = isGrant || queue |> Map.containsKey key
            // if it's a grant, then we don't care what its chkId is because clicking it will have no effect.
            // reserve the "real" id for something later.
            let chkId = chkId key trait1
            checkbox(isQueued, label, cost, key, Some chkId, fun () -> [])
        | _ -> notImpl() // probably not needed--why would we ever have a binary skill/attribute mod instead of a level?
    let subchoice (prefix, ctor: Constructor<_,_>, costFunc, pick: 't) =
        let txt = match costFunc with
                    | Some f -> $"{pick.ToString() |> String.uncamel} [{f pick}]"
                    | None -> pick.ToString() |> String.uncamel
        let chkId = chkId prefix pick
        let key = keyOf prefix pick
        let toggle select =
            if select then
                dispatch (Queue key)
            else
                dispatch (Unqueue key)
        // NOT always a div here--picked option shows as span in order to conserve vertical space
        class' "subchoice" (if queue.ContainsKey key then Html.span else Html.div) [
            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked (queue.ContainsKey key); prop.readOnly true; prop.onChange toggle]
            Html.label [prop.text txt; prop.htmlFor chkId]
            ]

    let refineN (n: int, ctor: Constructor<_, _>, costFunc, prefix, options: 't list) =
        [   if options.Length > n then
                match options |> List.choose(fun pick -> if queue.ContainsKey (keyOf prefix pick) then Some pick else None) with
                | picks when picks.Length >= n ->
                    for pick in picks do
                        subchoice(prefix, ctor, costFunc, pick)
                | _ ->
                    for pick in options do
                        subchoice(prefix, ctor, costFunc, pick)
            else checkbox(true, ctor.name.Value, (costFunc |> Option.map (fun f -> f options[0])), prefix, None, fun () -> [])
            ]

    let costOf = cost >> Some

    interface OutputBuilder<Chosen, ReactElement> with
        member _.grant(value) = binary(true, costOf value, prefix, None, value)
        member _.binary(value) = binary(false, costOf value, prefix, None, value)
        // labeled binary
        member _.binary(value, label) = binary(false, costOf value, prefix, Some label, value)
        member _.chooseWithStringInput(ctor, placeholder) =
            let id = ctor.name.Value
            let key = id::prefix
            class' "potentialChoice" Html.div [
                Html.input [prop.id id; prop.type'.checkbox; prop.isChecked (has key); prop.onChange(fun (check:bool) -> if check then Queue key |> dispatch else Unqueue key |> dispatch)]
                Html.label [prop.htmlFor id; prop.text $"""{id} [{ctor.create "" |> cost}]"""]
                if has key then
                    Html.input [prop.type'.text; prop.valueOrDefault queue[key]; prop.placeholder placeholder; prop.onChange(fun (text:string) -> QueueData(key, text) |> dispatch)]
                ]

        member _.chooseLevels(ctor, options) =
            // chooseLevels and chooseOne are different in the sense that chooseOne has no implied total ordering,
            //   so will have a different UI without + and - buttons.
            if ctor.name.IsNone then shouldntHappen $"ctor had no name! This isn't supposed to happen with the ctors that actually get used, only with low-level ctors like Trait that get composed into other ctors."
            let key = (ctor.name.Value)::prefix
            match options |> List.tryFindIndex(fun v -> has (keyOf key v)) with
            | Some ix when ix > 0 ->
                checkbox(has key, $"{ctor.name.Value} {options[ix]}", costOf (ctor.create options[ix]), key, None, fun () -> [Html.button [prop.text "-"]; Html.button [prop.text "+"]])
            | _ ->
                checkbox(has key, ctor.name.Value, costOf (ctor.create options[0]), key, None, fun () -> [Html.button [prop.text "-"]; Html.button [prop.text "+"]])

        member _.chooseOne(ctor, options) =
            let key = (ctor.name.Value)::prefix
            let cost' =
                match dataBuilder().chooseOne(ctor, options) with
                | [v] -> Some (cost v)
                | _ -> None
            checkbox(has key, ctor.name.Value, cost', key, None, fun () -> refineN(1, ctor, Some (ctor.create >> cost), key, options))

        member _.choose2D(ctor, options1, options2) =
            let key = (ctor.name.Value)::prefix
            let cost =
                match dataBuilder().choose2D(ctor, options1, options2) with
                | [v] -> cost v
                | _ -> List.allPairs options1 options2 |> List.minBy' (ctor.create >> cost)
                |> Some
            checkbox(has key, ctor.name.Value, cost, key, None, fun () -> (refineN(1, ctor, None, key, options1))@(refineN(1, ctor, None, key, options2)))

        member _.chooseOneFromHierarchy(ctor, lst) =
            let key = keyOf prefix (ctor.name.Value)
            let ancestorKey = key
            let toggle parentKeys key select =
                match parentKeys with
                | [] -> ()
                | ultimateAncestor::rest ->
                    ClearStrictlyUnder ultimateAncestor |> dispatch
                    for parentKey in rest do
                        Queue parentKey |> dispatch
                if select then Queue key |> dispatch
                else Unqueue key |> dispatch
            let makeReactElement = function
                | Multi.Const v ->
                    let key = (keyOf key v)
                    [checkboxWithExplicitToggle(queue.ContainsKey key, (traitName (unbox v)), None, key, None, toggle [ancestorKey; key], thunk [])]
                | Multi.One(ctor1, options1) ->
                    let key = keyOf key ctor1.name.Value
                    [checkboxWithExplicitToggle(has key, ctor1.name.Value, None, key, None, toggle [ancestorKey; key], thunk <| refineN(1, ctor1, None, key, options1))]
                | Multi.DistinctTwo(ctor1, options1) ->
                    let key = keyOf key ctor1.name.Value
                    [checkboxWithExplicitToggle(has key, ctor1.name.Value, None, key, None, toggle [ancestorKey; key], thunk <| refineN(2, ctor1, None, key, options1))]
            match dataBuilder().chooseOneFromHierarchy(ctor, lst) with
            | [] ->
                checkbox(has key, ctor.name.Value, None, key, None, thunk (lst |> List.collect makeReactElement))
            | _ ->
                let pick = lst |> List.find(function
                    | Multi.Const v -> has (keyOf key v)
                    | Multi.One(ctor, values) -> has (keyOf key ctor.name.Value) && (values |> List.filter (keyOf (keyOf key ctor.name.Value) >> has)).Length = 1
                    | Multi.DistinctTwo(ctor, values) -> has (keyOf key ctor.name.Value) && (values |> List.filter (keyOf (keyOf key ctor.name.Value) >> has)).Length = 2
                    )
                let cost = (dataBuilder().chooseOneFromHierarchy(ctor, lst))[0] |> costOf
                checkbox(has key, ctor.name.Value, cost, key, None, thunk1 makeReactElement pick)

        member _.grantOne(ctor, options) =
            let key = (ctor.name.Value)::prefix
            // code smell: maybe tuple2bind1 is a code smell. Maybe Enhanced Parry and Luck are DIFFERENT.
            if options.Length = 1 then
                React.fragment (refineN(1, ctor, Some(ctor.create >> cost), key, options))
            else
                let cost = (dataBuilder().grantOne(ctor, options)) |> List.tryHead |> Option.map cost
                checkbox(true, ctor.name.Value, cost, key, None, fun () -> refineN(1, ctor, None, key, options))

        member _.grantWithStringInput(ctor, placeholder) =
            let key = (ctor.name.Value)::prefix
            checkbox(true, ctor.name.Value, (costOf (ctor.create "")), key, None,
                            fun () -> [
                                Html.input [
                                    prop.type'.text
                                    prop.placeholder placeholder
                                    ]
                                ])

        member _.aggregate label elementsFunc =
            let elements = elementsFunc (extend label)
            (class' "aggregate" Html.div elements)
        member _.chooseUpToBudget budget label elementsFunc =
            let elements = elementsFunc (extend label)
            Html.fieldSet [
                Html.legend $"{label} ({budget} points)"
                (class' "aggregate" Html.div elements)
                ]
        member _.chooseUpToBudgetWithSuggestions budget label elementsFunc =
            let budgetsAndElements = elementsFunc (extend label)
            React.fragment [
                        for budget, section in budgetsAndElements do
                            if budget.IsSome then
                                Html.fieldSet [
                                        Html.legend $"{label} (at least {budget} points)"
                                        (class' "aggregate" Html.div section)
                                        ]
                            ]
    member private this.up = this :> OutputBuilder<_,_>

[<ReactComponent>]
let TraitView (profession: Templates.Package<Profession>, char: Character, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
    let builder = (ReactBuilder(char, [], queue, dispatch))
    class' "traitview" Html.fieldSet [
        classTxt' "subtitle" Html.legend profession.displayName
        Templates.menusFor builder profession.name
        ]
