module UI.Chargen.TraitView
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Fable.React
open Feliz
open Fable.Core
open UI

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
    interface OutputBuilder<Addresses.Chosen, Addresses.Chosen list> with
        member _.aggregate label f = List.concat (extend label |> f)
        member _.binary(value: Addresses.Chosen, label: string): Addresses.Chosen list =
            [if has (keyOf prefix value) then value]
        member _.binary(value: Addresses.Chosen): Addresses.Chosen list =
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
                | _ -> ()
                ]
        member _.grantWithStringInput(ctor, label) =
            [   let key = keyOf prefix ctor.name.Value
                let stringArg = queue |> Map.tryFind key |> Option.defaultValue ""
                ctor.create stringArg
                ]

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

    let checkboxBase(isChecked, label: string, key, uniqueId, toggleFunc, childrenFunc) =
        let chkId = defaultArg uniqueId (chkId key label)
        let toggle select =
            if select then dispatch (Queue key)
            else dispatch (Unqueue key)
        class' "potentialChoice" Html.div [
            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked isChecked; prop.readOnly true; prop.onChange (match toggleFunc with Some f -> f key | None -> toggle)]
            Html.label [prop.text label; prop.htmlFor chkId]
            if isChecked then
                yield! childrenFunc()
            ]
    let checkbox(isChecked, label, key, uniqueId, childrenFunc) =
        checkboxBase(isChecked, label, key, uniqueId, None, childrenFunc)
    let checkboxWithExplicitToggle(isChecked, label, key, uniqueId, toggle, childrenFunc) =
        checkboxBase(isChecked, label, key, uniqueId, Some toggle, childrenFunc)

    let binary (isGrant, prefix: Key, (label:string option), value) =
        match value with
        | Addresses.Trait trait1 as value ->
            let label = label |> Option.defaultWith (fun _ -> trait1 |> traitName)
            let key = keyOf prefix value
            let isQueued = isGrant || queue |> Map.containsKey key
            // if it's a grant, then we don't care what its chkId is because clicking it will have no effect.
            // reserve the "real" id for something later.
            let chkId = chkId key trait1
            checkbox(isQueued, label, key, Some chkId, fun () -> [])
        | _ -> notImpl() // probably not needed--why would we ever have a binary skill/attribute mod instead of a level?
    let subchoice (prefix, ctor: Constructor<_,_>, pick: 't) =
        let txt = pick.ToString() |> String.uncamel
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

    let refineN (n: int, ctor: Constructor<_, _>, prefix, options: 't list) =
        [   if options.Length > n then
                match options |> List.choose(fun pick -> if queue.ContainsKey (keyOf prefix pick) then Some pick else None) with
                | picks when picks.Length >= n ->
                    for pick in picks do
                        subchoice(prefix, ctor, pick)
                | _ ->
                    for pick in options do
                        subchoice(prefix, ctor, pick)
            else checkbox(true, ctor.name.Value, prefix, None, fun () -> [])
            ]

    interface OutputBuilder<Addresses.Chosen, ReactElement> with
        member _.grant(value) = binary(true, prefix, None, value)
        member _.binary(value) = binary(false, prefix, None, value)
        // labeled binary
        member _.binary(value, label) = binary(false, prefix, Some label, value)
        member _.chooseWithStringInput(ctor, placeholder) =
            let id = ctor.name.Value
            let key = id::prefix
            Html.div [
                Html.input [prop.id id; prop.type'.checkbox; prop.isChecked (has key); prop.onChange(fun (check:bool) -> if check then Queue key |> dispatch else Unqueue key |> dispatch)]
                Html.label [prop.htmlFor id; prop.text id]
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
                checkbox(has key, $"{ctor.name.Value} {options[ix]}", key, None, fun () -> [Html.button [prop.text "-"]; Html.button [prop.text "+"]])
            | _ ->
                checkbox(has key, ctor.name.Value, key, None, fun () -> [Html.button [prop.text "-"]; Html.button [prop.text "+"]])

        member _.chooseOne(ctor, options) =
            let key = (ctor.name.Value)::prefix
            checkbox(has key, ctor.name.Value, key, None, fun () -> refineN(1, ctor, key, options))

        member _.choose2D(ctor, options1, options2) =
            let key = (ctor.name.Value)::prefix
            checkbox(has key, ctor.name.Value, key, None, fun () -> (refineN(1, ctor, key, options1))@(refineN(1, ctor, key, options2)))

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
                    [checkboxWithExplicitToggle(queue.ContainsKey key, (traitName (unbox v)), key, None, toggle [ancestorKey; key], thunk [])]
                | Multi.One(ctor1, options1) ->
                    let key = keyOf key ctor1.name.Value
                    [checkboxWithExplicitToggle(queue.ContainsKey key, ctor1.name.Value, key, None, toggle [ancestorKey; key], thunk <| refineN(1, ctor1, key, options1))]
                | Multi.DistinctTwo(ctor1, options1) ->
                    let key = keyOf key ctor1.name.Value
                    [checkboxWithExplicitToggle(queue.ContainsKey key, ctor1.name.Value, key, None, toggle [ancestorKey; key], thunk <| refineN(2, ctor1, key, options1))]
            match dataBuilder().chooseOneFromHierarchy(ctor, lst) with
            | [] ->
                checkbox(has key, ctor.name.Value, key, None, thunk (lst |> List.collect makeReactElement))
            | _ ->
                let pick = lst |> List.find(function
                    | Multi.Const v -> has (keyOf key v)
                    | Multi.One(ctor, values) -> has (keyOf key ctor.name.Value) && (values |> List.filter (keyOf (keyOf key ctor.name.Value) >> has)).Length = 1
                    | Multi.DistinctTwo(ctor, values) -> has (keyOf key ctor.name.Value) && (values |> List.filter (keyOf (keyOf key ctor.name.Value) >> has)).Length = 2
                    )
                checkbox(has key, ctor.name.Value, key, None, thunk1 makeReactElement pick)

        member _.grantOne(ctor, options) =
            let key = (ctor.name.Value)::prefix
            checkbox(true, ctor.name.Value, key, None, fun () -> refineN(1, ctor, key, options))

        member _.grantWithStringInput(ctor, placeholder) =
            let key = (ctor.name.Value)::prefix
            checkbox(true, ctor.name.Value, key, None,
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
