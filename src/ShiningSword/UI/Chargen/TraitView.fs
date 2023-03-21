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
type DataCtx = { prefix: Key; queue: Map<Key, string> }
    with static member fresh = { prefix = []; queue = Map.empty }
let dataBuilder =
    let keyOf (prefix: Key) (meta: Metadata) =
        match meta.keySegment with
        | Some key ->
            key::prefix
        | None -> prefix
    let extend (ctx: DataCtx) (meta:Metadata) =
        { ctx with prefix = keyOf ctx.prefix meta }
    let hasKey ctx key =
        ctx.queue.ContainsKey key
    let has ctx meta =
        hasKey ctx (keyOf ctx.prefix meta)
    // modifies ctx so that the given metadata will consider to have been chosen, no matter what the user has selected
    let ctxAugment meta ctx = { ctx with queue = ctx.queue |> Map.add (keyOf ctx.prefix meta) "" }
    // for a limited subset of things that make sense to grant, such as binary and choose, unpacks their metadata and augments
    let ctxGrantOne ctx = function
            | Binary(meta, _)
            | ChooseOne(meta, _)
            | ChooseLevels(meta, _)
            | Choose2D(meta, _)
            | ChooseWithStringInput(meta, _, _) ->
                ctx |> ctxAugment meta
            | otherwise -> notImpl $"Unexpected grant type: '{otherwise}'"
    let ctxGrantMany ctx many =
        match many with
                | Aggregate(meta, _) -> ctx |> ctxAugment meta
                | Items(meta, items) ->
                    let ctx = ctx |> ctxAugment meta
                    items |> List.fold ctxGrantOne ctx
                | otherwise -> notImpl $"Unexpected grant type: '{otherwise}'" // I don't think we create any other grant types yet

    //let fulfilled key options = options |> List.tryFind (fun pick -> has(keyOf key pick))
    //let fulfilledFilter key options = options |> List.filter (fun pick -> has(keyOf key pick))
    let rec ofMany (ctx: DataCtx) = function
        | Aggregate(meta: Metadata, manyList: 't Many list) ->
            manyList |> List.collect (ofMany (extend ctx meta))
        | ChoosePackage(packages: (Metadata * 't Many) list) ->
            // unlike items, we can only pick one package. E.g. a Swashbuckler
            // can't pick the 20-point package for Sword! and also pick
            // the 20-point package for Sword and Dagger.
            let evalAggregate (meta: Metadata, many: 't Many) =
                if has ctx meta then
                    Some (many |> ofMany (extend ctx meta))
                else None
            packages |> List.tryPick evalAggregate |> Option.defaultValue []
        | GrantItems(many: 't Many) ->
            ofMany (ctxGrantMany ctx many) many
        | Items(meta: Metadata, items: 't OneResult list) ->
            items |> List.collect (ofOne (extend ctx meta))
        | Budget(budget: int, meta: Metadata, items: 't OneResult list) ->
            // dataBuilder doesn't enforce budgets (that's done by reactBuilder, if at all, based on settings),
            // so this is basically just like items
            items |> List.collect (ofOne (extend ctx meta))
        | NestedBudgets(totalBudget: int, meta: Metadata, suggestions: (int option * Metadata * 't OneResult list) list) ->
            // dataBuilder doesn't enforce budgets (that's done by reactBuilder, if at all, based on settings),
            // so this is basically just like items
            suggestions |> List.collect (fun (_,_,items) -> items |> List.collect (ofOne (extend ctx meta)))
    and ofOne (ctx: DataCtx) = function
        | Binary(meta: Metadata, v: 't) -> [if has ctx meta then v]
        | ChooseOne(meta: Metadata, choose: 't Choose) -> [
            if has ctx meta then
                let ctx = extend ctx meta
                let pack, unpack = viaAny<'t list>()
                let f = {   new Polymorphic<Constructor<Any, 't> * (Any * string) list, Any>
                                with
                                member _.Apply ((ctor, args)) =
                                    match args |> List.map fst |> List.tryFind (fun arg -> hasKey ctx (arg.ToString()::ctx.prefix)) with
                                    | Some chosenArg -> [chosenArg |> ctor.create] |> pack
                                    // if there's only one alternative, that's the same as no alternatives. Pick it.
                                    | None when args.Length = 1 -> [args[0] |> fst |> ctor.create] |> pack
                                    | None -> [] |> pack }
                yield! choose.generate f |> unpack
            ]
        | ChooseLevels(meta: Metadata, choose: 't Choose) ->
            // choose and chooseLevels are similar in logic but chooseLevels will show -/+ buttons on UI
            ChooseOne(meta, choose) |> ofOne ctx
        | Choose2D(meta: Metadata, choose2d: 't Choose2D) -> [
            if has ctx meta then
                let ctx = extend ctx meta
                let pack, unpack = viaAny<'t list>()
                let f = {   new Polymorphic<Constructor<Any * Any, 't> * (Any * string) list * (Any * string) list, Any>
                                with
                                member _.Apply ((ctor, args1, args2)) =
                                    let eval args =
                                        args |> List.map fst |> List.tryFind (fun arg -> hasKey ctx (arg.ToString()::ctx.prefix))
                                        // if there's only one alternative, that's the same as no alternatives. Pick it.
                                        |> Option.orElseWith (fun () -> if args.Length = 1 then Some (fst args[0]) else None)
                                    match eval args1, eval args2 with
                                    | Some chosenArg1, Some chosenArg2 -> [ctor.create(chosenArg1, chosenArg2)] |> pack
                                    | _ -> [] |> pack}
                yield! choose2d.generate f |> unpack
            ]
        | ChooseWithStringInput(meta: Metadata, ctor: Constructor<string, 't>, placeholder: string) -> [
            System.Console.Error.WriteLine $"ChooseOne: prefix={ctx.prefix}, meta.keySegment={meta.keySegment}: {has ctx meta}"
            System.Console.Error.WriteLine $"""Is it there? {ctx.queue |> Map.tryFind ["Weapon Bond"; "Swashbuckler"] |> Option.defaultValue "Nope"}"""
            if has ctx meta then
                let key = keyOf ctx.prefix meta
                System.Console.Error.WriteLine $"{key} => {ctx.queue[key]} ==> {ctor.create(ctx.queue[key])}"
                ctor.create(ctx.queue[key])
            ]
        | Grant(meta: Metadata, v: 't OneResult) ->
            v |> ofOne (ctxGrantOne (ctx |> ctxAugment meta) v)
        | ChooseOneFromHierarchy(meta: Metadata, hierarchy: 't OneHierarchy) ->
            hierarchy |> ofHierarchy (extend ctx meta)
    and ofHierarchy (ctx: DataCtx) = function
        | Leaf(meta: Metadata, v: 't) -> [
            if has ctx meta then
                v
            ]
        | Interior(meta: Metadata, hierarchy: 't OneHierarchy list) -> [
            if has ctx meta then
                yield! (hierarchy |> List.collect (ofHierarchy (extend ctx meta)))
            ]
    ofMany

//type foo() =
//        member _.aggregate label f = List.concat (extend label |> f)
//        member _.binary(value: Chosen, label: string): Chosen list =
//            [if has (keyOf prefix value) then value]
//        member _.binary(value: Chosen): Chosen list =
//            [if has (keyOf prefix value) then value]
//        member _.choose2D(ctor, options1, options2) =
//            [   let key = keyOf prefix ctor.name.Value
//                if has key then
//                    match (fulfilled key options1), (fulfilled key options2) with
//                    | Some v1, Some v2 -> ctor.create(v1, v2)
//                    | _ -> ()
//                ]
//        // similar to chooseOne but can default to lowest value
//        member _.chooseLevels(ctor, levels) =
//            [   let key = keyOf prefix ctor.name.Value
//                if has key then
//                    match (fulfilled key levels) with
//                    | Some v -> ctor.create(v)
//                    | None -> ctor.create(levels[0])
//                ]
//        member this.chooseLevels(ctor, levels, format) = this.up.chooseLevels(ctor, levels)
//        member _.chooseOne(ctor, options) =
//            [   let key = keyOf prefix ctor.name.Value
//                if has key then
//                    match (fulfilled key options) with
//                    | Some v -> ctor.create(v)
//                    | _ -> ()
//                ]
//        member _.chooseOneFromHierarchy(ctor, options) =
//            [   let key = keyOf prefix ctor.name.Value
//                if has key then
//                    let fulfilledParam = function
//                        | Multi.Const v when has (keyOf key v) -> Some v
//                        | Multi.One(ctor1, options1) when has (keyOf key ctor1.name.Value) ->
//                            match fulfilled (keyOf key ctor1.name.Value) options1 with
//                            | Some v -> Some (ctor1.create v)
//                            | None -> None
//                        | Multi.DistinctTwo(ctor1, options1) when has (keyOf key ctor1.name.Value) ->
//                            match fulfilledFilter (keyOf key ctor1.name.Value) options1 with
//                            | v1::v2::_ -> Some (ctor1.create(v1, v2))
//                            | _ -> None
//                        | _ -> None
//                    match (options |> List.tryPick fulfilledParam) with
//                    | Some v -> ctor.create(v)
//                    | None -> ()
//                ]
//        // visually but not semantically distinct from aggregate
//        member _.chooseUpToBudget budget label optionsFunc =
//            let builder = extend label
//            let results = optionsFunc builder.up
//            List.concat ((extend label |> optionsFunc |> unbox))
//        // visually but not semantically distinct from aggregate
//        member _.chooseUpToBudgetWithSuggestions budget label optionsFunc = List.concat (extend label |> unbox optionsFunc |> List.collect snd)
//        member _.chooseWithStringInput(ctor, placeholder) =
//            [   let key = keyOf prefix ctor.name.Value
//                if has key then
//                    ctor.create(queue.[key])
//                ]
//        member _.grant(value) = [value]
//        member _.grantOne(ctor, options) =
//            [   let key = keyOf prefix ctor.name.Value
//                match (fulfilled key options) with
//                | Some v -> ctor.create(v)
//                | _ when options.Length = 1 -> ctor.create(options[0]) // default to first
//                | _ -> ()
//                ]
//        member _.grantWithStringInput(ctor, label) =
//            [   let key = keyOf prefix ctor.name.Value
//                let stringArg = queue |> Map.tryFind key |> Option.defaultValue ""
//                ctor.create stringArg
//                ]
//    member private this.up = this :> OutputBuilder<_,_>

//type CostBuilder(char: Character, prefix: Key, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
//    let extend entry = CostBuilder(char, entry::prefix, queue, dispatch)
//    let dataBuilder(): OutputBuilder<_,_> = DataBuilder(char, prefix, queue, dispatch)
//    let keyOf (prefix: Key) (pick: 't) =
//        (pick.ToString())::prefix
//    let has key = queue.ContainsKey key
//    let fulfilled key options = options |> List.tryFind (fun pick -> has(keyOf key pick))
//    let fulfilledFilter key options = options |> List.filter (fun pick -> has(keyOf key pick))

//    interface OutputBuilder<Chosen, int> with
//        member _.aggregate label f = notImpl()
//        member _.binary(value: Chosen, label: string) = cost value
//        member _.binary(value: Chosen) = cost value
//        member _.choose2D(ctor, options1, options2) =
//            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
//            match dataBuilder().choose2D(ctor, options1, options2) with
//            | [v] -> cost v
//            | _ -> List.allPairs options1 options2 |> List.minBy' (ctor.create >> cost)

//        // similar to chooseOne but can default to lowest value
//        member _.chooseLevels(ctor, levels) =
//            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
//            match dataBuilder().chooseLevels(ctor, levels) with
//            | [v] -> cost v
//            | _ -> levels |> List.minBy' (ctor.create >> cost)
//        member this.chooseLevels(ctor, levels, format) = this.up.chooseLevels(ctor, levels)
//        member _.chooseOne(ctor, options) =
//            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
//            match dataBuilder().chooseLevels(ctor, options) with
//            | [v] -> cost v
//            | _ -> options |> List.minBy' (ctor.create >> cost)
//        member _.chooseOneFromHierarchy(ctor, options) =
//            // if a specific value is already chosen, give its cost, otherwise give the minimum possible cost for the whole family of options
//            match dataBuilder().chooseOneFromHierarchy(ctor, options) with
//            | [v] -> cost v
//            | _ ->
//                let expand = function
//                    | Multi.Const v -> [ctor.create v]
//                    | Multi.One(ctor1, options) -> options |> List.map (ctor1.create >> ctor.create)
//                    // for cost-evaluation purposes we are going to ignore the "distinct" requirement, e.g. we will check the cost on WeaponMaster(TwoWeapon(Rapier, Rapier)) because it's not worth excluding it.
//                    | Multi.DistinctTwo(ctor1, options) -> options |> List.allPairs options |> List.map (ctor1.create >> ctor.create)
//                options |> List.minBy' (expand >> List.minBy' cost)
//        // visually but not semantically distinct from aggregate
//        member _.chooseUpToBudget budget label optionsFunc = notImpl()
//        // visually but not semantically distinct from aggregate
//        member _.chooseUpToBudgetWithSuggestions budget label optionsFunc = notImpl()
//        member _.chooseWithStringInput(ctor, placeholder) = (ctor.create "") |> cost
//        member _.grant(value) = cost value
//        member _.grantOne(ctor, options) = options |> List.minBy' (ctor.create >> cost)
//        member _.grantWithStringInput(ctor, label) = (ctor.create "") |> cost
//    member private this.up = this :> OutputBuilder<_,_>

//type ReactBuilder(char: Character, prefix: Key, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
//    let extend entry = ReactBuilder(char, entry::prefix, queue, dispatch)
//    let dataBuilder() : OutputBuilder<_,_> = DataBuilder(char, prefix, queue, dispatch)
//    let keyOf (prefix: Key) (pick: 't) =
//        (pick.ToString())::prefix
//    let has key = queue.ContainsKey key
//    let fulfilled key options = options |> List.tryFind (fun pick -> has(keyOf key pick))
//    let fulfilledFilter key options = options |> List.filter (fun pick -> has(keyOf key pick))

//    let chkId (key:Key) (v:'t) =
//        ("chk" + (key |> String.concat "-") + (v.ToString())).Replace(" ", "")

//    let checkboxBase(isChecked, label: string, cost: int option, key, uniqueId, toggleFunc, childrenFunc) =
//        let chkId = defaultArg uniqueId (chkId key label)
//        let toggle select =
//            if select then dispatch (Queue key)
//            else dispatch (Unqueue key)
//        class' "potentialChoice" Html.div [
//            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked isChecked; prop.readOnly true; prop.onChange (match toggleFunc with Some f -> f key | None -> toggle)]
//            let txt = match cost with Some cost -> $"{label} [{cost}]" | _ -> label
//            Html.label [prop.text txt; prop.htmlFor chkId]
//            if isChecked then
//                yield! childrenFunc()
//            ]
//    let checkbox(isChecked, label, cost, key, uniqueId, childrenFunc) =
//        checkboxBase(isChecked, label, cost, key, uniqueId, None, childrenFunc)
//    let checkboxWithExplicitToggle(isChecked, label, cost, key, uniqueId, toggle, childrenFunc) =
//        checkboxBase(isChecked, label, cost, key, uniqueId, Some toggle, childrenFunc)

//    let binary (isGrant, cost, prefix: Key, (label:string option), value) =
//        match value with
//        | Data.Trait trait1 as value ->
//            let label = label |> Option.defaultWith (fun _ -> trait1 |> traitName)
//            let key = keyOf prefix value
//            let isQueued = isGrant || queue |> Map.containsKey key
//            // if it's a grant, then we don't care what its chkId is because clicking it will have no effect.
//            // reserve the "real" id for something later.
//            let chkId = chkId key trait1
//            checkbox(isQueued, label, cost, key, Some chkId, fun () -> [])
//        | _ -> notImpl() // probably not needed--why would we ever have a binary skill/attribute mod instead of a level?
//    let subchoice (prefix, ctor: Constructor<_,_>, costFunc, pick: 't) =
//        let txt = match costFunc with
//                    | Some f -> $"{pick.ToString() |> String.uncamel} [{f pick}]"
//                    | None -> pick.ToString() |> String.uncamel
//        let chkId = chkId prefix pick
//        let key = keyOf prefix pick
//        let toggle select =
//            if select then
//                dispatch (Queue key)
//            else
//                dispatch (Unqueue key)
//        // NOT always a div here--picked option shows as span in order to conserve vertical space
//        class' "subchoice" (if queue.ContainsKey key then Html.span else Html.div) [
//            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked (queue.ContainsKey key); prop.readOnly true; prop.onChange toggle]
//            Html.label [prop.text txt; prop.htmlFor chkId]
//            ]

//    let refineN (n: int, ctor: Constructor<_, _>, costFunc, prefix, options: 't list) =
//        [   if options.Length > n then
//                match options |> List.choose(fun pick -> if queue.ContainsKey (keyOf prefix pick) then Some pick else None) with
//                | picks when picks.Length >= n ->
//                    for pick in picks do
//                        subchoice(prefix, ctor, costFunc, pick)
//                | _ ->
//                    for pick in options do
//                        subchoice(prefix, ctor, costFunc, pick)
//            else checkbox(true, ctor.name.Value, (costFunc |> Option.map (fun f -> f options[0])), prefix, None, fun () -> [])
//            ]

//    let costOf = cost >> Some

//    interface OutputBuilder<Chosen, ReactElement> with
//        member _.grant(value) = binary(true, costOf value, prefix, None, value)
//        member _.binary(value) = binary(false, costOf value, prefix, None, value)
//        // labeled binary
//        member _.binary(value, label) = binary(false, costOf value, prefix, Some label, value)
//        member _.chooseWithStringInput(ctor, placeholder) =
//            let id = ctor.name.Value
//            let key = id::prefix
//            class' "potentialChoice" Html.div [
//                Html.input [prop.id id; prop.type'.checkbox; prop.isChecked (has key); prop.onChange(fun (check:bool) -> if check then Queue key |> dispatch else Unqueue key |> dispatch)]
//                Html.label [prop.htmlFor id; prop.text $"""{id} [{ctor.create "" |> cost}]"""]
//                if has key then
//                    Html.input [prop.type'.text; prop.valueOrDefault queue[key]; prop.placeholder placeholder; prop.onChange(fun (text:string) -> QueueData(key, text) |> dispatch)]
//                ]

//        member _.chooseLevels(ctor, options, format) =
//            // chooseLevels and chooseOne are different in the sense that chooseOne has no implied total ordering,
//            //   so will have a different UI without + and - buttons.
//            if ctor.name.IsNone then shouldntHappen $"ctor had no name! This isn't supposed to happen with the ctors that actually get used, only with low-level ctors like Trait that get composed into other ctors."
//            let key = (ctor.name.Value)::prefix
//            let setChoice ix =
//                if betweenInclusive 0 (options.Length - 1) ix then
//                    dispatch (ClearStrictlyUnder key)
//                    dispatch (Queue (keyOf key options[ix]))
//            match options |> List.tryFindIndex(fun v -> has (keyOf key v)) with
//            | Some ix when ix > 0 ->
//                let incr _ = setChoice (ix+1)
//                let decr _ = setChoice (ix-1)
//                checkbox(has key, format.valueFormatter(ctor.name.Value, options[ix]),
//                    costOf (ctor.create options[ix]), key, None,
//                        fun () ->
//                            [   Html.button [prop.text "-"; prop.onClick decr]
//                                Html.button [prop.text "+"; prop.onClick incr]])
//            | _ ->
//                let incr _ = setChoice 1
//                checkbox(has key, format.valueFormatter(ctor.name.Value, options[0]),
//                    costOf (ctor.create options[0]), key, None,
//                        fun () ->
//                            [   Html.button [prop.text "-"; prop.disabled true]
//                                Html.button [prop.text "+"; prop.onClick incr]])
//        member this.chooseLevels(ctor, levels) = this.up.chooseLevels(ctor, levels, { valueFormatter = sprintf "%A" })

//        member _.chooseOne(ctor, options) =
//            let key = (ctor.name.Value)::prefix
//            let cost' =
//                match dataBuilder().chooseOne(ctor, options) with
//                | [v] -> Some (cost v)
//                | _ -> None
//            checkbox(has key, ctor.name.Value, cost', key, None, fun () -> refineN(1, ctor, Some (ctor.create >> cost), key, options))

//        member _.choose2D(ctor, options1, options2) =
//            let key = (ctor.name.Value)::prefix
//            let cost =
//                match dataBuilder().choose2D(ctor, options1, options2) with
//                | [v] -> cost v
//                | _ -> List.allPairs options1 options2 |> List.minBy' (ctor.create >> cost)
//                |> Some
//            checkbox(has key, ctor.name.Value, cost, key, None, fun () -> (refineN(1, ctor, None, key, options1))@(refineN(1, ctor, None, key, options2)))

//        member _.chooseOneFromHierarchy(ctor, lst) =
//            let key = keyOf prefix (ctor.name.Value)
//            let ancestorKey = key
//            let toggle parentKeys key select =
//                match parentKeys with
//                | [] -> ()
//                | ultimateAncestor::rest ->
//                    ClearStrictlyUnder ultimateAncestor |> dispatch
//                    for parentKey in rest do
//                        Queue parentKey |> dispatch
//                if select then Queue key |> dispatch
//                else Unqueue key |> dispatch
//            let makeReactElement = function
//                | Multi.Const v ->
//                    let key = (keyOf key v)
//                    [checkboxWithExplicitToggle(queue.ContainsKey key, (traitName (unbox v)), None, key, None, toggle [ancestorKey; key], thunk [])]
//                | Multi.One(ctor1, options1) ->
//                    let key = keyOf key ctor1.name.Value
//                    [checkboxWithExplicitToggle(has key, ctor1.name.Value, None, key, None, toggle [ancestorKey; key], thunk <| refineN(1, ctor1, None, key, options1))]
//                | Multi.DistinctTwo(ctor1, options1) ->
//                    let key = keyOf key ctor1.name.Value
//                    [checkboxWithExplicitToggle(has key, ctor1.name.Value, None, key, None, toggle [ancestorKey; key], thunk <| refineN(2, ctor1, None, key, options1))]
//            match dataBuilder().chooseOneFromHierarchy(ctor, lst) with
//            | [] ->
//                checkbox(has key, ctor.name.Value, None, key, None, thunk (lst |> List.collect makeReactElement))
//            | _ ->
//                let pick = lst |> List.find(function
//                    | Multi.Const v -> has (keyOf key v)
//                    | Multi.One(ctor, values) -> has (keyOf key ctor.name.Value) && (values |> List.filter (keyOf (keyOf key ctor.name.Value) >> has)).Length = 1
//                    | Multi.DistinctTwo(ctor, values) -> has (keyOf key ctor.name.Value) && (values |> List.filter (keyOf (keyOf key ctor.name.Value) >> has)).Length = 2
//                    )
//                let cost = (dataBuilder().chooseOneFromHierarchy(ctor, lst))[0] |> costOf
//                checkbox(has key, ctor.name.Value, cost, key, None, thunk1 makeReactElement pick)

//        member _.grantOne(ctor, options) =
//            let key = (ctor.name.Value)::prefix
//            // code smell: maybe tuple2bind1 is a code smell. Maybe Enhanced Parry and Luck are DIFFERENT.
//            if options.Length = 1 then
//                React.fragment (refineN(1, ctor, Some(ctor.create >> cost), key, options))
//            else
//                let cost = (dataBuilder().grantOne(ctor, options)) |> List.tryHead |> Option.map cost
//                checkbox(true, ctor.name.Value, cost, key, None, fun () -> refineN(1, ctor, None, key, options))

//        member _.grantWithStringInput(ctor, placeholder) =
//            let key = (ctor.name.Value)::prefix
//            checkbox(true, ctor.name.Value, (costOf (ctor.create "")), key, None,
//                            fun () -> [
//                                Html.input [
//                                    prop.type'.text
//                                    prop.placeholder placeholder
//                                    ]
//                                ])

//        member _.aggregate label elementsFunc =
//            let elements = elementsFunc (extend label)
//            (class' "aggregate" Html.div elements)
//        member _.chooseUpToBudget budget label elementsFunc =
//            let elements = elementsFunc (extend label)
//            let expenditures =
//                dataBuilder().chooseUpToBudget budget label elementsFunc
//                |> List.sumBy cost
//            Html.fieldSet [
//                Html.legend $"{label} ({expenditures}/{budget} points)"
//                (class' "aggregate" Html.div elements)
//                ]
//        member _.chooseUpToBudgetWithSuggestions budget label elementsFunc =
//            let (budgetsAndElements: int option * ReactElement list) = elementsFunc (extend label)
//            let chosen =
//                dataBuilder().chooseUpToBudget budget label elementsFunc
//            let expenditures =
//                dataBuilder().chooseUpToBudget budget label elementsFunc
//                |> List.sumBy cost
//            React.fragment [
//                        for (budget, section), expenditure in budgetsAndElements do
//                            if budget.IsSome then
//                                Html.fieldSet [
//                                        Html.legend $"{label} (at least {budget} points)"
//                                        (class' "aggregate" Html.div section)
//                                        ]
//                            ]
//    member private this.up = this :> OutputBuilder<_,_>

[<ReactComponent>]
let TraitView (profession: Templates.Package<Profession>, char: Character, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
    //let builder = (ReactBuilder(char, [], queue, dispatch))
    //class' "traitview" Html.fieldSet [
    //    classTxt' "subtitle" Html.legend profession.displayName
    //    Templates.menusFor builder profession.name
    //    ]
    notImpl()
