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

let (|PickedIndex|_|) isSatisfied ((kind, args) as options: LabeledChoiceOption) =
    match args |> List.tryFindIndex isSatisfied with
    | Some ix -> Some ix
    // if there's only one alternative, that's the same as no alternatives. Pick it.
    | None when args.Length = 1 -> Some 0
    // Leveled defaults to the first option even if there are more, as long as the choice is selected. User will use -/+ to upscale it.
    | None when kind = Leveled && args.Length >= 1 -> Some 0
    | _ -> None

let (|PickedArgAndLabel|_|) isSatisfied = function
    | PickedIndex isSatisfied ix as (_,options) -> Some options[ix]
    | _ -> None

let (|PickedArg|_|) isSatisfied = function
    | PickedIndex isSatisfied ix as (_,options) -> Some (fst options[ix])
    | _ -> None

// Essentially a trait has three stats: 1. Already exists on the underlying character,
// 2. being added (in queue), or 3. not present at all. E.g. Swashbuckler who's going
// up a level already has Combat Reflexes, but may be adding Enhanced Time Sense
// for 30 points, and doesn't have Kiai at all. ReactBuilder is for adding stuff to
// the queue, while DataBuilder is for extracting all of the stuff in the queue and
// dispatching it to the underlying character.
[<AutoOpen>]
module DataBuilder =
    type DataCtx = { prefix: Key; queue: Map<Key, string>; includePotentials: bool }
        with static member fresh = { prefix = []; queue = Map.empty; includePotentials = false }
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
    let isSatisfied ctx =
        hasKey ctx ctx.prefix

    // modifies ctx so that the given metadata will consider to have been chosen, no matter what the user has selected
    let ctxAugment meta ctx =
        let key = (keyOf ctx.prefix meta)
        if ctx.queue.ContainsKey key |> not then
            { ctx with queue = ctx.queue |> Map.add key "" }
        else ctx
    // for a limited subset of things that make sense to grant, such as binary and choose, unpacks their metadata and augments
    let ctxGrantOne ctx = function
            | Binary(meta, _)
            | Choose(meta, _)
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

    let rec ofMany (ctx: DataCtx) = function
        | Aggregate(meta: Metadata, manyList: 't Many list) ->
            manyList |> List.collect (ofMany (extend ctx meta))
        | ChoosePackage(packages: (Metadata * 't Many) list) ->
            // unlike items, we can only pick one package. E.g. a Swashbuckler
            // can't pick the 20-point package for Sword! and also pick
            // the 20-point package for Sword and Dagger.
            let evalAggregate (meta: Metadata, many: 't Many) =
                if has ctx meta || ctx.includePotentials then
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
        | Binary(meta: Metadata, v: 't) -> [if has ctx meta || ctx.includePotentials then v]
        | Choose(meta: Metadata, choose: 't Choose) -> [
            // Leveled and Selection are similar in logic but Leveled will show -/+ buttons on UI,
            // and Leveled also defaults to the first item
            if has ctx meta || ctx.includePotentials then
                let ctx = extend ctx meta
                let pack, unpack = viaAny<'t list>()
                let f = {   new Polymorphic<Constructor<Any, 't> * LabeledChoiceOption, Any>
                                with
                                member _.Apply ((ctor, ((_, args) as options))) =
                                    let attempt (_, txt) =
                                        let meta = Metadata.key' txt
                                        let ctx = extend ctx meta
                                        isSatisfied ctx

                                    match options with
                                    | PickedArg attempt chosenArg -> [chosenArg |> ctor.create] |> pack
                                    | _ when ctx.includePotentials -> args |> List.map (fst >> ctor.create) |> pack
                                    | _ -> [] |> pack }
                yield! choose.generate f |> unpack
            ]
        | Choose2D(meta: Metadata, choose2d: 't Choose2D) -> [
            if has ctx meta || ctx.includePotentials then
                let ctx = extend ctx meta
                let pack, unpack = viaAny<'t list>()
                let f = {   new Polymorphic<Constructor<Any * Any, 't> * LabeledChoiceOption * LabeledChoiceOption, Any>
                                with
                                member _.Apply ((ctor, ((_, args1) as options1), ((_, args2) as options2))) =
                                    let attempt (_, txt) =
                                        let meta = Metadata.key' txt
                                        let ctx = extend ctx meta
                                        isSatisfied ctx

                                    let keys = ctx.queue.Keys |> Array.ofSeq

                                    match options1, options2 with
                                    | PickedArg attempt chosenArg1, PickedArg attempt chosenArg2 -> [ctor.create(chosenArg1, chosenArg2)] |> pack
                                    | PickedArg attempt chosenArg1, _ when ctx.includePotentials -> args2 |> List.map fst |> List.map (fun arg2 -> ctor.create(chosenArg1, arg2)) |> pack
                                    | _, PickedArg attempt chosenArg2 when ctx.includePotentials -> args1 |> List.map fst |> List.map (fun arg1 -> ctor.create(arg1, chosenArg2)) |> pack
                                    | _ when ctx.includePotentials -> List.allPairs (args1 |> List.map fst) (args2 |> List.map fst) |> List.map ctor.create |> pack
                                    | _ -> [] |> pack}
                yield! choose2d.generate f |> unpack
            ]
        | ChooseWithStringInput(meta: Metadata, ctor: Constructor<string, 't>, placeholder: string) -> [
            if has ctx meta || ctx.includePotentials then
                let key = keyOf ctx.prefix meta
                ctor.create(ctx.queue[key])
            ]
        | Grant(meta: Metadata, v: 't OneResult) ->
            v |> ofOne (ctxGrantOne (ctx |> ctxAugment meta) v)
        | ChooseOneFromHierarchy(meta: Metadata, hierarchy: 't OneHierarchy) ->
            hierarchy |> ofHierarchy (extend ctx meta)
    and ofHierarchy (ctx: DataCtx) = function
        | Leaf(meta: Metadata, v: 't) -> [
            if has ctx meta || ctx.includePotentials then
                v
            ]
        | Interior(meta: Metadata, hierarchy: 't OneHierarchy list) -> [
            if has ctx meta || ctx.includePotentials then
                yield! (hierarchy |> List.collect (ofHierarchy (extend ctx meta)))
            ]
    let dataBuilder =
        ofMany

[<ReactComponent>]
let VisuallyGroup label (elements: ReactElement list) =
    let collapsed, setCollapsed = React.useState false
    match label with
    | Some (label: string) ->
        class' "visualGroup" Html.fieldSet [
            Html.legend label
            Html.button [   prop.text (if collapsed then "Expand" else "Collapse")
                            prop.onClick (fun _ -> setCollapsed (not collapsed))]
            if not collapsed then
                yield! elements
            ]
    | None ->
        class' "visualGroup" Html.div elements

[<AutoOpen>]
module ReactBuilder =
    type ReactCtx = { char: Character; prefix: Key; queue: Map<Key, string>; dispatch: TraitMsg -> unit; collapsing: bool }
        with
        static member fresh char = { char = char; prefix = []; queue = Map.empty; dispatch = ignore; collapsing = false }
        static member create(char, queue, dispatch: TraitMsg -> unit) = { char = char; prefix = []; queue = queue; dispatch = dispatch; collapsing = false }
    let toDataCtx (ctx: ReactCtx) : DataCtx =
        { prefix = ctx.prefix; queue = ctx.queue; includePotentials = false }
    let keyOf (prefix: Key) (meta: Metadata) : Key =
        match meta.keySegment with
        | Some key ->
            key::prefix
        | None -> prefix
    let extend (ctx: ReactCtx) (meta:Metadata) =
        { ctx with prefix = keyOf ctx.prefix meta }
    let hasKey ctx key =
        ctx.queue.ContainsKey key
    let has ctx meta =
        hasKey ctx (keyOf ctx.prefix meta)
    let isSatisfied ctx =
        hasKey ctx ctx.prefix
    // modifies ctx so that the given metadata will consider to have been chosen, no matter what the user has selected
    let ctxAugment meta ctx =
        let key = (keyOf ctx.prefix meta)
        if ctx.queue.ContainsKey key |> not then
            { ctx with queue = ctx.queue |> Map.add key "" }
        else ctx
    // for a limited subset of things that make sense to grant, such as binary and choose, unpacks their metadata and augments
    let ctxGrantOne ctx = function
            | Binary(meta, _)
            | Choose(meta, _)
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

    let chkId (key:Key) =
        ("chk" + (key |> String.concat "-")).Replace(" ", "")

    //let binary (isGrant, cost, prefix: Key, (label:string option), value) =
    //    match value with
    //    | Data.Trait trait1 as value ->
    //        let label = label |> Option.defaultWith (fun _ -> trait1 |> traitName)
    //        let key = keyOf prefix value
    //        let isQueued = isGrant || queue |> Map.containsKey key
    //        // if it's a grant, then we don't care what its chkId is because clicking it will have no effect.
    //        // reserve the "real" id for something later.
    //        let chkId = chkId key trait1
    //        checkbox(isQueued, label, cost, key, Some chkId, fun () -> [])
    //    | _ -> notImpl() // probably not needed--why would we ever have a binary skill/attribute mod instead of a level?

    let toggle dispatch key select =
        if select then dispatch (Queue key)
        else dispatch (Unqueue key)
    [<ReactComponent>]
    let checkbox(ctx: ReactCtx, meta: Metadata, label', cost: int option, toggleFunc: (Key -> bool -> unit) option, ifChecked: (unit -> ReactElement) option) =
    #if DEBUG
        // assume: ctx already has meta in it
        match meta.keySegment with
        | Some k when ctx.prefix.Head <> k ->
            shouldntHappen $"The ctx passed to checkbox should always already be extended with meta (prefix={ctx.prefix}, meta.keySegment={k})"
        | _ -> ()
    #endif
        let key = ctx.prefix
        let chkId = (chkId key)
        let isChecked = isSatisfied ctx // do not use has ctx meta! because ctx already has meta in it.
        let label' = label' |> Option.defaultWith (fun _ -> meta.label |> Option.defaultWith (fun _ -> meta.keySegment.Value))
        let elements = [
            Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked isChecked; prop.readOnly true; prop.onChange (match toggleFunc with Some f -> f key | None -> toggle ctx.dispatch key)]
            let txt = match cost with Some cost -> $"{label'} [{cost}]" | _ -> label'
            Html.label [prop.text txt; prop.htmlFor chkId]
            match ifChecked with
            | Some f when isChecked -> f()
            | _ -> ()
            ]
        if ctx.collapsing && isChecked then class' "potentialChoice" Html.span elements else class' "potentialChoice" Html.div elements
    let placeholder v = Html.div (v.ToString() |> fun s -> $"placeholder {s.Substring(0, min s.Length 30)}...")
    let rec ofMany (ctx: ReactCtx) many =
        match many with
        | Aggregate(meta: Metadata, manyList: _ Many list) ->
            let ctx = extend ctx meta
            VisuallyGroup meta.label [
                for many in manyList do // should we have a border or something?
                    (ofMany ctx many: ReactElement)
                ]
        | ChoosePackage(packages: (Metadata * _ Many) list) ->
            // unlike items, we can only pick one package. E.g. a Swashbuckler
            // can't pick the 20-point package for Sword! and also pick
            // the 20-point package for Sword and Dagger.
            placeholder """ChoosePackage: let evalAggregate (meta: Metadata, many: 't Many) =
                if has ctx meta then
                    Some (many |> ofMany (extend ctx meta))
                else None
            packages |> List.tryPick evalAggregate |> Option.defaultValue []"""
        | GrantItems(many: _ Many) ->
            ofMany (ctxGrantMany ctx many) many
        | Items(meta: Metadata, items: _ OneResult list) ->
            items |> List.map (ofOne (extend ctx meta)) |> VisuallyGroup meta.label
        | Budget(budget: int, meta: Metadata, items: _ OneResult list) ->
            // TODO: enforce budgets to whatever extent is appropriate
            let cost = DataBuilder.ofMany (toDataCtx ctx) many |> List.sumBy cost
            let txt = String.join " " [defaultArg meta.label ""; $"{cost}/{budget} points"]
            let ctx = extend ctx meta
            VisuallyGroup (Some txt) [
                for item in items do // should we have a border or something?
                    (ofOne ctx item: ReactElement)
                ]
        | NestedBudgets(totalBudget: int, meta: Metadata, suggestions: (int option * Metadata * _ OneResult list) list) ->
            // TODO: enforce budgets to whatever extent is appropriate
            let cost = DataBuilder.ofMany (toDataCtx ctx) many |> List.sumBy cost
            let txt = String.join " " [defaultArg meta.label ""; $"{cost}/{totalBudget} points"]
            let ctx = extend ctx meta
            VisuallyGroup (Some txt) [
                for budget, meta, items in suggestions do
                    let cost = DataBuilder.ofMany (toDataCtx ctx) (Items(meta, items)) |> List.sumBy Data.cost
                    let txt2 = String.join " " [defaultArg meta.label ""; (match budget with Some budget -> $"{cost}/{budget} points" | None -> $"{cost} points")]
                    VisuallyGroup (Some txt2) [
                        yield! items |> List.map (ofOne (extend ctx meta))
                        ]
                ]
    and ofOne (ctx: ReactCtx) (one:Chosen OneResult) : ReactElement =
        match one with
        | Binary(meta: Metadata, v) -> checkbox(extend ctx meta, meta, Some(Format.value ctx.char.stats v), Some (cost v), None, None)
        | Choose(meta: Metadata, choose: _ Choose) ->
            let cost' = DataBuilder.ofOne (toDataCtx ctx) one |> List.tryHead |> Option.map cost
            let ctx = { extend ctx meta with collapsing = true }
            let rootctx = ctx
            let rootKey = rootctx.prefix
            let rootMeta = meta

            let pack, unpack = viaAny<ReactElement>()
            let f = {   new Polymorphic<Constructor<Any, Chosen> * LabeledChoiceOption, Any>
                            with
                            member _.Apply ((ctor, ((kind, args) as options))) =
                                let attempt (_, txt) =
                                    let meta = Metadata.key' txt
                                    let ctx = extend ctx meta
                                    isSatisfied ctx
                                let selection ctx (chosenArg, txt) =
                                    let meta = Metadata.key' txt
                                    let ctx = extend ctx meta
                                    let trait1 = chosenArg |> ctor.create
                                    let cost = cost trait1
                                    checkbox(ctx, meta, Some txt, Some cost, None, None)
                                let levels ctx ix =
                                    let keyOf j = keyOf ctx.prefix (Metadata.key' (args[j] |> snd))
                                    let setChoice ix' =
                                        if betweenInclusive 0 (args.Length - 1) ix' then
                                            ctx.dispatch (Unqueue (keyOf ix))
                                            ctx.dispatch (Queue (keyOf ix'))
                                    let incr _ = setChoice (ix+1)
                                    let decr _ =
                                        if ix > 0 then
                                            setChoice (ix-1)
                                        elif ix = 0 then
                                            ctx.dispatch (Unqueue rootKey) // unqueue the root item because the user doesn't want the trait at all

                                    let trait1 = ctor.create (args[ix] |> fst)
                                    let txt = $"{trait1 |> Format.name} [{cost trait1}]"
                                    txt, React.fragment [
                                            if args.Length > 1 then
                                                Html.button [prop.text "-"; prop.onClick decr]
                                                Html.button [prop.text "+"; prop.onClick incr; prop.disabled (ix >= args.Length - 1)]
                                            ]
                                match options, kind with
                                | PickedIndex attempt ix, Leveled ->
                                    let txt, fragment = levels ctx ix
                                    checkbox(rootctx, rootMeta, Some txt, cost', None, Some (thunk fragment)) |> pack
                                | PickedArgAndLabel attempt chosenArg, Selection ->
                                    checkbox(rootctx, rootMeta, Some ctor.name.Value, None, None, Some (fun () -> selection ctx chosenArg))
                                    |> pack
                                | _, Leveled ->
                                    shouldntHappen()
                                | _, Selection ->
                                    // otherwise, show all the options
                                    checkbox(rootctx, rootMeta, Some ctor.name.Value, None, None, Some (fun () ->
                                        args |> List.map (selection ctx) |> VisuallyGroup None))
                                    |> pack
                        }
            [   choose.generate f |> unpack
                ] |> VisuallyGroup None
        | Choose2D(meta: Metadata, choose2d: _ Choose2D) ->
            let cost' = DataBuilder.ofOne (toDataCtx ctx) one |> List.tryHead |> Option.map cost
            let ctx = { extend ctx meta with collapsing = true }
            let rootctx = ctx
            let rootKey = rootctx.prefix
            let rootMeta = meta
            let toggleChoice key select =
                if select then
                    ctx.dispatch (Queue key)
                else
                    ctx.dispatch (Unqueue key)

            let pack, unpack = viaAny<ReactElement>()
            let f = {   new Polymorphic<Constructor<Any * Any, Chosen> * LabeledChoiceOption * LabeledChoiceOption, Any>
                            with
                            member _.Apply ((ctor, ((kind1, args1) as options1), ((kind2, args2) as options2))) =
                                let attempt (_, txt) =
                                    let meta = Metadata.key' txt
                                    let ctx = extend ctx meta
                                    isSatisfied ctx
                                let selection ctx (chosenArg, txt) =
                                    let meta = Metadata.key' txt
                                    let ctx = extend ctx meta
                                    checkbox(ctx, meta, Some txt, None, None, None)
                                let levels (args: _ list) ctx ix =
                                    let keyOf j = keyOf ctx.prefix (Metadata.key' (args[j] |> snd))
                                    let setChoice ix' =
                                        if betweenInclusive 0 (args.Length - 1) ix' then
                                            ctx.dispatch (Unqueue (keyOf ix))
                                            ctx.dispatch (Queue (keyOf ix'))
                                    let incr _ = setChoice (ix+1)
                                    let decr _ =
                                        if ix > 0 then
                                            setChoice (ix-1)
                                        elif ix = 0 then
                                            printfn $"{(Unqueue rootKey)}"
                                            ctx.dispatch (Unqueue rootKey) // unqueue the root item because the user doesn't want the trait at all

                                    let txt = $"{ctor.name.Value} {args[ix] |> snd}"
                                    txt, React.fragment [
                                            if args.Length > 1 then
                                                Html.button [prop.text "-"; prop.onClick decr]
                                                Html.button [prop.text "+"; prop.onClick incr; prop.disabled (ix >= args.Length - 1)]
                                            ]

                                // collapse any choice which is already picked down to the picked option, so the user can uncheck it if desired
                                // otherwise show all

                                let getTxtAndFragments ((kind, args) as options) =
                                    match options, kind with
                                    | PickedIndex attempt ix, Leveled ->
                                        let txt, fragment = levels args ctx ix
                                        [txt], [fragment]
                                    | PickedArgAndLabel attempt chosenArg, Selection ->
                                        [], [selection ctx chosenArg]
                                    | _ -> [], [for arg in args do selection ctx arg]
                                let append2 (lhs1, lhs2) (rhs1, rhs2) = lhs1@rhs1, lhs2@rhs2
                                let txts, fragments = append2 (getTxtAndFragments options1) (getTxtAndFragments options2)
                                let txt = match txts with
                                          | [] -> ctor.name.Value
                                          | t -> String.concat " " t
                                checkbox(rootctx, rootMeta, Some txt, cost', None, Some (fun () -> React.fragment fragments))
                        }
            [   choose2d.generate f |> unpack
                ] |> VisuallyGroup None
        | ChooseWithStringInput(meta: Metadata, ctor: Constructor<string, _>, placeholder: string) ->
            let cost = ctor.create "" |> cost
            let ctx = extend ctx meta
            let label =
                if isSatisfied ctx && String.isntWhitespace ctx.queue[ctx.prefix] then
                    $"{ctor.name} ({ctx.queue[ctx.prefix]})"
                else $"{ctor.name}"
                |> String.uncamel
            let textEntry () =
                if String.isntWhitespace ctx.queue[ctx.prefix] then Html.div []
                else
                    let sendData (txt: string) =
                        QueueData(ctx.prefix, txt) |> ctx.dispatch
                    TextEntryForm(placeholder, sendData)
            [   checkbox(ctx, meta, Some label, Some cost, None, Some textEntry)
                ] |> VisuallyGroup None
        | Grant(meta: Metadata, v: _ OneResult) ->
            v |> ofOne (ctxGrantOne (ctx |> ctxAugment meta) v)
        | ChooseOneFromHierarchy(meta: Metadata, hierarchy: _ OneHierarchy) ->
            hierarchy |> ofHierarchy (extend ctx meta) |> placeholder
    and ofHierarchy (ctx: ReactCtx) = function
        | Leaf(meta: Metadata, v) -> [
            if has ctx meta then
                v
            ]
        | Interior(meta: Metadata, hierarchy: _ OneHierarchy list) -> [
            if has ctx meta then
                yield! (hierarchy |> List.collect (ofHierarchy (extend ctx meta)))
            ]
    let reactBuilder : ReactCtx -> Chosen Many -> ReactElement =
        ofMany

[<ReactComponent>]
let TraitView (profession: Templates.Package<Profession>, char: Character, queue: Map<Key, string>, dispatch: TraitMsg -> unit) =
    let builder = reactBuilder(ReactCtx.create(char, queue, dispatch))
    class' "traitview" Html.fieldSet [
        classTxt' "subtitle" Html.legend profession.displayName
        (Templates.menusFor profession.name |> builder)
        ]
