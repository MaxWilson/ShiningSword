module Menus

type KeySegment = string
type 't ReversedList = 't list
type Key = KeySegment ReversedList
/// we want to avoid letting sequences get cut off so we use StructuredFormatDisplay with custom logic
[<StructuredFormatDisplay("{DisplayText}")>]
type MenuOutput =
    | Either of label: string option * options: MenuSelection list
    | And of label: string option * grants: MenuOutput list
    | Leveled of label: string * Key * currentLevel: int * levelCount: int
    | Leaf of label: string
    with
    member this.DisplayText =
        let show lst = lst |> List.map String.structured |> String.concat ", "
        match this with
        | Either(None, children) -> $"Either({show children})"
        | Either(Some label, children) -> $"Either({label}, {show children})"
        | And(None, grants) -> $"And({show grants})"
        | And(Some label, grants) -> $"And({label}, {show grants})"
        | Leveled(label, key, currentLevel, levelCount) -> $"Leveled({label}, {key}, {currentLevel}, {levelCount})"
        | Leaf(label) -> $"Leaf({label})"
and MenuSelection = bool * Key * MenuOutput

type 't Output = 't * MenuOutput
type 't ListOutput = ('t list) Output
type 't OptionOutput = ('t option) Output

type MaybeLevel = Level of int | Flag
type LevelSpec<'arg, 't> = { ctor: 'arg -> 't; toString: 't -> string }

type OfferConfigCore = { // the core, public part of the offerConfig that we can expose publicly--no type parameters
    key: KeySegment option
    label: string option
    explicitUnselectedLabel: string option
    }
    with static member blank = { key = None; label = None; explicitUnselectedLabel = None }

type 't OfferConfig = { inner: OfferConfigCore; toString: ('t -> string) option } // specialized OfferConfig with extra information about creating labels
let blank() = { inner = OfferConfigCore.blank; toString = None }
type OfferInput = {
    selected: Map<Key, MaybeLevel>
    prefix: KeySegment ReversedList
    }
    with
    static member fresh = { selected = Map.empty; prefix = []; }
    member input.fullKey config =
        input.fullKey config.key
    member input.fullKey (segment: KeySegment option) =
        match segment with Some k -> k::input.prefix | None -> input.prefix
    member input.extend (config: OfferConfigCore) = { input with prefix = input.fullKey config }
    member input.extend (segment: KeySegment option) = { input with prefix = input.fullKey segment }
    member this.has (key: Key) = key = [] || this.selected.ContainsKey key
    member this.lookup (key: Key) = if key = [] then Some Flag else this.selected.TryFind key

type 't Offer = { config: OfferConfigCore; func: (OfferConfigCore -> OfferInput -> 't * MenuOutput) }
    with
    member this.recur input = this.func this.config input
    member this.LabelWhenUnselected (ix: int) =
        let config = this.config
        config.explicitUnselectedLabel |> Option.orElse config.label |> Option.defaultWith (fun () -> $"Option {ix+1}")

type 't ListOffer = ('t list) Offer
type 't OptionOffer = ('t option) Offer

type 'reactElement RenderApi = {
    checked': string * Key * ('reactElement list) -> 'reactElement
    unchecked: string * Key -> 'reactElement
    unconditional: string * ('reactElement list) -> 'reactElement
    leveledLeaf: string * Key * int * int -> 'reactElement // label, key, currentValue, levelCount
    combine: 'reactElement list -> 'reactElement
    }

let render (render: 'reactElement RenderApi) (menus: MenuOutput list) =
    let rec recur shortCircuit (render, renderMe: (string * 'reactElement list) -> 'reactElement) menu : 'reactElement =
        // renderMe lets us delay rendering of a checked/unchecked option inside an either until we have data about its label and children.

        let (|OneSelection|_|) lst =
            match lst |> List.filter Tuple3.get1 with
            | [true, key, v] -> Some (key, v)
            | _ -> None

        // if we're inside of a collapsed EITHER, then convert any and all leaves into checkboxes that can uncheck part of this EITHER and uncollapse it
        let collapsedRender key =
            { render with unconditional = fun (label, children) -> render.checked'(label, key, children) }

        match menu with
        | Either(None, OneSelection (key, child)) ->
            // if the either has no label and is already ready, just omit it from the visual tree and show the child directly. I'm not sure it's correct to ignore renderMe though.
            let renderChild (label, children) = render.checked'(label, key, children)
            recur shortCircuit (collapsedRender key, renderChild) child
        | Either(label, selections) ->
            let collapse = selections |> List.every (fun (isChecked, _, _) -> isChecked)
            let children = [
                if not shortCircuit then // don't show EITHER options in detail until the either itself has been selected
                    for (isChecked, key, child) in selections do
                        let renderChild (label: string, children) =
                            if isChecked then render.checked'(label, key, children) else render.unchecked(label, key)
                        let childReact = recur (not isChecked) ((if collapse then collapsedRender key else render), renderChild) child
                        childReact
                ]
            if collapse then
                render.combine children
            else
                renderMe(defaultArg label "Choose one:", children)
        | And(label, grants) ->
            let childReacts = grants |> List.map (recur shortCircuit (render, render.unconditional))
            match label with
            | Some label ->
                renderMe(label, childReacts)
            | None ->
                render.combine childReacts
        | Leveled(name, key, lvl, levelCount) -> render.leveledLeaf(name, key, lvl, levelCount)
        | Leaf(name) -> renderMe(name, [])
    menus |> List.map (recur false (render, render.unconditional)) |> render.combine

type 't EitherPattern = Choice<('t * MenuSelection list), ('t * MenuSelection list), ('t * MenuSelection list)> // convenience type helper to reduce duplication while avoiding type ambiguity. Don't feel bad if we wind up scrapping it.

type Op =
    static let configDefaultKey config key = if config.key.IsSome then config else { config with key = Some key }
    static let configDefaultBoth config key = if config.key.IsSome && config.label.IsSome then config else { config with key = Some (defaultArg config.key key); label = Some (defaultArg config.label key) }
    static let offer(config, logic) = { config = config; func = logic }
    static let eitherF (|Fulfilled|Partial|Fallback|) valueWhenUnselected options config =
        offer(
            { config.inner
                with explicitUnselectedLabel =
                        match config.inner.label, options with
                        | Some label, _ -> Some label
                        | None, [o] -> o.config.explicitUnselectedLabel |> Option.orElseWith (fun () -> (defaultArg config.toString String.structured) o |> Some)
                        | None, _ -> None
                },
            fun config input ->
                let children = [
                    for ix, o in options |> List.mapi Tuple2.create do
                        let key = o.config.key |> Option.orElse o.config.label
                        let fullKey = input.fullKey key
                        let selected = key.IsSome && input.has fullKey
                        if selected then
                            let value, menu = o.recur (input.extend key)
                            value, (selected, fullKey, menu)
                        else
                            valueWhenUnselected, (false, fullKey, Leaf (o.LabelWhenUnselected ix))
                    ]
                match children with
                | Fulfilled(value, childMenus) ->
                    // when we're at quota, exclude all the unpicked options from the menu unless and until some current selections are unpicked
                    value, Either(config.label, childMenus)
                | Partial(value, allChildMenus) ->
                    value, Either(config.label, allChildMenus)
                | Fallback(fallbackValue, allChildMenus) ->
                    fallbackValue, Either(config.label, allChildMenus)
            )
    static let andF config (offers: _ ListOffer list) =
        let unselectedLabel =
            match config.inner.label, offers with
            | Some label, _ -> Some label
            | None, [lhs;rhs] ->
                // promote label in the simple case
                Some $"{lhs.LabelWhenUnselected 0} and {rhs.LabelWhenUnselected 1}"
            | _ -> None
        offer(
            { config.inner
                with
                explicitUnselectedLabel = unselectedLabel
                key = config.inner.key |> Option.orElse config.inner.label |> Option.orElse unselectedLabel
                      |> Option.orElseWith (fun () -> offers |> List.collect (_.config.key >> List.ofOption) |> String.join " and " |> Some)
                },
            fun config input ->
                let children = [
                    for o in offers do
                        // we only need the key to distinguish between eithers, not ands, so we extend the input by the child key only for either
                        let (value, menu) = o.recur input
                        value, menu
                    ]
                let selectedValues = children |> List.collect fst
                let noMissingValues = children |> List.exists (fst >> List.isEmpty)
                let childMenus = children |> List.map snd
                if noMissingValues then
                    selectedValues, And(config.label, childMenus)
                else
                    selectedValues, And(None, childMenus)
            )


    static member trait' (v: 't): 't OptionOffer =
        Op.trait'({ inner = OfferConfigCore.blank; toString = None }, v)
    static member trait' (config: 't OfferConfig, v): 't OptionOffer =
        let render = (defaultArg config.toString String.structured)
        offer(configDefaultBoth config.inner (render v), fun config input -> Some v, (Leaf (defaultArg config.label (render v))))

    static member level (name: string, spec: LevelSpec<int, 't>, levels: int list): 't OptionOffer =
        Op.level({ blank() with toString = Some spec.toString }, (name, spec, levels))
    static member level (config, (name: string, spec: LevelSpec<int, 't>, levels: int list)): 't OptionOffer =
        let config = {
            config
            with
                inner.key = config.inner.key |> Option.orElse (Some name) // use the generic name as the key, not the specific level which changes over time as the user clicks
                inner.explicitUnselectedLabel = config.inner.explicitUnselectedLabel |> Option.orElse (Some $"{spec.ctor levels[0] |> spec.toString }")
            }
        offer(config.inner, fun config input ->
            let fullKey =
                match config.key, input.prefix with
                // code smell: there's probably a more elegant way to separate the either/level interactions with key
                | Some key, head::_ when key = head -> input.prefix // if we're inside an either then it has already added our key to its prefix
                | _ -> input.fullKey config.key // otherwise kludge: if we're not inside an either, we need to extend the prefix so we can have unique data for this control
            let level ix =
                let level = levels[ix] // e.g. if this is skill("Rapier", [+5..+8]) then ix 0 means level = +5 and value = Rapier +5
                let value = spec.ctor level
                Some value, Leveled(defaultArg config.label $"{spec.toString value}", fullKey, ix, levels.Length)
            match input.lookup fullKey with
            | Some (Level lvl) when lvl < levels.Length -> level lvl
            | _ when levels.Length >= 1 -> // we are permissive in the input we accept, partly to make testing easier. Flag means "default to the lowest value", e.g. Rapier +5-+7 defaults to Rapier +5.
                level 0
            | _ -> shouldntHappen "A leveled option with no levels is nonsense"
            )

    static member budget (budgetF, offers: 't ListOffer list) =
        Op.budget(blank(), budgetF, offers)
    static member budget (budgetF, offers: 't OptionOffer list) =
        Op.budget(blank(), budgetF, offers |> List.map Op.promote)
    static member budget (config, budgetF: 't list -> int, offers: 't OptionOffer List) : 't ListOffer =
        Op.budget(config, budgetF, offers |> List.map Op.promote)
    static member budget (config, budgetF: 't list -> int, offers: 't ListOffer List) : 't ListOffer =
        let (|Fulfilled|Partial|Fallback|) (children: ('t list * MenuSelection) list) : 't list EitherPattern =
            match children |> List.filter (function _, (true, _, _) -> true | _ -> false) with
            | lst when lst.Length > 0 ->
                let values = lst |> List.collect fst
                let remainingBudget = budgetF values
                if remainingBudget <= 0 then
                    Fulfilled(values, lst |> List.map snd) // return only the selected menus, in case they want to unselect something
                else
                    Partial(values, children |> List.map snd) // return all child menus so user can keep selecting
            | _ -> Fallback([], children |> List.map snd) // return all child menus so user can keep selecting
        eitherF (|Fulfilled|Partial|Fallback|) [] offers config

    static member either options : 't OptionOffer =
        Op.either(blank(), options)
    static member either (config, options: 't OptionOffer list) : 't OptionOffer =
        let (|Fulfilled|Partial|Fallback|) (children: ('t option * MenuSelection) list) : 't option EitherPattern =
            match children |> List.tryFind (function _, (true, _, _) -> true | _ -> false) with
            | Some(value, childMenu) -> Fulfilled(value, [childMenu])
            | None when false -> Partial(None, children |> List.map snd)
            | None -> Fallback(None, children |> List.map snd)
        eitherF (|Fulfilled|Partial|Fallback|) None options config

    static member eitherN (options: 't OptionOffer list) : 't ListOffer =
        Op.eitherN(blank(), 1, options)
    static member eitherN (options: 't ListOffer list) : 't ListOffer =
        Op.eitherN(blank(), 1, options)
    static member eitherN (config, n: int, options: 't OptionOffer list) : 't ListOffer =
        Op.eitherN(config, n, options |> List.map (fun o -> Op.promote o))
    static member eitherN (config, n: int, options: 't ListOffer list) : 't ListOffer =
        let (|Fulfilled|Partial|Fallback|) (children: ('t list * MenuSelection) list) : 't list EitherPattern =
            match children |> List.filter (function _, (true, _, _) -> true | _ -> false) with
            | lst when lst.Length = n -> Fulfilled(lst |> List.collect fst, lst |> List.map snd)
            | lst when lst.Length > 0 -> Partial(lst |> List.collect fst, children |> List.map snd) // return all child menus so user can keep selecting
            | _ -> Fallback([], children |> List.map snd) // return all child menus so user can keep selecting
        eitherF (|Fulfilled|Partial|Fallback|) [] options config

    static member and' (offers: 't OptionOffer list) : 't ListOffer =
        andF (blank()) (offers |> List.map (fun o -> Op.promote o))
    static member and' (offers: 't ListOffer list) : 't ListOffer =
        andF (blank()) offers
    static member and' (config, offers: 't OptionOffer list) : 't ListOffer =
        andF config (offers |> List.map (fun o -> Op.promote o))
    static member and' (config: 't OfferConfig, offers: 't ListOffer list) : 't ListOffer =
        andF config offers

    static member promote (o: 't OptionOffer): 't ListOffer =
        offer(
            o.config,
            fun config input ->
                let (v, menu) = o.recur input
                List.ofOption v, menu
            )
let evaluate (input: OfferInput) (offer: _ Offer) =
    // we need to be able to distinguish between eithers if they are at the root, so extend the prefix by the root key if one exists
    offer.recur (input.extend offer.config.key)

open type Op


