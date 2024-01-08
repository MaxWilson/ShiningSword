module Chargen.Accept

open Expecto
open Swensen.Unquote

// okay, for each leaf/level/composition, we need to know:
// 1. If it's checkable, what is the current state?
// 2. If it's checkable, how do we change the state?
// 3. What is the label, if any? If there's no explicit label, how do we display it? (Might depend on whether or not it's checked or finished.)
//    3a. Do we ever NOT have a label? I.e. are there any headless UI components? I think maybe we want to collapse fulfilled eithers.

type KeySegment = string
type 't ReversedList = 't list
type Key = KeySegment ReversedList
type MenuOutput =
    | Either of label: string option * options: MenuSelection list
    | And of label: string option * grants: MenuOutput list
    | Leveled of label: string * level: int
    | Leaf of label: string
and MenuSelection = bool * Key * MenuOutput

type 't Output = 't * MenuOutput
type 't ListOutput = ('t list) Output
type 't OptionOutput = ('t option) Output

type OfferConfig = {
    key: KeySegment option
    label: string option
    }
    with static member blank = { key = None; label = None }
type MaybeLevel = Level of int | Flag
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
    member input.extend (config: OfferConfig) = { input with prefix = input.fullKey config }
    member input.extend (segment: KeySegment option) = { input with prefix = input.fullKey segment }

type 't Offer = { config: OfferConfig; func: (OfferConfig -> OfferInput -> 't * MenuOutput) }
    with
    member this.recur input = this.func this.config input
type 't ListOffer = ('t list) Offer
type 't OptionOffer = ('t option) Offer

open type OfferConfig

type 'reactElement RenderApi = {
    checked': string * ('reactElement list) -> 'reactElement
    unchecked: string -> 'reactElement
    unconditional: string * ('reactElement list) -> 'reactElement
    leaf: string -> 'reactElement
    leveledLeaf: string * int -> 'reactElement
    combine: 'reactElement list -> 'reactElement
    }

let render (render: 'reactElement RenderApi) (menus: MenuOutput list) =
    let rec recur recurOnChildren (renderMe: (string * 'reactElement list) -> 'reactElement) menu : 'reactElement =
        let (|OneSelection|_|) lst =
            match lst |> List.filter Tuple3.get1 with
            | [true, _, v] -> Some v
            | _ -> None
        match menu with
        | Either(None, OneSelection child) ->
            recur recurOnChildren render.checked' child // if the either has no label and is already ready, just omit it from the visual tree and show the child directly. I'm not sure it's correct to ignore renderMe though.
        | Either(label, selections) ->
            let children = [
                if recurOnChildren then
                    for (isChecked, _, child) in selections do
                        let renderChild (label: string, children) =
                            if isChecked then render.checked'(label, children) else render.unchecked label
                        let childReact = recur isChecked renderChild child
                        childReact
                ]
            renderMe(defaultArg label "Choose one:", children)
        | And(label, grants) ->
            let childReacts = grants |> List.map (recur true render.unconditional)
            renderMe(defaultArg label "And:", childReacts)
        | Leveled(name, lvl) -> render.leveledLeaf(name, lvl)
        | Leaf(name) -> render.leaf name
    menus |> List.map (recur true render.unconditional) |> render.combine

type Op =
    static let configDefaultKey config key = if config.key.IsSome then config else { config with key = Some key }
    static member offer(config, func) = { config = config; func = func }
    static member offer func = Op.offer(OfferConfig.blank, func)

    static member skill (name: string, ctor: int -> 't, level: int): 't OptionOffer =
        Op.skill({ OfferConfig.blank with label = Some $"{name} %+d{level}" }, (name, ctor, [level]))
    static member skill (name: string, ctor: int -> 't, levels: int list): 't OptionOffer =
        Op.skill(OfferConfig.blank, (name, ctor, levels))
    static member skill (config, (name: string, ctor: int -> 't, levels: int list)): 't OptionOffer =
        Op.offer(configDefaultKey config name, fun config input ->
            let fullKey = input.prefix // no need to extend the prefix because only one key is possible--we're not in an either here
            let level ix =
                let level = levels[ix] // e.g. if this is skill("Rapier", [+5..+8]) then ix 0 means level = +5 and value = Rapier +5
                let value = ctor level
                Some value, Leveled(defaultArg config.label $"{value}", ix)
            match input.selected.TryFind fullKey with
            | Some (Level lvl) when lvl < levels.Length -> level lvl
            | Some Flag when levels.Length = 1 -> // we are permissive in the input we accept, partly to make testing easier. You can set Flag on a Levelled property as long as it has only one value, e.g. Rapier +5 can be selected
                level 0
            | _ ->
                None, (Leaf (defaultArg config.label (toString name)))
            )

    static member trait' (v: 't): 't OptionOffer =
        Op.trait'({ OfferConfig.blank with label = Some (toString v) }, v)
    static member trait' (config, v): 't OptionOffer =
        Op.offer(configDefaultKey config (toString v), fun config input -> Some v, (Leaf (defaultArg config.label (toString v))))

    static member budgeted v: 't ListOffer = notImpl()

    static member either options : 't OptionOffer =
        Op.either(OfferConfig.blank, options)
    static member either (config, options: 't OptionOffer list) : 't OptionOffer =
        Op.offer(
            config,
            fun config input ->
                let children = [
                    for ix, o in options |> List.mapi Tuple2.create do
                        let key = o.config.key |> Option.orElse o.config.label
                        let fullKey = input.fullKey key
                        let selected = key.IsSome && input.selected.ContainsKey fullKey
                        if selected then
                            let value, menu = o.recur (input.extend key)
                            value, (true, fullKey, menu)
                        else
                            None, (false, fullKey, Leaf (defaultArg o.config.label $"Option {ix}"))
                    ]
                match children |> List.tryFind (function _, (true, _, _) -> true | _ -> false) with  // if this were eitherN we'd return them all but since it's regular either we return the first one selected, if any
                | Some (value, childMenu) ->
                    value, Either(config.label, [childMenu]) // exclude all the unpicked options from the menu unless and until the current selection is unpicked
                | None ->
                    let allChildMenus = children |> List.map snd
                    None, Either(config.label, allChildMenus)
            )

    static member eitherN (options: 't OptionOffer list) : 't ListOffer =
        Op.eitherN(OfferConfig.blank, 1, options)
    static member eitherN (options: 't ListOffer list) : 't ListOffer =
        Op.eitherN(OfferConfig.blank, 1, options)
    static member eitherN (config, n: int, options: 't OptionOffer list) : 't ListOffer =
        Op.eitherN(config, n, options |> List.map (fun o -> Op.promote o))
    static member eitherN (config, n: int, options: 't ListOffer list) : 't ListOffer =
        Op.offer(
            config,
            fun config input ->
                let children = [
                    for ix, o in options |> List.mapi Tuple2.create do
                        let key = o.config.key |> Option.orElse o.config.label
                        let fullKey = input.fullKey key
                        let selected = key.IsSome && input.selected.ContainsKey fullKey
                        if selected then
                            let value, menu = o.recur (input.extend key)
                            value, (selected, fullKey, menu)
                        else
                            [], (false, fullKey, Leaf (defaultArg o.config.label $"Option {ix}"))
                    ]
                match children |> List.filter (function _, (true, _, _) -> true | _ -> false) with
                | lst when lst.Length = n ->
                    // when we're at quota, exclude all the unpicked options from the menu unless and until some current selections are unpicked
                    let values = lst |> List.collect fst
                    let childMenus = lst |> List.map snd
                    values, Either(config.label, childMenus)
                | _ ->
                    let allChildMenus = children |> List.map snd
                    [], Either(config.label, allChildMenus)
            )
    static member and' (offers: 't OptionOffer list) : 't ListOffer =
        Op.and'(OfferConfig.blank, offers)
    static member and' (offers: 't ListOffer list) : 't ListOffer =
        Op.and'(OfferConfig.blank, offers)
    static member and' (config, offers: 't OptionOffer list) : 't ListOffer =
        Op.and'(config, offers |> List.map (fun o -> Op.promote o))
    static member and' (config, offers: 't ListOffer list) : 't ListOffer =
        Op.offer(
            config,
            fun config input ->
                let children = [
                    for o in offers do
                        // we only need the key to distinguish between eithers, not ands, so we extend the input by the child key only for either
                        let (value, menu) = o.recur input
                        value, menu
                    ]
                let selectedValues = children |> List.collect fst
                let childMenus = children |> List.map snd
                selectedValues, And(config.label, childMenus)
            )

    static member promote (o: 't OptionOffer): 't ListOffer =
        Op.offer(
            o.config,
            fun config input ->
                let (v, menu) = o.recur input
                List.ofOption v, menu
            )
    static member evaluate (state: OfferInput) (offer: _ Offer) =
        offer.recur state

let newKey txt = $"{txt}-{System.Guid.NewGuid()}"
let label txt = { blank with label = Some txt }
open type Op

type Trait' = CombatReflexes | Skill of string * int

(* Requirements:
Terseness: flatten some and's, e.g. "Fast draw (swords & daggers) +1" all on one line, instead of two separate lines.
Terseness: hide irrelevant options in either, e.g. if you can pick swords or daggers and sword is picked, don't show dagger any more.
I.e. collapse either when semi-ready (no more choices at that level).
Terseness: don't show child options until it's possible to pick them, e.g. don't show specific kinds of swords until sword! is picked.
Correctness: checkboxes only inside an either or budget
Terseness: hide things too expensive for remaining budget
UX: leveled traits
Correctness: mutual exclusion within either unselects old selection when new selection is made
*)

let makeSkill name (v: int) = Skill(name, v)
let skill(name:string, level: int) =
    Op.skill(name, makeSkill name, level)
let skillN(name:string, levels: int list) =
    Op.skill(name, makeSkill name, levels)

// swash is not a MenuOutput but it can create MenuOutputs which can then be either unit tested or turned into ReactElements
// think of swash as an offer menu
let swash(): Trait' ListOffer list = [

    skill("Climbing", 1) |> promote
    skillN("Stealth", [1..3]) |> promote
    budgeted(20, [
        trait' CombatReflexes
        skillN("Acrobatics", [1..3])
        ])
    let mainWeapons = ["Rapier"; "Broadsword"; "Polearm"; "Two-handed sword"] |> List.map (fun name -> name, newKey name)
    let weaponsAt (bonus: int) = mainWeapons |> List.map (fun (name, key) -> Op.skill({ blank with key = Some key }, (name, makeSkill name, [bonus])))
    eitherN [
        either(label "Sword!", weaponsAt +5) |> promote
        and'(label "Sword and Dagger", [either(weaponsAt +4); skill("Main-gauche", +1)])
        and'(label "Sword and Shield", [either(weaponsAt +4); skill("Shield", +2)])
        ]
    eitherN [
        skill("Fast-draw (Sword)", +2) |> promote
        and'([skill("Fast-draw (Sword)", +1); skill("Fast-draw (Dagger)", +1)])
        ]
    ]

(*
    Choose one:
        [ ] Sword!
        [ ] Sword and dagger
        [ ] Sword and shield

================================

    [X] Sword!
        [ ] Rapier
        [ ] Broadsword
        [ ] Shortsword

================================

    [X] Sword! [X] Rapier

*)

type Pseudoreact =
    | Checked of string * Pseudoreact list
    | Unchecked of string
    | Unconditional of string * Pseudoreact list
    | NumberInput of string * int
    | Div of string
    | Fragment of Pseudoreact list

let pseudoReactApi = {
    checked' = Checked
    unchecked = Unchecked
    leaf = Div
    leveledLeaf = NumberInput
    unconditional = Unconditional
    combine = Fragment
    }

let parseKey (key: string) : Key =
    key.Split("-") |> List.ofArray |> List.rev
let evalFor (selections: string list) offers =
    let keys = selections |> List.map parseKey |> List.map (fun k -> k, Flag) |> Map.ofList
    evaluate { OfferInput.fresh with selected = keys } offers |> snd
[<Tests>]
let units = testList "Unit.Chargen" [
    let key = parseKey
    testCase "basic either" <| fun () ->
        test <@ either[trait' "Fight"; trait' "Hide"] |> evalFor [] = Either(None, [false, key "Fight", Leaf "Fight"; false, key "Hide", Leaf "Hide"]) @>
        test <@ either[trait' "Fight"; trait' "Hide"] |> evalFor ["Fight"] = Either(None, [true, key "Fight", Leaf "Fight"]) @>
    testCase "nested either with list" <| fun () ->
        let nestedEither = eitherN [
            either(label "Sword!", [skill("Rapier", +5); skill("Broadsword", +5); skill("Shortsword", +5)]) |> promote
            and'(label "Sword and Dagger", [
                either [skill("Rapier", +4); skill("Broadsword", +4); skill("Shortsword", +4)]
                skill("Main-gauche", +1)
                ])
            and'(label "Sword and Shield", [
                either [skill("Rapier", +4); skill("Broadsword", +4); skill("Shortsword", +4)]
                skill("Shield", +2)
                ])
            ]
        test <@ nestedEither |> evalFor [] =
            Either(None, [
                false, key "Sword!", Leaf "Sword!"
                false, key "Sword and Dagger", Leaf "Sword and Dagger"
                false, key "Sword and Shield", Leaf "Sword and Shield"
                ]) @>
        test <@ nestedEither |> evalFor ["Sword!"] =
            Either(None, [
                true, key "Sword!", Either(Some "Sword!", [
                    false, key "Sword!-Rapier", Leaf "Rapier +5" // note how Leveled is only Leveled if selected. When unselected it's a Leaf just like anything else.
                    false, key "Sword!-Broadsword", Leaf "Broadsword +5"
                    false, key "Sword!-Shortsword", Leaf "Shortsword +5"
                    ])
                ]) @>
        test <@ nestedEither |> evalFor ["Sword!"; "Sword!-Rapier"] =
            Either(None, [
                true, key "Sword!", Either(Some "Sword!", [
                    true, key "Sword!-Rapier", Leveled("Rapier +5", 0) // it's a Levelled, not a Leaf, because it's currently selected. Note that the level is 0, not +5, because it's the lowest level out of +5 to +5.
                    ])
                ]) @>
        test <@ nestedEither |> evalFor ["Sword and Dagger"] =
            Either(None, [
                true, key "Sword and Dagger", And(None, [
                        Either(None, [
                            false, key "Sword and Dagger-Rapier", Leaf "Rapier +4"
                            false, key "Sword and Dagger-Broadsword", Leaf "Broadsword +4"
                            false, key "Sword and Dagger-Shortsword", Leaf "Shortsword +4"
                            ])
                        Leveled("Main-gauche", +1)
                        ])
                    ]) @>
    ]
let proto1 = testCase "proto1" <| fun () ->
    let key = parseKey
    let actual = swash() |> List.map (evaluate OfferInput.fresh >> snd) // shouldn't actually use OfferInput.fresh here. Need to pick the options we want to show up in pseudoActual.s
    let pseudoActual = // pseudo-actual because actual will be created from templates + OfferInput (i.e. selected keys), not hardwired as Menus, but that's still TODO
        let menus = [
            Leveled("Climbing", 1)
            Leveled("Stealth", 3)
            Either(None, [
                true, key "Sword!", Either(Some "Sword!", [
                    false, key "Rapier", Leveled("Rapier", +5)
                    false, key "Broadsword", Leveled("Broadsword", +5)
                    false, key "Shortsword", Leveled("Shortsword", +5)
                    ])
                ])
            Either(None, [true, key "Fast-Draw (Sword)", Leveled("Fast-draw (Sword)", +2)])
            ]
        test <@ menus = actual @>
        render pseudoReactApi menus
    let fail expect v = failwith $"Expected {expect} but got {v}\nContext: {pseudoActual}"
    let (|Checked|) = function Checked(label, children) -> Checked(label, children) | v -> fail "Checked" v
    let (|Unchecked|) = function Unchecked(label) -> Unchecked(label) | v -> fail "Unchecked" v
    let (|Unconditional|) = function Unconditional(label, children) -> Unconditional(label, children) | v -> fail "Unconditional" v
    let (|NumberInput|) = function NumberInput(label, value) -> NumberInput(label, value) | v -> fail "NumberInput" v
    let (|Div|) = function Div(label) -> Div(label) | v -> fail "Div" v
    let (|Fragment|) = function Fragment(children) -> Fragment(children) | v -> fail "Fragment" v
    let (|Expect|) expect actual = if expect = actual then true else failwith $"Expected {expect} but got {actual}"
    match pseudoActual with
    | Fragment([
        NumberInput(Expect "Climbing" _, Expect 1 _)
        NumberInput(Expect "Stealth" _, Expect 3 _)
        Checked(Expect "Sword!" _, [
            NumberInput(Expect "Rapier" _, Expect +5 _)
            NumberInput(Expect "Broadsword" _, Expect +5 _)
            NumberInput(Expect "Shortsword" _, Expect +5 _)
            ])
        NumberInput(Expect "Fast-draw (Sword)" _, Expect +2 _)
        ]) -> ()
    | v -> matchfail v // maybe we got the wrong number of NumberInputs from the Unconditional or something. Would be nice to have the error message say exactly what went wrong,
                    // but Expect active pattern isn't valid as an input to Fragment/Unconditional/etc. so we can't just Expect a specific list of children. Although... maybe we can refactor
                    // to use functions instead of active patterns?


[<Tests>]
let tests =
    testList "Accept.Chargen" [
        proto1
        ptestCase  "Terseness #1" <| fun () -> failtest """flatten some and's, e.g. "Fast draw (swords & daggers) +1" all on one line, instead of two separate lines."""
        ptestCase  "Terseness #2" <| fun () -> failtest """hide irrelevant options in either, e.g. if you can pick swords or daggers and sword is picked, don't show dagger any more. I.e. collapse either when semi-ready (no more choices at that level)."""
        ptestCase  "Terseness #3" <| fun () -> failtest """don't show child options until it's possible to pick them, e.g. don't show specific kinds of swords until sword! is picked."""
        ptestCase "Correctness #1" <| fun () -> failtest """checkboxes only inside an either or budget"""
        ptestCase  "Terseness #4" <| fun () -> failtest """hide things too expensive for remaining budget"""
        ptestCase  "UX #1" <| fun () -> failtest """leveled traits"""
        ptestCase "Correctness #2" <| fun () -> failtest """mutual exclusion within either unselects old selection when new selection is made"""
    ]
