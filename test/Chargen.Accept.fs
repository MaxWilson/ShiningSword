module Tests

open Expecto
open Swensen.Unquote

// okay, for each leaf/level/composition, we need to know:
// 1. If it's checkable, what is the current state?
// 2. If it's checkable, how do we change the state?
// 3. What is the label, if any? If there's no explicit label, how do we display it? (Might depend on whether or not it's checked or finished.)
//    3a. Do we ever NOT have a label? I.e. are there any headless UI components?

type MenuOutput =
    | Either of label: string option * options: (bool * MenuOutput) list
    | And of label: string option * grants: MenuOutput list
    | Leveled of label: string * level: int
    | Leaf of label: string

type 't Output = 't * MenuOutput
type 't ListOutput = ('t list) Output
type 't OptionOutput = ('t option) Output

type Key = string
type OfferInput = {
    selected: Set<Key>
    }
type 't Offer = Offer of (OfferInput -> 't)
type 't ListOffer = ('t list) Offer
type 't OptionOffer = ('t option) Offer

type OfferContext = {
    key: Key option
    label: string option
    }
    with static member blank = { key = None; label = None }
open type OfferContext

type 'reactElement RenderApi = {
    checked': string * ('reactElement list) -> 'reactElement
    unchecked: string -> 'reactElement
    unconditional: string * ('reactElement list) -> 'reactElement
    leaf: string -> 'reactElement
    leveledLeaf: string * int -> 'reactElement
    combine: 'reactElement list -> 'reactElement
    }

let render (render: 'reactElement RenderApi, menus: MenuOutput list) =
    let rec recur recurOnChildren (renderMe: (string * 'reactElement list) -> 'reactElement) menu : 'reactElement =
        match menu with
        | Either(label, selections) ->
            let childReacts = [
                if recurOnChildren then
                    for (isChecked, child) in selections do
                        let renderChild (label: string, children) =
                            if isChecked then render.checked'(label, children) else render.unchecked label
                        let childReact = recur isChecked renderChild child
                        childReact
                ]
            renderMe(defaultArg label "Choose one:", childReacts)
        | And(label, grants) ->
            let childReacts = grants |> List.map (recur true render.unconditional)
            renderMe(defaultArg label "And:", childReacts)
        | Leveled(name, lvl) -> render.leveledLeaf(name, lvl)
        | Leaf(name) -> render.leaf name
    menus |> List.map (recur true render.unconditional) |> render.combine

type Op =
    static member skill v: 't OptionOffer = notImpl()
    static member trait' v: 't OptionOffer = notImpl()
    static member budgeted v: 't ListOffer = notImpl()
    static member either v : 't OptionOffer = notImpl()
    static member and' v : 't OptionOffer = notImpl()
    static member eitherN v : 't ListOffer = notImpl()
    static member andN' v : 't ListOffer = notImpl()
    static member promote (o: 't OptionOffer): 't ListOffer = notImpl()
let newKey txt = $"{txt}-{System.Guid.NewGuid()}"
let label txt = { blank with label = Some txt }
open type Op

type Trait' = CombatReflexes

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

// swash is not a MenuOutput but it can create MenuOutputs which can then be either unit tested or turned into ReactElements
// think of swash as an offer menu
let swash: Trait' ListOffer list = [
    skill("Climbing", 1) |> promote
    skill("Stealth", [1..3]) |> promote
    budgeted(20, [
        trait' CombatReflexes
        skill("Acrobatics", [1..3])
        ])
    let mainWeapons = ["Rapier"; "Broadsword"; "Polearm"; "Two-handed sword"] |> List.map (fun name -> name, newKey name)
    let weaponsAt (bonus: int) = mainWeapons |> List.map (fun (name, key) -> skill({ blank with key = Some key }, name, bonus))
    eitherN [
        either(label "Sword!", weaponsAt +5) |> promote
        andN'(label "Sword and Dagger", [either(weaponsAt +4); skill("Main-gauche", +1)])
        andN'(label "Sword and Shield", [either(weaponsAt +4); skill("Shield", +2)])
        ]
    eitherN [
        skill("Fast-draw (Sword)", +2) |> promote
        andN'([skill("Fast-draw (Sword)", +1); skill("Fast-draw (Dagger)", +1)])
        ]
    ]

(*
    choose one:
        [ ] sword!
        [ ] sword and dagger
        [ ] sword and shield

================================

    choose one:
        [X] sword!
            [ ] Rapier
            [ ] Broadsword
            [ ] Shortsword

================================

    choose one:
        [X] sword!
            [X] Rapier

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

let proto1 = testCase "Terseness #1" <| fun () ->

    ()

[<Tests>]
let tests =
    testList "Acceptance" [
        proto1
        testCase  "Terseness #1" <| fun () -> failtest """flatten some and's, e.g. "Fast draw (swords & daggers) +1" all on one line, instead of two separate lines."""
        testCase  "Terseness #2" <| fun () -> failtest """hide irrelevant options in either, e.g. if you can pick swords or daggers and sword is picked, don't show dagger any more. I.e. collapse either when semi-ready (no more choices at that level)."""
        testCase  "Terseness #3" <| fun () -> failtest """don't show child options until it's possible to pick them, e.g. don't show specific kinds of swords until sword! is picked."""
        testCase "Correctness #1" <| fun () -> failtest """checkboxes only inside an either or budget"""
        testCase  "Terseness #4" <| fun () -> failtest """hide things too expensive for remaining budget"""
        testCase  "UX #1" <| fun () -> failtest """leveled traits"""
        testCase "Correctness #2" <| fun () -> failtest """mutual exclusion within either unselects old selection when new selection is made"""
        testCase "placeholder" <| fun _ ->
            test <@ 1 + 1 = 2 @>
    ]
