module Chargen.Accept

open Menus
open Expecto
open Swensen.Unquote

// okay, for each leaf/level/composition, we need to know:
// 1. If it's checkable, what is the current state?
// 2. If it's checkable, how do we change the state?
// 3. What is the label, if any? If there's no explicit label, how do we display it? (Might depend on whether or not it's checked or finished.)
//    3a. Do we ever NOT have a label? I.e. are there any headless UI components? I think maybe we want to collapse fulfilled eithers.

open type Op

[<StructuredFormatDisplay("{DisplayText}")>]
type Trait' = CombatReflexes | Skill of string * int
    with
    member this.DisplayText =
        match this with
        | CombatReflexes -> "Combat Reflexes"
        | Skill(name, level) -> $"{name} %+d{level}"

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
    Op.trait'(makeSkill name level)
let skillN(name:string, levels: int list) =
    Op.level(name, makeSkill name, levels)

// swash is not a MenuOutput but it can create MenuOutputs which can then be either unit tested or turned into ReactElements
// think of swash as an offer menu
let swash(): Trait' ListOffer list = [
    let budgetStub n = fun _ -> n // currently budgetF is hardwired to always think there's another n in the budget. TODO: make it aware of the current selections somehow
    skill("Climbing", 1) |> promote
    skillN("Stealth", [1..3]) |> promote
    budget(budgetStub 20, [
        trait' CombatReflexes
        skillN("Acrobatics", [1..3])
        ])
    let weaponsAt (bonus: int) = [for name in ["Rapier"; "Broadsword"; "Shortsword"] -> Op.level(name, makeSkill name, [bonus..bonus+3])] // make sure Op.level gets exercised
    eitherN [
        either(label "Sword!", weaponsAt +5) |> promote
        and'(label "Sword and Dagger", [either(weaponsAt +4); skill("Main-gauche", +1)])
        and'(label "Sword and Shield", [either(weaponsAt +4); skill("Shield", +2)])
        ]
    eitherN [
        skill("Fast-Draw (Sword)", +2) |> promote
        and'([skill("Fast-Draw (Sword)", +1); skill("Fast-Draw (Dagger)", +1)])
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
    // /- is escaped form, treated as meaning -. E.g. Fast/-Draw becomes Fast-Draw.
    System.Text.RegularExpressions.Regex.Split(key, "(?<!/)-") |> List.ofArray |> List.map (fun s -> s.Replace("/-", "-")) |> List.rev
let evalFor (selections: string list) offers =
    let keys = selections |> List.map parseKey |> List.map (fun k -> k, Flag) |> Map.ofList
    evaluate { OfferInput.fresh with selected = keys } offers |> snd

let testFor (selections: string list) expected offers =
    let actual = evalFor selections offers
    if actual <> expected then
        let actualS, expectedS = actual |> String.structured, expected |> String.structured
        let firstDiff = [0..actualS.Length-1]
        let same, actual, expected = String.diff actualS expectedS
        failtest $"Actual diverged from expected! After: \n{same}\n\nExpected: \n{expected}\n\nbut got:\n{actual}"

let testFors (selections: string list) expected offers =
    let actual = offers |> List.map (evalFor selections)
    if actual <> expected then
        let actualS, expectedS = actual |> String.structured, expected |> String.structured
        let same, actual, expected = String.diff actualS expectedS
        failtest $"Actual diverged from expected! After: \n{same}\n\nExpected: \n{expected}\n\nbut got:\n{actual}"

type FightHide = Fight | Hide

[<Tests>]
let units = testList "Unit.Chargen" [
    let key = parseKey
    testCase "basic either" <| fun () ->
        either[trait' Fight; trait' Hide] |> testFor [] (Either(None, [false, key "Fight", Leaf "Fight"; false, key "Hide", Leaf "Hide"]))
        either[trait' Fight; trait' Hide] |> testFor ["Fight"] (Either(None, [true, key "Fight", Leaf "Fight"]))
    testCase "nested either with list" <| fun () ->
        let nestedEither = eitherN [
            either(label "Sword!", [skillN("Rapier", [+5..+6]); skillN("Broadsword", [+5..+6]); skillN("Shortsword", [+5..+6])]) |> promote // make sure to exercise Op.level even though the actual DFRPG swashbuckler doesn't have a +6 option
            and'(label "Sword and Dagger", [
                either [skill("Rapier", +4); skill("Broadsword", +4); skill("Shortsword", +4)]
                skill("Main-gauche", +1)
                ])
            and'(label "Sword and Shield", [
                either [skill("Rapier", +4); skill("Broadsword", +4); skill("Shortsword", +4)]
                skill("Shield", +2)
                ])
            ]
        nestedEither |> testFor [] (
            Either(None, [
                false, key "Sword!", Leaf "Sword!"
                false, key "Sword and Dagger", Leaf "Sword and Dagger"
                false, key "Sword and Shield", Leaf "Sword and Shield"
                ])
            )
        nestedEither |> testFor ["Sword!"] (
            Either(None, [
                true, key "Sword!", Either(Some "Sword!", [
                    false, key "Sword!-Rapier", Leaf "Rapier +5" // note how Leveled is only Leveled if selected. When unselected it's a Leaf just like anything else.
                    false, key "Sword!-Broadsword", Leaf "Broadsword +5"
                    false, key "Sword!-Shortsword", Leaf "Shortsword +5"
                    ])
                ])
            )
        nestedEither |> testFor ["Sword!"; "Sword!-Rapier"] (
            Either(None, [
                true, key "Sword!", Either(Some "Sword!", [
                    true, key "Sword!-Rapier", Leveled("Rapier +5", 0) // it's a Levelled, not a Leaf, because it's currently selected. Note that the level is 0, not +5, because it's the lowest level out of +5 to +5.
                    ])
                ])
            )
        nestedEither |> testFor ["Sword and Dagger"] (
            Either(None, [
                true, key "Sword and Dagger", And(Some "Sword and Dagger", [
                        Either(None, [
                            false, key "Sword and Dagger-Rapier +4", Leaf "Rapier +4" // notice how, since we're using trait' and not leveled, the +4 shows up in the key
                            false, key "Sword and Dagger-Broadsword +4", Leaf "Broadsword +4"
                            false, key "Sword and Dagger-Shortsword +4", Leaf "Shortsword +4"
                            ])
                        Leaf "Main-gauche +1" // notice: Leaf, not Leveled, not because it's not selected but because it's defined via Op.trait' not Op.level
                        ])
                    ])
            )
    ]

let proto1 = testCase "proto1" <| fun () ->
    let key = parseKey
    let pseudoActual = // pseudo-actual because actual will be created from templates + OfferInput (i.e. selected keys), not hardwired as Menus, but that's still TODO
        let offers = swash()
        let expectedMenus = [
            Leaf "Climbing +1" // Leaf not Level because swash() template is only using trait', not level
            Leveled("Stealth +1", 0) // Leveled because it can go up to +3
            Either(None, [
                false, key "Combat Reflexes", Leaf "Combat Reflexes"
                false, key "Acrobatics", Leaf "Acrobatics +1"
                ])
            Either(None, [
                true, key "Sword!", Either(Some "Sword!", [
                    false, key "Sword!-Rapier", Leaf "Rapier +5" // leveled traits are only Leveled if selected. When unselected it's a Leaf just like anything else.
                    false, key "Sword!-Broadsword", Leaf "Broadsword +5"
                    false, key "Sword!-Shortsword", Leaf "Shortsword +5"
                    ])
                ])
            Either(None, [true, ["Fast-Draw (Sword) +2"], Leaf "Fast-Draw (Sword) +2"])
            ]
        offers |> testFors ["Sword!"; "Fast/-Draw (Sword) +2"] expectedMenus // evaluate swash() with Sword! selected and compare it to expectedMenus. Escape Fast-Draw to prevent it from being interpreted by parseKey as Fast + Draw
        render pseudoReactApi expectedMenus // if that passes, render it to ReactElements and see if it looks right
    let fail expect v = failwith $"Expected {expect} but got {v}\nContext: {pseudoActual}"
    let (|Checked|) = function Checked(label, children) -> Checked(label, children) | v -> fail "Checked" v
    let (|Unchecked|) = function Unchecked(label) -> Unchecked(label) | v -> fail "Unchecked" v
    let (|Unconditional|) = function Unconditional(label, children) -> Unconditional(label, children) | v -> fail "Unconditional" v
    let (|NumberInput|) = function NumberInput(label, value) -> NumberInput(label, value) | v -> fail "NumberInput" v
    let (|Div|) = function Div(label) -> Div(label) | v -> fail "Div" v
    let (|Fragment|) = function Fragment(children) -> Fragment(children) | v -> fail "Fragment" v
    let (|Expect|_|) expect actual = if expect = actual then Some () else failwith $"Expected {expect} but got {actual}"
    match pseudoActual with
    | Fragment([
        Unconditional(Expect "Climbing +1", [])
        NumberInput(Expect "Stealth +1", Expect 0)
        Unconditional(Expect "Choose one:", [
            Unchecked(Expect "Combat Reflexes")
            Unchecked(Expect "Acrobatics +1")
            ])
        Checked(Expect "Sword!", [
            Unchecked(Expect "Rapier +5")
            Unchecked(Expect "Broadsword +5")
            Unchecked(Expect "Shortsword +5")
            ])
        Checked(Expect "Fast-Draw (Sword) +2", Expect [])
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
