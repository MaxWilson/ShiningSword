module Tests

open Expecto
open Swensen.Unquote

type MenuOutput =
  | Either of MenuOutput list
  | And of MenuOutput list
  | Leveled of string * int
  | Leaf of string

// swash is not a MenuOutput but it can create MenuOutputs which can then be either unit tested or turned into ReactElements
// think of swash as an offer menu
// let swash = [
//     skill("Climbing", 1)
//     skill("Stealth", [1..3])
//     budgeted(20, [
//         trait' CombatReflexes
//         skill("Acrobatics", [1..3])
//         ])
//     let mainWeapons = ["Rapier"; "Broadsword"; "Polearm"; "Two-handed sword"] |> List.map (fun name -> name, newKey name)
//     let weaponsAt (bonus: int) = mainWeapons |> List.map (fun (name, key) -> skill({ blank with key = Some key }, name, bonus))
//     either [
//         either(label "Sword!", weaponsAt +5)
//         and'(label "Sword and Dagger", [either(weaponsAt +4); skill("Main-gauche", +1)])
//         and'(label "Sword and Shield", [either(weaponsAt +4); skill("Shield", +2)])
//         ]
//     either [
//         skill("Fast-draw (Sword)", +2)
//         and'([skill("Fast-draw (Sword)", +1); skill("Fast-draw (Dagger)", +1)])
//         ]
//     ]



[<Tests>]
let tests =
  testList "Acceptance" [
    testCase "placeholder" <| fun _ ->
      test <@ 1 + 1 = 2 @>

  ]
