#I __SOURCE_DIRECTORY__
#load @"Common.fs"

type 't EnumW = {| elements: 't array |}
// Following the Elm is Wrong methodology here by defining Ord based on fromInt/toInt.
//     Ref: https://reasonablypolymorphic.com/blog/elm-is-wrong/
// Yes, in real life you wouldn't define this type class with these members but that's not the point right now.
//     We just want to show that you can write functions against these type-classes and automatically derive
//     witnesses from other type class witnesses as needed.
type 't OrdW = {|
    fromInt: int -> 't
    toInt: 't -> int
    |}

let inline (|EnumT|) (x: ^tc) =
    {|
        elements = (^tc : (member elements: ^t array) (x))
    |}
let inline (|OrdT|) (x: ^tc) =
    {|
        fromInt = (^tc : (member fromInt: (int -> ^t)) (x))
        toInt = (^tc : (member toInt: (^t -> int)) (x))
    |}

type Color =
    | Red
    | Yellow
    | Green
    | Brown
    | Scarlet
    | Black
    | Ochre
    | Peach
    | Ruby
    | Olive
    | Violet
    | Fawn
    | Azure
    | Lemon
    | Russett
    | Grey
    | Purple
    | White
    | Pink
    | Orange
    | Blue
    with
    static member enumW: Color EnumW = {|
        elements = [|
            Red; Yellow; Green; Brown; Scarlet; Black; Ochre; Peach; Ruby; Olive; Violet; Fawn;
            Azure; Lemon; Russett; Grey; Purple; White; Pink; Orange; Blue
        |]
    |}

let inline sort (OrdT ord as tc) (items: 't array) =
    items |> Array.map ord.toInt |> Array.sort |> Array.map ord.fromInt

let inline derivingOrd (witness: 't EnumW) = {|
        witness with
            fromInt = fun ix -> witness.elements.[ix]
            toInt = fun v -> witness.elements |> Array.findIndex ((=) v)
    |}

[|Grey; Red; White; Blue; Pink|] |> sort (derivingOrd Color.enumW) // we can sort based on a derived typeclass!
[|8.0;3.3; 3.14; -9.|] |> sort ({| toInt = (fun (f: float) -> int f); fromInt = (fun (i:int) -> float i) |})
// We can supply an explicit witness for floats
type Mood = Happy | Sad | Angry
[|Angry; Sad; Sad; Happy|] |> sort (derivingOrd {| elements = [| Happy; Sad; Angry |] |}) // proof that it's really generic for all Enums

