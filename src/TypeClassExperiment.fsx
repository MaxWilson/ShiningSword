#I __SOURCE_DIRECTORY__
#load @"Common.fs"

type 'a EnumT =
    abstract elements: 'a array
type 'a OrdT =
    abstract fromInt: int -> 'a
    abstract toInt: 'a -> int

let sort<'t, 'tc when 'tc :> 't OrdT and 'tc :> 't EnumT> (tc:'tc) (items: 't array) =
    items |> Array.map tc.toInt |> Array.sort |> Array.map tc.fromInt

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
    static member enumW = {
        new (Color EnumT) with
            member this.elements = [|
                Red; Yellow; Green; Brown; Scarlet; Black; Ochre; Peach; Ruby; Olive; Violet; Fawn;
                Azure; Lemon; Russett; Grey; Purple; White; Pink; Orange; Blue
            |] }

type 't OrdEnum =
    inherit ('t EnumT)
    inherit ('t OrdT)

let derivingOrd (w: #('t EnumT)) = {
    new ('t OrdEnum) with
        member this.fromInt ix = w.elements.[ix]
        member this.toInt v = w.elements |> Array.findIndex ((=) v)
    interface ('t EnumT) with
        member this.elements = w.elements
    }

[|Grey; Red; White; Blue; Pink|] |> sort (derivingOrd Color.enumW)
