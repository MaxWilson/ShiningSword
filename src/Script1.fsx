#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#load @"Common.fs"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Abstractions\Log.fs"
#load @"Abstractions\Dice.fs"
#load @"Abstractions\DataEngine.fs"
#load @"Model\Types.fs"
#load @"Model\Functions.fs"
#load @"Model\Names.fs"
#load @"Model\Tables.fs"
#load @"Model\Operations.fs"
#load @"Model\Chargen.fs"
#load @"Model\Battle.fs"
#load @"Model\MonsterManual.fs"
#load @"Model\Gameplay.fs"
#load @"Engine\DataEngine.fs"


let inline konst x _ = x

type CFunctor() =
    static member inline fmap (f: ^a -> ^b, a: ^a list) = List.map f a
    static member inline fmap (f: ^a -> ^b, a: ^a option) =
        match a with
        | None -> None
        | Some x -> Some (f x)

    // default implementation of replace
    static member inline replace< ^a, ^b, ^c, ^d, ^e when ^a :> CFunctor and (^a or ^d): (static member fmap: (^b -> ^c) * ^d -> ^e) > (a, f) =
        ((^a or ^d) : (static member fmap : (^b -> ^c) * ^d -> ^e) (konst a, f))

    // call overridden replace if present
    static member inline replace< ^a, ^b, ^c when ^b: (static member replace: ^a * ^b -> ^c)>(a: ^a, f: ^b) =
        (^b : (static member replace: ^a * ^b -> ^c) (a, f))

let inline replace_instance< ^a, ^b, ^c, ^d when (^a or ^c): (static member replace: ^b * ^c -> ^d)> (a: ^b, f: ^c) =
        ((^a or ^c): (static member replace: ^b * ^c -> ^d) (a, f))

// Note the concrete type 'CFunctor' specified in the signature
let inline replace (a: ^a) (f: ^b): ^a0 when (CFunctor or  ^b): (static member replace: ^a *  ^b ->  ^a0) =
    replace_instance<CFunctor, _, _, _> (a, f)

let inline bar< ^a, ^b, ^c, ^d when ^a: (member foo: ^b * ^c -> ^d)> (a:^a, b:^b, c) =
    (^a: (member foo: ^b * ^c -> ^d) (a,b,c))

type A() =
    member this.foo(x, y) = x + y
bar(A(), 2, 3)

foo (A())
module Functor =
    let inline (|HasFoo|) x =
        fun a1 a2 -> (^a: (member foo: ^b * ^c -> 'd) (x, a1, a2))
    let inline foo (HasFoo f) = f
    let inline bar< ^a, ^b, ^c, ^d when ^a: (member foo: ^b * ^c -> ^d)> (a:^a, b:^b, c) =
        (^a: (member foo: ^b * ^c -> ^d) (a,b,c))
    let inline (|HasAdd|) x =
        fun arg -> (^a : (static member add: ^a * ^b -> ^c) (x,arg))
    let inline add row (HasAdd f: 't) : 't =
        f row
    let inline (|HasTransform|) x =
        fun (id, t) -> (^a : (static member transform: ^a * ^b * ^c -> ^d) (x,id,t))
    let inline transform1 id t ((HasTransform f):'t) : 't =
        f(id, t)
    let inline transform2 (HasTransform f) = f
    let inline (|HasReplace|) x =
        fun arg -> (^a : (member replace: 'b -> 'c) (x,arg))
    let inline replace (id, row) (HasReplace f: 't) : 't =
        f(id, row)
    let inline (|HasToSeq|) x =
        fun () -> (^a : (member toSeq: unit -> 'b) x)
    let inline toSeq (HasToSeq f) : 't seq =
        f()
open Functor

type FastList<'t> = { rows: Map<int, 't>; lastId: int option }
    with
    static member add (row: 't) (data:FastList<'t>)=
        let id = (defaultArg data.lastId 0) + 1
        { data with rows = data.rows |> Map.add id row; lastId = Some id }
    static member transform(data:FastList<'t>, id, f) =
        let row = data.rows.[id]
        { data with rows = data.rows |> Map.add id (f row) }
    member data.replace(id, row) =
        { data with rows = data.rows |> Map.add id row }
    member data.toSeq() =
        seq { for i in 1..(defaultArg data.lastId 0) -> data.rows.[i] }
    static member fresh(): FastList<'t> = { rows = Map.empty; lastId = None }

let t = FastList<int>.fresh()
let y = toSeq t
let z = Functor.transform1 (0, id, t)

transform2 t
