let toList f v = [f v]
type Depends = Depends of typeName: string * value:obj
type Any = obj
let viaAny<'t>() = box<'t>, unbox<'t>

type Polymorphic<'arg, 'result> =
    abstract member Apply: 'arg -> 'result

let list f = { new Polymorphic<'a, 'a list> with
                member _.Apply v = toList f v }
let flip f x y = f y x
let inline apply (f: Polymorphic<'a, 'b>) (v: 'a) = f.Apply v

list ((*)2) |> flip apply 3
list (fun x -> x + x) |> flip apply "abc"

type Constructor<'args, 'Type> = {
    create: 'args -> 'Type
    extract: 'Type -> 'args option
    name: string option
    }
    with
    static member (=>) (lhs: Constructor<_, 't1>, rhs: Constructor<'t1, _>) =
        {   create = rhs.create << lhs.create
            extract = rhs.extract >> Option.bind lhs.extract
            name =  match lhs.name, rhs.name with
                    | Some lhs, Some rhs -> Some ($"{rhs} ({lhs})")
                    | Some lhs, _ -> Some lhs
                    | _, Some rhs -> Some rhs
                    | _ -> None
            }
let ctor(create, extract) = { create = create; extract = extract; name = None }
let namedCtor(name, create, extract) = { create = create; extract = extract; name = Some name }

(*
budget 60
    More HP 1-3
    Luck 2-3
*)
type 'trait1 ChooseLevels =
    abstract generate: Polymorphic<Constructor<Any, 'trait1> * Any list, Any> -> Any

type ChooseLevels<'arg, 'trait1>(ctor: Constructor<'arg, 'trait1>, values: 'arg list) =
    let packArg, unpackArg = viaAny<'arg>()
    let anyCtor = { name = ctor.name; create = (unpackArg >> ctor.create); extract = ctor.extract >> Option.map packArg }
    let anyValues = values |> List.map packArg
    interface 'trait1 ChooseLevels with
        member this.generate (f: Polymorphic<Constructor<Any, 'trait1> * Any list, Any>) =
            f.Apply(anyCtor, anyValues)

type 't Many =
    | Aggregate of 't OneResult list
    | Budget of int * 't OneResult list
    | ChooseOneAggregate of 't Many list
and 't OneResult =
    | Choose of 't ChooseLevels
    | Binary of 't
let choose a b = Choose(ChooseLevels<_,_>(a,b))
type Luck = Regular | Extraordinary | Ridiculous
type Trait =
    | HP of int
    | Luck of Luck
    | GreatVoid

let HP = namedCtor(nameof(HP), HP, function | HP x -> Some x | _ -> None)
let Luck = namedCtor(nameof(Luck), Luck, function | Luck x -> Some x | _ -> None)

let swash() = Budget (60, [
    Binary GreatVoid
    choose HP [1;2;3]
    choose Luck [Extraordinary; Ridiculous]
    ])

module GetAll =
    let rec manyFromOne = function
        | Binary (item: 'trait1) -> [item]
        | Choose choice ->
            let pack, unpack = viaAny<_ list>()
            let f = {   new Polymorphic<Constructor<Any, 'trait1> * Any list, Any>
                            with
                            member _.Apply ((ctor, args)) = args |> List.map ctor.create |> pack }
            choice.generate f |> unpack
    let rec manyFromMany = function
        | Budget (n, items: 'trait1 OneResult list) -> items |> List.collect manyFromOne
        | Aggregate items -> items |> List.collect manyFromOne
        | ChooseOneAggregate itemss -> itemss |> List.collect manyFromMany
module GetCost =
    let rec costOfOne costOf = function
        | Binary (item: 'trait1) -> costOf item
        | Choose choice ->
            let pack, unpack = viaAny<int>()
            let f = {   new Polymorphic<Constructor<Any, 'trait1> * Any list, Any>
                            with
                            member _.Apply ((ctor, args)) = args |> List.map ctor.create |> List.sumBy costOf |> pack }
            choice.generate f |> unpack
    let rec costOfMany (costOf: 'trait1 -> int) = function
        | Budget (n, items) -> items |> List.sumBy (costOfOne costOf)
        | Aggregate items -> items |> List.sumBy (costOfOne costOf)
        | ChooseOneAggregate itemss -> itemss |> List.sumBy (costOfMany costOf)
module GetFirst =
    let rec oneFromOne = function
        | Binary (item: 'trait1) -> item
        | Choose choice ->
            let pack, unpack = viaAny<_ list>()
            let f = {   new Polymorphic<Constructor<Any, 'trait1> * Any list, Any>
                            with
                            member _.Apply ((ctor, args)) = args |> List.map ctor.create |> pack }
            choice.generate f |> unpack |> List.head
    let rec oneFromMany = function
        | Budget (n, items) -> items |> List.map oneFromOne |> List.head
        | Aggregate items -> items |> List.map oneFromOne |> List.head
        | ChooseOneAggregate itemss -> itemss |> List.map oneFromMany |> List.head
GetAll.manyFromMany (swash())
GetFirst.oneFromMany (swash())
GetCost.costOfMany (function HP n -> n * 2 | Luck _ -> 30 | GreatVoid -> 10) (swash())
