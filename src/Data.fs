module Data
open Common

module Functor =
    // NOTE which type arguments have to be statically bound with ^ and which can be bound at JIT-time with '
    //    If you make something ' which should be ^ it won't compile
    let inline (|HasAdd|) x =
        fun args -> (^a : (static member add: ^a * 'b -> ^a) (x,args))
    let inline add row (HasAdd f: 't) : 't =
        f row
    let inline (|HasTransform|) x =
        fun args -> (^a : (static member transform: ^a * 'b -> ^a) (x,args))
    let inline transform id t ((HasTransform f):'t) : 't =
        f(id, t)
    let inline (|HasReplace|) x =
        fun args -> (^a : (static member replace: ^a * 'b -> ^a) (x,args))
    let inline replace (id, row) (HasReplace f: 't) : 't =
        f(id, row)
    let inline (|HasToSeq|) x =
        fun args -> (^a : (static member toSeq: ^a * 'b -> 'c) (x,args))
    let inline toSeq (HasToSeq f) : 't seq =
        f()
    let inline (|HasTryFind|) x =
        fun args -> (^a : (static member tryFind: ^a * 'b -> 'c option) (x,args))
    let inline tryFind id (HasTryFind f) =
        f id
    let inline (|HasFind|) x =
        fun args -> (^a : (static member find: ^a * 'b -> 'c) (x,args))
    let inline find id (HasFind f) =
        f id
    let inline (|HasTryFindValue|) x =
        fun args -> (^a : (static member tryFindValue: ^a * 'b -> 'c option) (x,args))
    let inline tryFindValue v (HasTryFindValue f) =
        f v
    let inline (|HasFindValue|) x =
        fun args -> (^a : (static member findValue: ^a * 'b -> 'c) (x,args))
    let inline findValue v (HasFindValue f) =
        f v

type FastList<'t> = { rows: Map<int, 't>; lastId: int option }
    with
    static member add (fastList: FastList<'t>, row: 't)=
        let id = (defaultArg fastList.lastId 0) + 1
        { fastList with rows = fastList.rows |> Map.add id row; lastId = Some id }
    static member transform(fastList: FastList<'t>, (id, f)) =
        let row = fastList.rows.[id]
        { fastList with rows = fastList.rows |> Map.add id (f row) }
    static member replace(fastList: FastList<'t>, (id, row)) =
        { fastList with rows = fastList.rows |> Map.add id row }
    static member toSeq(fastList: FastList<'t>) =
        seq { for i in 1..(defaultArg fastList.lastId 0) -> fastList.rows.[i] }
    static member fresh(): FastList<'t> = { rows = Map.empty; lastId = None }
    static member tryFind(fastList: FastList<'t>, id) =
        fastList.rows |> Map.tryFind id
    static member find(fastList: FastList<'t>, id) =
        fastList.rows |> Map.find id

type SymmetricMap<'key, 'v when 'key: comparison and 'v: comparison> = {
    data: Map<'key, 'v> * Map<'v, 'key>
    }
    with
    static member empty(): SymmetricMap<'key, 'v> = { data = Map.empty, Map.empty }
    static member find(d: SymmetricMap<'key, 'v>, k) = (fst d.data) |> Map.find k
    static member tryFind(d: SymmetricMap<'key, 'v>, k) = (fst d.data) |> Map.tryFind k
    static member findValue(d: SymmetricMap<'key, 'v>, v) = (snd d.data) |> Map.find v
    static member tryFindValue(d: SymmetricMap<'key, 'v>, v) = (snd d.data) |> Map.tryFind v
    static member add({data=(m1, m2)}: SymmetricMap<'key, 'v>, (k,v)) = { data = (m1 |> Map.add k v), (m2 |> Map.add v k) }
    static member toSeq(d: SymmetricMap<'key, 'v>) = (fst d.data) |> Map.toSeq
    static member isEmpty(d: SymmetricMap<'key, 'v>) = (fst d.data) |> Map.isEmpty
    static member keys(d: SymmetricMap<'key, 'v>) = (fst d.data) |> Map.keys
    static member values(d: SymmetricMap<'key, 'v>) = (snd d.data) |> Map.keys

module SymmetricRelation =
    type Data<'t1, 't2 when 't1: comparison and 't2: comparison> = {
        forward: Map<'t1, 't2 list>
        backward: Map<'t2, 't1 list>
    }
    let empty = { forward = Map.empty; backward = Map.empty }
    let add v1 v2 (d:Data<_,_>) =
        let createOrExtend key v map =
            match map |> Map.tryFind key with
            | Some lst -> map |> Map.add key (v::lst)
            | None -> Map.add key [v] map
        {
            forward = createOrExtend v1 v2 d.forward
            backward = createOrExtend v2 v1 d.backward
        }
    let addMany v1 v2s (d:Data<_,_>) =
        v2s |> List.fold (fun data v2 -> data |> add v1 v2) d
    let removeAllForward v1 (d:Data<_,_>) =
        match d.forward |> Map.tryFind v1 with
        | None -> d // nothing to remove
        | Some v2s ->
            let deleteFrom (map:Map<_,_>) key =
                match map.[key] |> List.filter ((<>) v1) with
                | [] -> map |> Map.remove key
                | vs -> map |> Map.add key vs // replace with a new, shortened list
            let backward' = v2s |> List.fold deleteFrom d.backward
            { d with backward = backward'; forward = d.forward |> Map.remove v1 }
    let removeAllBackward v2 (d:Data<_,_>) =
        match d.backward |> Map.tryFind v2 with
        | None -> d // nothing to remove
        | Some v1s ->
            let deleteFrom (map:Map<_,_>) key =
                match map.[key] |> List.filter ((<>) v2) with
                | [] -> map |> Map.remove key
                | vs -> map |> Map.add key vs // replace with a new, shortened list
            let forward' = v1s |> List.fold deleteFrom d.forward
            { d with forward = forward'; backward = d.backward |> Map.remove v2 }

module Queue =
    type Data<'t> = { frontList: 't list; reversedList: 't list }
    [<GeneralizableValue>]
    let create<'t> = { frontList = []; reversedList = [] }: Data<'t>
    let isEmpty (queue: Data<_>) = queue.frontList = [] && queue.reversedList = []
    let add input (queue: Data<_>) = { queue with reversedList = input::queue.reversedList }
    let normalize = function
        | { reversedList = [] } as queue -> queue
        | { frontList = front; reversedList = rev } -> { frontList = front @ (List.rev rev); reversedList = [] }
    let (|Pop|) (queue: Data<_>) = 
        match normalize queue with
        | { frontList = h::rest } as queue -> (h, ({ queue with frontList = rest })) |> Some
        | _ -> None
    
