module Data

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

type FastList<'t> = { rows: Map<int, 't>; lastId: int option }
    with
    static member add (data, row: 't)=
        let id = (defaultArg data.lastId 0) + 1
        { data with rows = data.rows |> Map.add id row; lastId = Some id }
    static member transform(data, (id, f)) =
        let row = data.rows.[id]
        { data with rows = data.rows |> Map.add id (f row) }
    static member replace(data, (id, row)) =
        { data with rows = data.rows |> Map.add id row }
    static member toSeq(data) =
        seq { for i in 1..(defaultArg data.lastId 0) -> data.rows.[i] }
    static member fresh(): FastList<'t> = { rows = Map.empty; lastId = None }
    static member tryFind(data, id) =
        data.rows |> Map.tryFind id

module SymmetricMap =
    type Data<'key, 'v when 'key: comparison and 'v: comparison> = Map<'key, 'v> * Map<'v, 'key>
    let inline empty() = Map.empty, Map.empty
    let find k (d: Data<_,_>) = (fst d) |> Map.find k
    let findValue k (d: Data<_,_>) = (snd d) |> Map.find k
    let tryFind k (d: Data<_,_>) = (fst d) |> Map.tryFind k
    let tryFindValue k (d: Data<_,_>) = (snd d) |> Map.tryFind k
    let add k v (m1, m2) = m1 |> Map.add k v, m2 |> Map.add v k
    let toSeq (d: Data<_,_>) = fst d |> Map.toSeq
    let isEmpty (d: Data<_,_>) = fst d |> Map.isEmpty
    let keys (d: Data<_,_>) = (d |> fst) |> Map.toSeq |> Seq.map fst
    let values (d: Data<_,_>) = (d |> fst) |> Map.toSeq |> Seq.map snd

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
