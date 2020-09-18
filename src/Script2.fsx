#load "Common.fs"
#load "Optics.fs"
open Common
open Optics
open Optics.Operations

module Row =
    type d = Map<string, obj>
    type Property<'t> = Lens<d, 't option>
    type 't Status = Ready of 't | Deferred
    let fresh = Map.empty
    let private set k v d =
        match v with
        | Some v -> Map.add k (box v) d
        | None -> Map.remove k d
    let stringProp name : Property<string> =
        lens (Map.tryFind name >> Option.map unbox<string>) (set name)
    let numProp name : Property<int> =
        lens (Map.tryFind name >> Option.map unbox<int>) (set name)
    let rowProp name : Property<d> =
        lens (Map.tryFind name >> Option.map unbox<d>) (set name)
    let intListProp name : Property<int list> =
        lens (Map.tryFind name >> Option.map unbox<int list>) (set name)
    let rowListProp name : Property<d list> =
        lens (Map.tryFind name >> Option.map unbox<d list>) (set name)
    let stringListProp name : Property<string list> =
        lens (Map.tryFind name >> Option.map unbox<string list>) (set name)
    let apply (p: Property<'t>) (row: d) =
        match read p row with
        | Some v -> Ready v
        | _ -> Deferred
open Row
let rec id = numProp (nameof id)
let rec name = stringProp (nameof name)
let r = fresh
r |> writeSome name "Bob" |> read name
r |> read name

module Environment =
    type d<'key, 'value when 'key: comparison> = {
        data: Map<'key, 'value>
        freshkey: unit -> 'key * d<'key, 'value>
        }

open Environment
type NumberOrStringOrList = Number of int | String of string | List of NumberOrStringOrList

type RowKey = int * string
type GameState = {
    data: Environment.d<RowKey,NumberOrStringOrList>
    }

type Wildcard() = do failwith "Not impl"

[<AbstractClass>]
type Awaiter(msg: string, dest: RowKey) =
    abstract tryFulfill: GameState -> string -> GameState option

type 't Value = Ready of 't | Awaiting of Awaiter

[<AbstractClass>]
type API() =
    // state + actor, event = state'
    abstract startAction: Wildcard -> GameState -> GameState
    // state + event = state'
    abstract startEvent: Wildcard -> GameState -> GameState
    // read: property + GameState -> result + GameState
    abstract read: Property<'t> -> GameState -> 't Value * GameState

