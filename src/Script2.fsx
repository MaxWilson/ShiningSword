#load "Common.fs"
#load "Optics.fs"
open Common
open Optics
open type Optics.Operations

// normally in F# we do things functionally, but when there's going to be an event loop
// in the middle, it's useful to do our script programming with top-level state too
// so that we're writing APIs in the same mode we'll be consuming them.
module Stateful =
    let iter f (state: byref<'t>) =
        state <- f state
        state
    let rec iterUntil fstop fiter (state: byref<'t>) =
        if fstop state then state
        else
            iter fiter &state |> ignore
            iterUntil fstop fiter &state
open Stateful
//// tests
//let mutable x = 0
//let flip f x y = f y x
//let z = flip (>) 3
//iterUntil (flip (>) 3) ((+)1) &x

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
let mutable r1 = fresh
iter (writeSome name "Bob") &r1
iter (writeSome id 3) &r1
iter (over id (Option.map ((+) 1))) &r1

module Environment =
    type d<'key, 'value when 'key: comparison> = {
        data: Map<'key, 'value>
        freshkey: unit -> 'key * d<'key, 'value>
        }

open Environment
type NumberOrStringOrList = Number of int | String of string | List of NumberOrStringOrList

type RowKey = int * string
type Wildcard() = do failwith "Not impl"
type Id = Event of int | Step of int
[<AbstractClass>]
type Awaiter(msg: string, dest: RowKey) =
    abstract tryFulfill: GameState -> string -> GameState option
and GameState = {
    data: Environment.d<RowKey,NumberOrStringOrList>
    awaiting: Awaiter list
    }

type 't Value = Ready of 't | Awaiting of Awaiter
type 't Result = 't Value * GameState
type EventValue = Wildcard // will probably end up some kind of union
[<AbstractClass>]
type API() =
    // state + actor, event = id + state'
    abstract startAction: Wildcard -> GameState -> Id * GameState
    // state + event = state'
    abstract startEvent: Wildcard -> GameState -> Id * GameState
    // state + id = result?
    abstract readEventResult: Id -> GameState -> EventValue Result
    // read: property + GameState -> result?
    abstract read: Property<'t> -> GameState -> 't Result
    // step + event = state'
    abstract startStep: Wildcard -> GameState -> GameState
    // data + event = state'
    abstract setData: Wildcard -> GameState -> GameState

// null stubs for developing the interface
let setup (api: API) _ = Unchecked.defaultof<GameState>
let star = Unchecked.defaultof<Wildcard>
let api = Unchecked.defaultof<API>

// test 1: shoot a Fireball!
let mutable state = setup api ([1, "orc"; 4, "ogre"])

