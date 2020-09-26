#load "Common.fs"
#load "Optics.fs"
open Common
open Optics
open type Optics.Operations

// normally in F# we do things functionally, but when there's going to be an event loop
// in the middle, it's useful to do our script programming with top-level state too
// so that we're writing APIs in the same mode we'll be consuming them.
module Stateful =
    let iter (state: byref<'t>) f =
        state <- f state
        state
    let iterSnd (state: byref<'t>) f =
        let result, state' = f state
        state <- state'
        result
    let rec iterUntil (state: byref<'t>) fstop fiter =
        if fstop state then state
        else
            iter &state fiter |> ignore
            iterUntil &state fstop fiter
open Stateful
//// tests
//let mutable x = 0
//let flip f x y = f y x
//let z = flip (>) 3
//iterUntil &x (flip (>) 3) ((+)1)
//iterSnd &x (fun x -> x*x, x+1)

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
iter &r1 (writeSome name "Bob")
iter &r1 (writeSome id 3)
iter &r1 (over id (Option.map ((+) 1)))

module Environment =
    type d<'key, 'value when 'key: comparison> = {
        data: Map<'key, 'value>
        freshkey: d<'key, 'value> -> 'key * d<'key, 'value>
        }
open Environment

module SymmetricLookup =
    type d<'key, 'value when 'key: comparison and 'value: comparison> = {
        forward: Map<'key, 'value list>
        backward: Map<'value, 'key list>
        }
    let add k v d =
        let addOrCreate k v m =
            let vs' =
                match m |> Map.tryFind k with
                | None -> [v]
                | Some vs -> v::vs
            m |> Map.add k vs'
        { d with
            forward = d.forward |> addOrCreate k v
            backward = d.backward |> addOrCreate v k }
    let private remove k v m =
        match m |> Map.tryFind k with
        | None -> m
        | Some vs ->
            match vs |> List.filter ((<>) v) with
            | [] -> m |> Map.remove k
            | vs' -> m |> Map.add k vs'
    let removeForward k d =
        let vs = match d.forward |> Map.tryFind k with Some vs -> vs | None -> []
        { d with
            forward = d.forward |> Map.remove k
            backward = vs |> List.fold (fun map v -> remove v k map) d.backward }
    let removeBackward v d =
        let ks = match d.backward |> Map.tryFind v with Some vs -> vs | None -> []
        { d with
            forward = ks |> List.fold (fun map k -> remove k v map) d.forward
            backward = d.backward |> Map.remove v }

    let fresh = { forward = Map.empty; backward = Map.empty }
open SymmetricLookup

type NumberOrStringOrList = Number of int | String of string | List of NumberOrStringOrList

type RowKey = int
type Wildcard() = do failwith "Not impl"
type Id = Event of int | Step of int
type RollId = int
type IndividualId = RowKey // individuals are a subset of data
type PropertyName = string
type DataId = RowKey option * PropertyName

[<AbstractClass>]
type Awaiter(msg: string, dest: RowKey) =
    abstract tryFulfill: GameState -> string -> GameState option
and GameState = {
    data: Environment.d<RowKey,Row.d>
    roster: SymmetricLookup.d<string, IndividualId>
    awaiting: Awaiter list
    }
    with
    static member fresh =
        let rec nextKey key d =
            key, { d with freshkey = nextKey (key+1) }
        {
        data = { data = Map.empty; freshkey = nextKey 1 }
        roster = SymmetricLookup.fresh
        awaiting = []
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

module Loader =
    type LoadKind = ByName of string | FromTemplate of int * string
    type Output = LoadKind list
    type InputBuilder() =
        [<CustomOperation("template")>]
        member _.Template (monad, template) = monad@[FromTemplate(1,template)]
        [<CustomOperation("template")>]
        member _.Template (monad, n, template) = monad@[FromTemplate(n,template)]
        [<CustomOperation("add")>]
        member _.Add (monad, name) =
            monad@[ByName(name)]
        member _.Zero() = []
        member _.Yield(()) = []
        member _.Yield(name:string) = [ByName name]
        member _.Combine(lhs, rhs) = lhs@rhs
        member _.Delay(f) = f()
        member _.For(monad,f) = monad@(f())
    let build = InputBuilder()

// null stubs for developing the interface
let loadIndividuals _ state : GameState = GameState.fresh
let star = Unchecked.defaultof<Wildcard>

let startEvent _ state : GameState = notImpl()
let fixpoint state : GameState = notImpl()
let define _ state : GameState = notImpl()
let read (prop: Row.Property<'t>) rowId state : 't option * GameState = notImpl()
let requiredData (state: GameState) : DataId list = notImpl()
let requiredRolls (state: GameState) : RollId list = notImpl()
let describeRoll (id: RollId) (state: GameState): string = notImpl()
let individuals (state: GameState) : IndividualId list = notImpl()
let describeIndividual (id: IndividualId) (state: GameState): string = notImpl()
let roll(n, d) = notImpl()
let fulfill roll n state : GameState = notImpl()
let supplyData (rowId: RowKey option) (property:Property<'t>) (value: 't) state : GameState = notImpl()

// helper function for testing, not really used at runtime
let getRollRequirementsByDescription descriptionSubstring state =
    requiredRolls state
    |> List.filter (fun id -> (describeRoll id state).Contains descriptionSubstring)
let getRollRequirementByDescription descriptionSubstring state =
    getRollRequirementsByDescription descriptionSubstring state
    |> List.head
let getIndividualsByName nameSubstring state =
    individuals state
    |> List.filter (fun id -> (describeIndividual id state).Contains nameSubstring)
let getIndividualByName nameSubstring state =
    getIndividualsByName nameSubstring state
    |> List.head

// test 1: shoot a Fireball!
let hp = Row.numProp "HP"
let ruleSet = {| properties = [hp] |}
let mutable state = GameState.fresh
iter &state (define ruleSet)
iter &state (loadIndividuals (Loader.build { "Frodo"; template "orc"; template 4 "ogre" }))
iter &state (startEvent "Fireball")
iter &state fixpoint
iterSnd &state (read hp (getIndividualByName "orc" state)) = None
iter &state (fulfill (getRollRequirementByDescription "8d6" state) 11)
iter &state (supplyData (getIndividualByName "orc" state |> Some) hp 15)
iter &state (supplyData (getIndividualByName "ogre1" state |> Some) hp 42)
iter &state (supplyData (getIndividualByName "ogre4" state |> Some) hp 46)
for id in (getRollRequirementsByDescription "saving throw" state) do // everybody rolls a 1 for test simplicity
    iter &state (fulfill id 1) |> ignore
iter &state fixpoint // finish processing any events including inflicting damage
iterSnd &state (read hp (getIndividualByName "orc" state)) = Some 4
iterSnd &state (read hp (getIndividualByName "ogre1" state)) = Some 31 // wounded by Fireball
iterSnd &state (read hp (getIndividualByName "ogre2" state)) = None // still unknown
iterSnd &state (read hp (getIndividualByName "ogre3" state)) = None // still unknown
iterSnd &state (read hp (getIndividualByName "ogre1" state)) = Some 35 // wounded by Fireball
