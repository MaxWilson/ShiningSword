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
let rec Id = numProp (nameof Id)
let rec Kind = numProp (nameof Kind) // for data parent relationships: orc #1 is a kind of orc and inherits data from it, etc.
let rec Name = stringProp (nameof Name)
let r = fresh
r |> writeSome Name "Bob" |> read Name
r |> read Name
let mutable r1 = fresh
iter &r1 (writeSome Name "Bob")
iter &r1 (writeSome Id 3)
iter &r1 (over Id (Option.map ((+) 1)))

module Environment =
    type d<'key, 'value when 'key: comparison> = {
        data: Map<'key, 'value>
        freshkey: d<'key, 'value> -> 'key * d<'key, 'value>
        }
    let add k v d =
        { d with data = d.data |> Map.add k v }
    let data_ = lens(fun d -> d.data) (fun v d -> { d with data = v })

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
    globalData: Row.d
    data: Environment.d<RowKey,Row.d>
    roster: Map<IndividualId, string>
    awaiting: Awaiter list
    }
    with
    static member fresh =
        let rec nextKey key d =
            key, { d with freshkey = nextKey (key+1) }
        {
        globalData = Row.fresh
        data = { data = Map.empty; freshkey = nextKey 1 }
        roster = Map.empty
        awaiting = []
        }
    static member data_ = lens(fun d -> d.data) (fun v d -> { d with data = v })

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
        [<CustomOperation("template", MaintainsVariableSpaceUsingBind=false)>]
        member _.Template (monad, template) =
            printfn "Template1: %A + %A ==> %A" monad template (monad@[FromTemplate(1,template)])
            monad@[FromTemplate(1,template)]
        [<CustomOperation("template", MaintainsVariableSpaceUsingBind=false)>]
        member _.Template (monad, n, template) =
            printfn "Template2: %A + %A %A ==> %A" monad n template (monad@[FromTemplate(n,template)])
            monad@[FromTemplate(n,template)]
        [<CustomOperation("add")>]
        member _.Add (monad, name) =
            printfn "Add: %A + %A ==> %A" monad name (monad@[ByName(name)])
            monad@[ByName(name)]
        member _.Zero() = printfn "Zero"; []
        member _.Yield(()) = printfn "Yield"; []
        member _.Yield(n:int) =
            printfn "Yield %A" n
            [ByName "Int"]
        member _.Yield(name:string) =
            printfn "Yield %A" name
            [ByName name]
        member _.Return(name:string) =
            printfn "Return %A" name
            ByName name
        member _.Combine(lhs, rhs) =
            printfn "Combining %A %A" lhs rhs
            let rhs = rhs()
            let result = lhs@rhs
            printfn "%A + %A ==> %A" lhs rhs result
            result
        member _.Delay(f) =
            let x = System.Guid.NewGuid()
            printfn "Delay prep %A" x
            fun() ->
                printfn "Un-delaying %A" x
                let r = f()
                printfn "Delayed %A: %A" x r
                r
        member _.For(monad,f) =
            printfn "For"
            monad@(f())
        member _.Run(f) = f()
    let build = InputBuilder()

Loader.build {
    template 4 "Ogre"
    let x = 2 in "Frodo"
    template "Orc"
    //template 4 "ogre"
    }

let supplyData (rowId: RowKey option) (property:Property<'t>) (value: 't) state : GameState =
    // for now, just sets data, but later on should check for any just-unblocked work
    match rowId with
    | Some rowId ->
        state |> writeSome (GameState.data_ => Environment.data_ => (Map.keyed_ rowId) => property) value
    | None ->
        { state with globalData = state.globalData |> writeSome property value }
let loadIndividuals references (state: GameState) : GameState =
    let loadByName name (state: GameState) =
        let id, data = state.data.freshkey state.data
        let state = { state with
                        roster = state.roster |> Map.add id name
                        data = data |> Environment.add id Row.fresh
                        }
        state
    let loadFromTemplate (n, template) (state: GameState) =
        [1..n] |> List.fold (fun state n -> loadByName ($"{template} #{n}") state) state
    references
    |> List.fold (fun state -> function
                    | Loader.ByName name -> loadByName name state
                    | Loader.FromTemplate(n, template) -> loadFromTemplate(n,template) state)
                 state

// stubs for developing the interface
let star = Unchecked.defaultof<Wildcard>

let startEvent _ state : GameState = notImpl()
let fixpoint state : GameState = notImpl()
let define _ state : GameState = notImpl()
let read (property: Row.Property<'t>) (rowId: RowKey) (state: GameState) : 't option * GameState =
    // currently has no notion of "blocked". Will add that later, as a None.
    match state |> read (GameState.data_ => Environment.data_ => (Map.keyed_ rowId) => property) with
    | Some(Some v) -> Some v, state
    | _ -> None, state // bad rowId or unset property
let requiredData (state: GameState) : DataId list = notImpl()
let requiredRolls (state: GameState) : RollId list = notImpl()
let describeRoll (id: RollId) (state: GameState): string = notImpl()
let individuals (state: GameState) : IndividualId list = state.roster |> Map.keys |> List.ofSeq
let describeIndividual (id: IndividualId) (state: GameState): string = state.roster.[id]
let roll(n, d) = notImpl()
let fulfill roll n state : GameState = notImpl()

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
iter &state (supplyData (getIndividualByName "ogre #1" state |> Some) hp 42)
iter &state (supplyData (getIndividualByName "ogre #4" state |> Some) hp 46)
for id in (getRollRequirementsByDescription "saving throw" state) do // everybody rolls a 1 for test simplicity
    iter &state (fulfill id 1) |> ignore
iter &state fixpoint // finish processing any events including inflicting damage
iterSnd &state (read hp (getIndividualByName "orc" state)) = Some 4
iterSnd &state (read hp (getIndividualByName "ogre #1" state)) = Some 31 // wounded by Fireball
iterSnd &state (read hp (getIndividualByName "ogre #2" state)) = None // still unknown
iterSnd &state (read hp (getIndividualByName "ogre #3" state)) = None // still unknown
iterSnd &state (read hp (getIndividualByName "ogre #4" state)) = Some 35 // wounded by Fireball
