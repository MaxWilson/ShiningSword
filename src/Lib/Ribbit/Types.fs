namespace Domain.Ribbit

open Domain.Character
type Id = int

type RuntimeType = Number | Id  | Text | Roll | Rolls | Flags | Bool
type RuntimeValue = Number of int | Id of Id | Text of string | Roll of RollSpec | Rolls of RollSpec list | Flags of string Set| Bool of bool
type Row = Map<Name, RuntimeValue>

type RibbitError = DataRequest of int * propertyName: Name | BugReport of msg: string

[<AbstractClass>]
type Property<'t>() =
    abstract Name: string
    abstract Type: RuntimeType
    abstract Get: Id -> State -> 't
    abstract GetM: Id -> Expression<'t>
    abstract Set: Id *'t -> State -> State
    abstract SetM: Id * 't -> Statement

and RValue<'t> = StateChange<State, 't> // In this context, rvalue = something that can be bound to a let! variable
and Statement = StateChange<State, unit>
and ExpressionResult<'t> = Result<'t, RibbitError>
and Expression<'t> = StateChange<State, Result<'t, RibbitError>> // Expression = something that can yield an RValue EVENTUALLY once all requests are filled

and PropertiesByType = {
    number: Map<Name, Property<int>>
    id: Map<Name, Property<Id>>
    roll: Map<Name, Property<RollSpec>>
    rolls: Map<Name, Property<RollSpec list>>
    flags: Map<Name, Property<string Set>>
    bool: Map<Name, Property<bool>>
    }
    with static member fresh = { number = Map.empty; id = Map.empty; roll = Map.empty; rolls = Map.empty; flags = Map.empty; bool = Map.empty }

and Scope = {
    rows: Map<Id, Row>
    biggestIdSoFar: Id option
    }
    with static member fresh = { rows = Map.empty; biggestIdSoFar = None }

and Affordance = {
    name: Name
    action: Id -> State -> State
    }

and State = {
    properties: PropertiesByType // used primarily in parsing and serialization scenarios. Otherwise use props directly via object references.
    kindsOfMonsters: Map<Name, Id> // used for looking up kinds if necessary
    roster: Map<Name, Id> // actual creatures, by specific name like "Orc #1" or "Ugly Orc"
    categories: Map<Name, (Id*Name) list> // mapping from king like "Orc" to specific name. Used for generating unique specific names.
    scope: Scope
    affordances: Map<Name, Affordance>
    openRequests: RibbitError list // probably also need some way of knowing what to restart after a request is filled
    }
    with
    static member fresh = { scope = Scope.fresh; kindsOfMonsters = Map.empty; roster = Map.empty; categories = Map.empty;
        affordances = Map.empty; properties = PropertiesByType.fresh; openRequests = []
        }

type FightResult = Victory | Defeat | Ongoing
type RoundResult = { outcome: FightResult; msgs: string list; ribbit: State }

// this doesn't properly belong in ribbit but because treasure is part of the aftermath of the combat,
// and because it's convenient to put TreasureType in the monster stat blocks, I'll allow it for now
type TreasureType = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z

module Ops =
    let toStatement logic : Statement = fun state -> (), logic state
    let castFailure (runtimeType: RuntimeType) id propName (v: RuntimeValue) =
        BugReport $"row #{id} property {propName} was should be a {runtimeType} but was actually {v}" |> Error
    // registerRequest adds a request to the list of things a user will be requested to fill. Associating
    // a request with logic to retry will happen separately and later, when the caller fails to synchronously
    // receive a value.
    let registerRequest request : Statement = fun state -> (), { state with openRequests = request::state.openRequests }
    let private _get (rowId: Id, propertyName: Name, fallback, getter) (state: State) =
        let rec recur rowId' =
            match state.scope.rows with
            | Lookup rowId' (Lookup propertyName value) -> getter value
            | Lookup rowId' (Lookup "prototype" (Id id)) when id > 0 -> recur id
            | _ -> fallback rowId propertyName state
        recur rowId

    let getSynchronously (rowId: Id, propertyName: Name, defaultValue, getter) =
        let fallback _ _ _ =
            match defaultValue with
            | Some v -> Ok v
            | None -> BugReport $"row #{rowId} property {propertyName} was accessed synchronously but was actually missing" |> Error
        _get (rowId, propertyName, fallback, getter)

    let getAsync<'t> (rowId, propertyName, defaultValue: 't option, castRuntimeValue) : Expression<'t> = fun state ->
        let fallback rowId propertyName = stateChange {
            // first check if there's a default
            match defaultValue with
            | Some v -> return Ok v
            | _ ->
                let req = DataRequest(rowId, propertyName)
                do! registerRequest req
                return Error req
            }
        let getter runtimeValue =
            castRuntimeValue runtimeValue, state
        _get (rowId, propertyName, fallback, getter) state

    let setScope (rowId: Id, propertyName: Name, runtimeValue: RuntimeValue) (scope:Scope) =
        let setProperty = function
            | Some row -> Some (row |> Map.add propertyName runtimeValue)
            | None -> Some (Map.ofList [propertyName, runtimeValue])
        let data' = scope.rows |> Map.change rowId setProperty
        { scope with rows = data' }

    // this doesn't smell quite right--why does it need to be specially synchronous? Used only for FlagsProperty. TODO: decide if upsert should be an expression instead
    let upsertScope (rowId: Id, propertyName: Name, defaultValue: RuntimeValue, upsert: RuntimeValue option -> RuntimeValue) (state:State) =
        let scope = state.scope
        let setProperty = function
            | Some row -> Some (row |> Map.change propertyName (upsert >> Some))
            | None ->
                match state |> getSynchronously (rowId, propertyName, Some defaultValue, Ok) with
                | Ok inheritedValue ->
                    Some (Map.ofList [propertyName, upsert (Some inheritedValue)])
                | Error err ->
                    shouldntHappen()
        let data' = scope.rows |> Map.change rowId setProperty
        { state with scope = { scope with rows = data' } }

    let updateScope (state: State) (f: Scope -> Scope) =
        { state with scope = state.scope |> f }

    let setState (rowId: Id, propertyName: Name, runtimeValue: RuntimeValue as args) (state:State) =
        { state with scope = state.scope |> setScope args }

    let hasValue (rowId, propertyName) (state:State) =
        match state.scope.rows with
        | Lookup rowId (Lookup propertyName _) -> true
        | _ -> false
open Ops

type NumberProperty(name, defaultValue: _ option) =
    inherit Property<int>()
    override this.Name = name
    override this.Type = RuntimeType.Number
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Number value)
    override this.SetM(rowId, value) = setState (rowId, name, Number value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, defaultValue, function Number n -> Ok n | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, defaultValue, function Number n -> Ok n | v -> castFailure RuntimeType.Number rowId name v)
    new(name, defaultValue: int) = NumberProperty(name, Some defaultValue)
    new(name) = NumberProperty(name, None)

type BoolProperty(name, defaultValue: _ option) =
    inherit Property<bool>()
    override this.Name = name
    override this.Type = RuntimeType.Bool
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Bool value)
    override this.SetM(rowId, value) = setState (rowId, name, Bool value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, defaultValue, function Bool b -> Ok b | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, defaultValue, function Bool b -> Ok b | v -> castFailure RuntimeType.Bool rowId name v)
    new(name, defaultValue: bool) = BoolProperty(name, Some defaultValue)
    new(name) = BoolProperty(name, None)

type RollProperty(name, defaultValue) =
    inherit Property<RollSpec>()
    override this.Name = name
    override this.Type = RuntimeType.Roll
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Roll value)
    override this.SetM(rowId, value) = setState (rowId, name, Roll value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, defaultValue, function Roll r -> Ok r | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, defaultValue, function Roll r -> Ok r | v -> castFailure RuntimeType.Roll rowId name v)
    new(name, defaultValue: RollSpec) = RollProperty(name, Some defaultValue)
    new(name) = RollProperty(name, None)

type RollsProperty(name, defaultValue) =
    inherit Property<RollSpec list>()
    override this.Name = name
    override this.Type = RuntimeType.Rolls
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Rolls value)
    override this.SetM(rowId, value) = setState (rowId, name, Rolls value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, defaultValue, function Rolls rs -> Ok rs | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, defaultValue, function Rolls rs -> Ok rs | v -> castFailure RuntimeType.Rolls rowId name v)
    new(name, defaultValue: RollSpec list) = RollsProperty(name, Some defaultValue)
    new(name) = RollsProperty(name, None)

type FlagsProperty<'t>(name, defaultValue: string Set) =
    inherit Property<string Set>()
    override this.Name = name
    override this.Type = RuntimeType.Flags
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Flags value)
    override this.SetM(rowId, value) = setState (rowId, name, Flags value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, Some defaultValue, function Flags f -> Ok f | _ -> Ok defaultValue) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, Some defaultValue, function Flags b -> Ok b | v -> castFailure RuntimeType.Flags rowId name v)
    member this.SetAll(rowId, value) = (this :> Property<string Set>).Set(rowId, value)
    member this.SetFlag(rowId, targetFlag:'t, value) (state: State) =
        let target = targetFlag.ToString()
        state |> upsertScope (rowId, name, Flags defaultValue, function
            | (Some (Flags set)) ->
                if value = (set.Contains target) then Flags set
                elif value then set.Add target |> Flags
                else set |> Set.filter ((<>) target) |> Flags
            | _ -> (if value then [target] else []) |> Set.ofList |> Flags
            )
    member this.SetAllM(rowId, value) (state: State) = (), this.SetAll(rowId, value) state
    member this.SetFlagM(rowId, targetFlag, value) (state: State) = (), this.SetFlag(rowId, targetFlag, value) state
    member this.Check(rowId, targetFlag:'t) (state: State) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        match state |> getSynchronously (rowId, name, (Some false), function Flags flags -> check flags |> Ok | v -> castFailure RuntimeType.Flags rowId name v) with
        | Ok v -> v
        | Error err -> shouldntHappen err
    member this.CheckM(rowId, targetFlag) (state: State) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        state |> getAsync (rowId, name, (Some false), function Flags flags -> check flags |> Ok | v -> castFailure RuntimeType.Flags rowId name v)
    new(name) = FlagsProperty(name, Set.empty)

type IdProperty(name, defaultValue) =
    inherit Property<Id>()
    override this.Name = name
    override this.Type = RuntimeType.Id
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Id value)
    override this.SetM(rowId, value) = setState (rowId, name, Id value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, defaultValue, function Id id -> Ok id | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, defaultValue, function Id id -> Ok id | v -> castFailure RuntimeType.Id rowId name v)
    new(name, defaultValue: int) = IdProperty(name, Some defaultValue)
    new(name) = IdProperty(name, None)

type TextProperty(name, defaultValue) =
    inherit Property<string>()
    override this.Name = name
    override this.Type = RuntimeType.Text
    override this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Text value)
    override this.SetM(rowId, value) = setState (rowId, name, Text value) |> toStatement
    override this.Get(rowId) (state: State) =
        match state |> getSynchronously (rowId, name, defaultValue, function Text t -> Ok t | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getAsync (rowId, name, defaultValue, function Text t -> Ok t | v -> castFailure RuntimeType.Text rowId name v)
    new(name, defaultValue: string) = TextProperty(name, Some defaultValue)
    new(name) = TextProperty(name, None)
