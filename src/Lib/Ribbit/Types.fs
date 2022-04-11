module Domain.Ribbit.Types

open Domain.Character
type Id = int

type RuntimeValue = Number of int | Id of Id | Text of string | Roll of RollSpec | Flags of obj list | Bool of bool
type Row = Map<Name, RuntimeValue>

type Property = interface
    abstract Name: string
    abstract HasValue: State * Id -> bool
    abstract GetRuntimeValue: State * Id -> RuntimeValue
    end

and Scope = {
    rows: Map<Id, Row>
    biggestIdSoFar: Id option
    }
    with static member fresh = { rows = Map.empty; biggestIdSoFar = None }

and State = {
    kindsOfMonsters: Map<Name, Id> // used for looking up kinds if necessary
    roster: Map<Name, Id> // actual creatures, by specific name like "Orc #1" or "Ugly Orc"
    categories: Map<Name, (Id*Name) list> // mapping from king like "Orc" to specific name. Used for generating unique specific names.
    scope: Scope
    }
    with static member fresh = { scope = Scope.fresh; kindsOfMonsters = Map.empty; roster = Map.empty; categories = Map.empty }

let rec private getFromScope (rowId: Id, propertyName: Name, defaultValue, getter) (scope:Scope) =
    match scope.rows with
    | Lookup rowId (Lookup propertyName value) -> getter value
    | Lookup rowId (Lookup "prototype" (Id id)) when id > 0 -> getFromScope (id, propertyName, defaultValue, getter) scope
    | _ -> defaultValue()

let getFromState (rowId, propertyName, defaultValue, getter) (state: State) = getFromScope (rowId, propertyName, defaultValue, getter) state.scope

let private setScope (rowId: Id, propertyName: Name, runtimeValue: RuntimeValue) (scope:Scope) =
    let setProperty = function
        | Some row -> Some (row |> Map.add propertyName runtimeValue)
        | None -> Some (Map.ofList [propertyName, runtimeValue])
    let data' = scope.rows |> Map.change rowId setProperty
    { scope with rows = data' }

let private upsertScope (rowId: Id, propertyName: Name, upsert: RuntimeValue option -> RuntimeValue) (state:State) =
    let scope = state.scope
    let setProperty = function
        | Some row -> Some (row |> Map.change propertyName (upsert >> Some))
        | None ->
            let inheritedValue = scope |> getFromScope(rowId, propertyName, thunk None, Some)
            Some (Map.ofList [propertyName, upsert inheritedValue])
    let data' = scope.rows |> Map.change rowId setProperty
    { state with scope = { scope with rows = data' } }

let updateScope (state: State) (f: Scope -> Scope) =
    { state with scope = state.scope |> f }

let private setState (rowId: Id, propertyName: Name, runtimeValue: RuntimeValue as args) (state:State) =
    { state with scope = state.scope |> setScope args }

let hasValue (rowId, propertyName) (state:State) =
    match state.scope.rows with
    | Lookup rowId (Lookup propertyName _) -> true
    | _ -> false

type NumberProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(state, rowId) = this.Get rowId state |> Number
        member this.HasValue(state, rowId) = hasValue (rowId, name) state
    new(name, defaultValue: int) = NumberProperty(name, thunk defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State)=
        state |> setState (rowId, name, Number value)
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Number n -> n | _ -> defaultValue())

type BoolProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Bool
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: bool) = BoolProperty(name, thunk defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Bool value)
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Bool b -> b | _ -> defaultValue())

type RollProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Roll
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: RollSpec) = RollProperty(name, thunk defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Roll value)
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Roll r -> r | _ -> defaultValue())

type FlagsProperty<'t>(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = notImpl()
        member this.HasValue(scope, rowId) = false
    new(name, defaultValue: bool) = FlagsProperty(name, thunk defaultValue)
    member this.Name = name
    member this.Set(rowId, targetFlag:'t, value) (state: State) =
        state |> upsertScope (rowId, name, function
            | (Some (Flags lst)) ->
                if value = (lst |> List.contains (box targetFlag)) then Flags lst
                elif value then (box targetFlag)::lst |> Flags
                else lst |> List.filter ((<>) (box targetFlag)) |> Flags
            | _ -> (if value then [box targetFlag] else []) |> Flags
            )
    member this.Get(rowId, targetFlag:'t) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Flags lst -> lst |> List.contains (box targetFlag) | _ -> defaultValue())

type IdProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Id
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: int) = IdProperty(name, thunk defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Id value)
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Id n -> n | _ -> defaultValue())

type TextProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Text
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: string) = TextProperty(name, thunk defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Text value)
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Text t -> t | _ -> defaultValue())
