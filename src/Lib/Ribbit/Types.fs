namespace Domain.Ribbit

open Domain.Character
type Id = int

type RuntimeValue = Number of int | Id of Id | Text of string | Roll of RollSpec | Rolls of RollSpec list | Flags of string Set| Bool of bool
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

and Affordance = {
    name: Name
    action: Id -> State -> State
    }

and State = {
    kindsOfMonsters: Map<Name, Id> // used for looking up kinds if necessary
    roster: Map<Name, Id> // actual creatures, by specific name like "Orc #1" or "Ugly Orc"
    categories: Map<Name, (Id*Name) list> // mapping from king like "Orc" to specific name. Used for generating unique specific names.
    scope: Scope
    affordances: Map<Name, Affordance>
    }
    with static member fresh = { scope = Scope.fresh; kindsOfMonsters = Map.empty; roster = Map.empty; categories = Map.empty; affordances = Map.empty }

type Request = DataRequest of int * propertyName: Name | BehaviorRequestPlaceholder
type Expression<'t> = Result<'t, Request>
type FightResult = Victory | Defeat | Ongoing
type RoundResult = { outcome: FightResult; msgs: string list; ribbit: State }

module Ops =
    let getFromScope (rowId: Id, propertyName: Name, defaultValue, getter) (scope:Scope) =
        let rec recur rowId' =
            match scope.rows with
            | Lookup rowId' (Lookup propertyName value) -> getter value
            | Lookup rowId' (Lookup "prototype" (Id id)) when id > 0 -> recur id
            | _ -> defaultValue rowId propertyName scope
        recur rowId

    let getFromState (rowId, propertyName, defaultValue, getter) (state: State) = getFromScope (rowId, propertyName, defaultValue, getter) state.scope

    let setScope (rowId: Id, propertyName: Name, runtimeValue: RuntimeValue) (scope:Scope) =
        let setProperty = function
            | Some row -> Some (row |> Map.add propertyName runtimeValue)
            | None -> Some (Map.ofList [propertyName, runtimeValue])
        let data' = scope.rows |> Map.change rowId setProperty
        { scope with rows = data' }

    let upsertScope (rowId: Id, propertyName: Name, upsert: RuntimeValue option -> RuntimeValue) (state:State) =
        let scope = state.scope
        let setProperty = function
            | Some row -> Some (row |> Map.change propertyName (upsert >> Some))
            | None ->
                let inheritedValue = scope |> getFromScope(rowId, propertyName, (fun _ _ _ -> None), Some)
                Some (Map.ofList [propertyName, upsert inheritedValue])
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

type NumberProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(state, rowId) = this.Get rowId state |> Number
        member this.HasValue(state, rowId) = hasValue (rowId, name) state
    new(name, defaultValue: int) = NumberProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Number value)
    member this.SetM(rowId, value) (state: State) = (), this.Set(rowId, value) state
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Number n -> n | _ -> defaultValue rowId name state.scope)
    member this.GetM(rowId) (state: State) = this.Get (rowId) state, state

type BoolProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Bool
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: bool) = BoolProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Bool value)
    member this.SetM(rowId, value) (state: State) = (), this.Set(rowId, value) state
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Bool b -> b | _ -> defaultValue rowId name state.scope)
    member this.GetM(rowId) (state: State) = this.Get (rowId) state, state

type RollProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Roll
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: RollSpec) = RollProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Roll value)
    member this.SetM(rowId, value) (state: State) = (), this.Set(rowId, value) state
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Roll r -> r | _ -> defaultValue rowId name state.scope)
    member this.GetM(rowId) (state: State) = this.Get (rowId) state, state

type RollListProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Rolls
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: RollSpec list) = RollListProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Rolls value)
    member this.SetM(rowId, value) (state: State) = (), this.Set(rowId, value) state
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Rolls r -> r | _ -> defaultValue rowId name state.scope)
    member this.GetM(rowId) (state: State) = this.Get (rowId) state, state

type FlagsProperty<'t>(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = failwith "Nobody should be calling FlagsProperty.GetRuntimeValue()"
        member this.HasValue(scope, rowId) = false
    new(name, defaultValue: string Set) = FlagsProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.SetAll(rowId, value) (state: State) =
        state |> setState (rowId, name, Flags value)
    member this.Set(rowId, targetFlag:'t, value) (state: State) =
        let target = targetFlag.ToString()
        state |> upsertScope (rowId, name, function
            | (Some (Flags set)) ->
                if value = (set.Contains target) then Flags set
                elif value then set.Add target |> Flags
                else set |> Set.filter ((<>) target) |> Flags
            | _ -> (if value then [target] else []) |> Set.ofList |> Flags
            )
    member this.SetAllM(rowId, value) (state: State) = (), this.SetAll(rowId, value) state
    member this.SetM(rowId, targetFlag, value) (state: State) = (), this.Set(rowId, targetFlag, value) state
    member this.Check(rowId, targetFlag:'t) (state: State) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            printfn $"Checking: {set} contains {targetFlag}? {set.Contains target}"
            set.Contains target
        state |> getFromState (rowId, name, (fun a b c -> defaultValue a b c |> check), function Flags flags -> check flags | _ -> defaultValue rowId name state.scope |> check)
    member this.CheckM(rowId) (state: State) = this.Check (rowId) state, state

type IdProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Id
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: int) = IdProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Id value)
    member this.SetM(rowId, value) (state: State) = (), this.Set(rowId, value) state
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Id n -> n | _ -> defaultValue rowId name state.scope)
    member this.GetM(rowId) (state: State) = this.Get (rowId) state, state

type TextProperty(name, defaultValue) =
    interface Property with
        member this.Name = name
        member this.GetRuntimeValue(scope, rowId) = this.Get rowId scope |> Text
        member this.HasValue(scope, rowId) = hasValue (rowId, name) scope
    new(name, defaultValue: string) = TextProperty(name, fun _ _ _ -> defaultValue)
    member this.Name = name
    member this.Set(rowId, value) (state: State) =
        state |> setState (rowId, name, Text value)
    member this.SetM(rowId, value) (state: State) = (), this.Set(rowId, value) state
    member this.Get(rowId) (state: State) =
        state |> getFromState (rowId, name, defaultValue, function Text t -> t | _ -> defaultValue rowId name state.scope)
    member this.GetM(rowId) (state: State) = this.Get (rowId) state, state

// this doesn't properly belong in ribbit but because treasure is part of the aftermath of the combat,
// and because it's convenient to put TreasureType in the monster stat blocks, I'll allow it for now
type TreasureType = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z
