namespace Domain.Ribbit

open Domain.Character
type Id = int

type RuntimeType = Number | Id  | Text | Roll | Rolls | Flags | Bool
type RuntimeValue = Number of int | Id of Id | Text of string | Roll of RollSpec | Rolls of RollSpec list | Flags of string Set| Bool of bool
type Row = Map<Name, RuntimeValue>
type LogCategory = Good | Bad | Neither
type LogEntry = { msg: string; important: bool; category: LogCategory }
    with
    static member create msg = { msg = msg; important = false; category = Neither }
    static member create(msg, important, category) = { msg = msg; important = important; category = category }

type RibbitRequest = DataRequest of Id * propertyName: Name
type Address = LocalAddress of variableName: Name | PropertyAddress of Id * propertyName: Name
type RibbitMsg =
    | ReserveId of Id
    | AssociateMonsterKind of monsterKind:Name * Id
    | AssociateIndividual of personalName:Name * id: Id * monsterKind: Name option
    | RegisterRequest of RibbitRequest
    | Set of Address * value: RuntimeValue
    | SetRoster of Map<Name, Id>
type RibbitError = Awaiting of RibbitRequest | BugReport of msg: string

type Property(name: Name, runtimeType: RuntimeType) =
    member this.Name = name
    member this.Type = runtimeType

[<AbstractClass>]
type Property<'t>(name, runtimeType) =
    inherit Property(name, runtimeType)
    abstract Get: Id -> RibbitData -> 't
    abstract GetM: Id -> Evaluation<'t>
    abstract Set: Id *'t -> Ribbit -> Ribbit
    abstract SetM: Id * 't -> StateChange<Ribbit, unit>
and [<AbstractClass>]
    Expression<'t>() =
    abstract Eval: EvaluationContext -> 't RValue // expressions CANNOT modify state or the evaluation context variables, they can only succeed or fail.
and ExecutionContext = {
    locals: Row // e.g. arguments to the event within which the expression is embedded
    instructionPointer: int
    state: Ribbit
}
and EvaluationContext = {
    locals: Row // e.g. arguments to the event within which the expression is embedded
    ribbit: RibbitData
}
and RValue<'t> = Result<'t, RibbitError>
and Evaluation<'t> = EvaluationContext -> RValue<'t> // In this context, rvalue = something that can be bound to a let! variable. The Evaluation is the thing that we might be able to use to create the RValue, or else a RibbitError like Awaiting DataRequest
and Execution = StateChange<ExecutionContext, RValue<unit>>

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
    action: Id -> Ribbit -> Ribbit
    }

and RibbitData = {
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
and RibbitUnwrapped = Delta.DeltaDrivenState<RibbitData, RibbitMsg>
and Ribbit = Ribbit of RibbitUnwrapped
    with
    static member Data this = match this with Ribbit(data) -> data |> Delta.deref
    static member DataM this = match this with Ribbit(data) -> data |> Delta.deref, this
    static member OfMonad (v, ribbit) = v, Ribbit(ribbit)
    static member ExecuteM msg this : unit * Ribbit = match this with Ribbit(data) -> data |> Delta.executeM msg |> Ribbit.OfMonad
    member this.delta = match this with Ribbit(data) -> data
    member this.data = match this with Ribbit(data) -> data |> Delta.deref
    member this.transform f = match this with Ribbit(data) -> Ribbit (f data)
    member this.transform stateChange = (stateChange |> runNoResult this)
    member this.transformM stateChange = (), match this with Ribbit(data) -> (stateChange |> runNoResult data |> Ribbit)
    member this.transformM stateChange = (), (stateChange |> runNoResult this |> Ribbit)
    
and Statements = Sequence of Statement list | While of Expression<bool> * Statements
and CompiledStatements = Statement array
and Statement =
    | Assign of Address * Expression<RuntimeValue>
    | Jump of int

type FightResult = Victory | Defeat | Ongoing
type RoundResult = { outcome: FightResult; msgs: LogEntry list; ribbit: Ribbit }

// this doesn't properly belong in ribbit but because treasure is part of the aftermath of the combat,
// and because it's convenient to put TreasureType in the monster stat blocks, I'll allow it for now
type TreasureType = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z

module Ops =
    open Delta

    let getRibbit (ctx: EvaluationContext) = ctx.ribbit

    let withEvaluation (f: _ Evaluation) (ctx:ExecutionContext) =
        let ribbit = ctx.state.data
        (f { ribbit = ribbit; locals = ctx.locals }), ctx

    let castFailure (runtimeType: RuntimeType) id propName (v: RuntimeValue) =
        BugReport $"row #{id} property {propName} was should be a {runtimeType} but was actually {v}" |> Error
    // registerRequest adds a request to the list of things a user will be requested to fill. Associating
    // a request with logic to retry will happen separately and later, when the caller fails to synchronously
    // receive a value.
    let registerRequest request = fun state -> (), state |> Delta.execute (RegisterRequest request)

    let private _get (rowId: Id, propertyName: Name, fallback, getter) (ribbit: RibbitData) =
        let rec recur rowId' =
            match ribbit.scope.rows with
            | Map.Lookup rowId' (Map.Lookup propertyName value) -> getter value
            | Map.Lookup rowId' (Map.Lookup "prototype" (Id id)) when id > 0 -> recur id
            | _ -> fallback()
        recur rowId

    let getSynchronously (rowId: Id, propertyName: Name, defaultValue, getter) =
        let fallback() =
            match defaultValue with
            | Some v -> Ok v
            | None -> BugReport $"row #{rowId} property {propertyName} was accessed synchronously but was actually missing" |> Error
        _get (rowId, propertyName, fallback, getter)

    let getAsync<'t> (rowId, propertyName, defaultValue: 't option, castRuntimeValue) (ribbit: RibbitData) =
        let fallback() =
            // first check if there's a default
            match defaultValue with
            | Some v -> Ok v
            | _ ->
                let req = DataRequest(rowId, propertyName)
                Error (Awaiting req)
        let getter runtimeValue = castRuntimeValue runtimeValue
        _get (rowId, propertyName, fallback, getter) ribbit

    // this doesn't smell quite right--why does it need to be specially synchronous? Used only for FlagsProperty. TODO: decide if upsert should be an expression instead
    let upsertScope (rowId: Id, propertyName: Name, defaultValue: RuntimeValue, upsert: RuntimeValue option -> RuntimeValue) : StateChange<Ribbit,unit> = stateChange {
        let! ribbit = Ribbit.DataM
        let scope = ribbit.scope
        let currentValue = ribbit |> _get (rowId, propertyName, thunk None, Some)
        do! Set(PropertyAddress (rowId, propertyName), upsert currentValue) |> Ribbit.ExecuteM
        }

    let setState (rowId: Id, propertyName: Name, runtimeValue: RuntimeValue as args) = stateChange {
        do! Set(PropertyAddress(rowId, propertyName), runtimeValue) |> Ribbit.ExecuteM
        }

    let hasValue (rowId, propertyName) (ribbit: RibbitData) =
        match ribbit.scope.rows with
        | Map.Lookup rowId (Map.Lookup propertyName _) -> true
        | _ -> false

    let update msg (ribbit: RibbitData) =
        match msg with
        | ReserveId id ->
            { ribbit with scope = { ribbit.scope with biggestIdSoFar = max id (defaultArg ribbit.scope.biggestIdSoFar 0) |> Some }}
        | AssociateMonsterKind(monsterKind, id) ->
            { ribbit with kindsOfMonsters = ribbit.kindsOfMonsters |> Map.add monsterKind id }
        | AssociateIndividual(personalName, id, monsterKind) ->
            let ribbit = { ribbit with roster = ribbit.roster |> Map.add personalName id }
            match monsterKind with
            | Some (monsterKind) ->
                match ribbit.categories |> Map.tryFind monsterKind with
                | None ->
                    { ribbit with categories = ribbit.categories |> Map.add monsterKind [id, personalName] }
                | Some existing ->
                    { ribbit with categories = ribbit.categories |> Map.add monsterKind ((id, personalName)::existing) }
            | None -> ribbit
        | RegisterRequest request -> notImpl()
        | Set(PropertyAddress(rowId, propertyName), value) ->
            let count = ribbit.scope.rows.Count
            let rows = ribbit.scope.rows |> Map.change rowId (function
                | Some row -> row |> Map.add propertyName value |> Some
                | None -> Map.ofList [propertyName, value] |> Some
                )
            { ribbit with scope = { ribbit.scope with rows = rows } }
        | Set(address, value) -> notImpl()
        | SetRoster(roster) -> { ribbit with roster = roster }

open Ops

type Ribbit with
    static member Update msg (Ribbit ribbit) =
        Delta.execute msg ribbit |> Ribbit
    static member UpdateM msg (Ribbit ribbit) =
        (), Delta.execute msg ribbit |> Ribbit
    static member GetM f (ribbit: Ribbit) =
        f ribbit.data, ribbit
    static member Fresh = Delta.create((fun () -> RibbitData.fresh), update) |> Ribbit

type NumberProperty(name, defaultValue: _ option) =
    inherit Property<int>(name, RuntimeType.Number)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Number value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Number value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Number n -> Ok n | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Number n -> Ok n | v -> castFailure RuntimeType.Number rowId name v)
    new(name, defaultValue: int) = NumberProperty(name, Some defaultValue)
    new(name) = NumberProperty(name, None)

type BoolProperty(name, defaultValue: _ option) =
    inherit Property<bool>(name, RuntimeType.Bool)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Bool value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Bool value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Bool b -> Ok b | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Bool b -> Ok b | v -> castFailure RuntimeType.Bool rowId name v)
    new(name, defaultValue: bool) = BoolProperty(name, Some defaultValue)
    new(name) = BoolProperty(name, None)

type RollProperty(name, defaultValue) =
    inherit Property<RollSpec>(name, RuntimeType.Roll)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Roll value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Roll value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Roll r -> Ok r | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Roll r -> Ok r | v -> castFailure RuntimeType.Roll rowId name v)
    new(name, defaultValue: RollSpec) = RollProperty(name, Some defaultValue)
    new(name) = RollProperty(name, None)

type RollsProperty(name, defaultValue) =
    inherit Property<RollSpec list>(name, RuntimeType.Rolls)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Rolls value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Rolls value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Rolls rs -> Ok rs | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Rolls rs -> Ok rs | v -> castFailure RuntimeType.Rolls rowId name v)
    new(name, defaultValue: RollSpec list) = RollsProperty(name, Some defaultValue)
    new(name) = RollsProperty(name, None)

type FlagsProperty<'t>(name, defaultValue: string Set) =
    inherit Property<string Set>(name, RuntimeType.Flags)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Flags value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Flags value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, Some defaultValue, function Flags f -> Ok f | _ -> Ok defaultValue) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, Some defaultValue, function Flags b -> Ok b | v -> castFailure RuntimeType.Flags rowId name v)
    member this.SetAll(rowId, value) = (this :> Property<string Set>).Set(rowId, value)
    member this.SetFlag(rowId, targetFlag:'t, value) (state: Ribbit) =
        let target = targetFlag.ToString()
        state |> upsertScope (rowId, name, Flags defaultValue, function
            | (Some (Flags set)) ->
                if value = (set.Contains target) then Flags set
                elif value then set.Add target |> Flags
                else set |> Set.filter ((<>) target) |> Flags
            | _ -> (if value then [target] else []) |> Set.ofList |> Flags
            )
    member this.SetAllM(rowId, value) (state: Ribbit) = (), this.SetAll(rowId, value) state
    member this.SetFlagM(rowId, targetFlag, value) (state: Ribbit) = (), this.SetFlag(rowId, targetFlag, value) state
    member this.Check(rowId, targetFlag:'t) (ribbit: RibbitData) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        match ribbit |> getSynchronously (rowId, name, (Some false), function Flags flags -> check flags |> Ok | v -> castFailure RuntimeType.Flags rowId name v) with
        | Ok v -> v
        | Error err -> shouldntHappen err
    member this.CheckM(rowId, targetFlag) (ribbit: RibbitData) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        ribbit |> getAsync (rowId, name, (Some false), function Flags flags -> check flags |> Ok | v -> castFailure RuntimeType.Flags rowId name v)
    new(name) = FlagsProperty(name, Set.empty)

type IdProperty(name, defaultValue) =
    inherit Property<Id>(name, RuntimeType.Id)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Id value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Id value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Id id -> Ok id | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Id id -> Ok id | v -> castFailure RuntimeType.Id rowId name v)
    new(name, defaultValue: int) = IdProperty(name, Some defaultValue)
    new(name) = IdProperty(name, None)

type TextProperty(name, defaultValue) =
    inherit Property<string>(name, RuntimeType.Text)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> setState (rowId, name, Text value) |> snd
    override this.SetM(rowId, value) = setState (rowId, name, Text value)
    override this.Get(rowId) (ribbit: RibbitData) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Text t -> Ok t | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Text t -> Ok t | v -> castFailure RuntimeType.Text rowId name v)
    new(name, defaultValue: string) = TextProperty(name, Some defaultValue)
    new(name) = TextProperty(name, None)

type BinaryExpression<'t, 'lhs, 'rhs>(lhs: 'lhs Expression, rhs: 'rhs Expression, f: 'lhs -> 'rhs -> Result<'t, RibbitError>) =
    inherit Expression<'t>()
    override this.Eval (ctx: EvaluationContext) =
        lhs.Eval ctx |> Result.bind (fun lhs ->
            rhs.Eval ctx |> Result.bind (fun rhs ->
                f lhs rhs))

type PropertyExpression<'t>(rowId: Expression<Id>, prop: Property<'t>) =
    inherit Expression<'t>()
    override this.Eval (ctx: EvaluationContext) =
        rowId.Eval ctx |> Result.bind (fun rowId -> prop.GetM rowId ctx)

//type LocalExpression<'t>(paramName: string, prop: Property<'t>) =
//    inherit Expression<'t>()
//    override this.Eval (ctx: EvaluationContext) =
//        match ctx.locals |> Map.tryFind paramName with
//        | Some v -> Ok v
//        | _ ->
//            notImpl()
            //let req = DataRequest(LocalAddress(), )
            //Error (Awaiting req)

type UpcastExpression<'t>(inner: Expression<'t>, upcast': 't -> RuntimeValue) =
    inherit Expression<RuntimeValue>()
    override this.Eval (ctx: EvaluationContext) =
        inner.Eval ctx |> Result.map (fun inner -> upcast' inner)

module Ops2 =
    let numberAssignment (name: Name, expr: int Expression) =
        Assign(LocalAddress name, UpcastExpression(expr, Number))
