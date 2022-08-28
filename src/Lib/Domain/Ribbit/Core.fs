namespace Domain.Ribbit

open Domain
open Domain.Random
open Domain.Character
open Delta

[<AutoOpen>]
module Core =
    type RibbitMsg =
        | ReserveId of Id
        | AssociateMonsterKind of monsterKind:Name * Id
        | AssociateIndividual of personalName:Name * id: Id * monsterKind: Name option
        | RegisterRequest of RibbitRequest
        | Set of Address * value: RuntimeValue
        | SetRoster of Map<Name, Id>

    /// Ribbit operations that are about more than just data, such as logging and storing rosters
    [<AutoOpen>]
    module Composition =
        type LogCategory = Good | Bad | Neither
        type LogEntry = { msg: string; important: bool; category: LogCategory }
            with
            static member create msg = { msg = msg; important = false; category = Neither }
            static member create(msg, important, category) = { msg = msg; important = important; category = category }

    type 'Ribbit Affordance = {
        name: Name
        action: 'Ribbit Execution
        }
            
    type RibbitData = {
        properties: PropertiesByType // used primarily in parsing and serialization scenarios. Otherwise use props directly via object references.
        kindsOfMonsters: Map<Name, Id> // used for looking up kinds if necessary
        roster: Map<Name, Id> // actual creatures, by specific name like "Orc #1" or "Ugly Orc"
        categories: Map<Name, (Id*Name) list> // mapping from king like "Orc" to specific name. Used for generating unique specific names.
        scope: Scope
        affordances: Map<Name, Ribbit Affordance>
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
        static member Update msg (Ribbit ribbit) = Delta.execute msg ribbit |> Ribbit
        member this.update msg = Ribbit.Update msg this
        static member UpdateM msg (Ribbit ribbit) = (), Delta.execute msg ribbit |> Ribbit
        static member GetM f (ribbit: Ribbit) = f ribbit, ribbit
        member this.delta = match this with Ribbit(data) -> data
        member this.data = match this with Ribbit(data) -> data |> Delta.deref
        member this.transform f = match this with Ribbit(data) -> Ribbit (f data)
        member this.transform stateChange = (stateChange |> runNoResult this)
        member this.transformM stateChange = (), match this with Ribbit(data) -> (stateChange |> runNoResult data |> Ribbit)
        member this.transformM stateChange = (), (stateChange |> runNoResult this |> Ribbit)
    and PropertiesByType = {
        number: Map<Name, Property<int, Ribbit>>
        id: Map<Name, Property<Id, Ribbit>>
        roll: Map<Name, Property<RollSpec, Ribbit>>
        rolls: Map<Name, Property<RollSpec list, Ribbit>>
        flags: Map<Name, Property<string Set, Ribbit>>
        bool: Map<Name, Property<bool, Ribbit>>
        }
        with static member fresh = { number = Map.empty; id = Map.empty; roll = Map.empty; rolls = Map.empty; flags = Map.empty; bool = Map.empty }


    and [<AbstractClass>]
        Expression<'t>() =
        abstract Eval: 'Ribbit EvaluationContext -> 't RValue // expressions CANNOT modify state or the evaluation context variables, they can only succeed or fail.
    
    and Statements = Sequence of Statement list | While of Expression<bool> * Statements
    and CompiledStatements = Statement array
    and Statement =
        | Assign of Address * Expression<RuntimeValue>
        | Jump of int

    type FightResult = Victory | Defeat | Ongoing
    type RoundResult = { outcome: FightResult; msgs: LogEntry list; ribbit: Ribbit }

    let getRibbit (ctx: Ribbit EvaluationContext) = ctx.ribbit

    let withEvaluation (f: Evaluation<_,_>) (ctx: Ribbit ExecutionContext) =
        let ribbit = ctx.state
        (f { ribbit = ribbit; locals = ctx.locals }), ctx

    let castFailure (runtimeType: RuntimeType) id propName (v: RuntimeValue) =
        BugReport $"row #{id} property {propName} was should be a {runtimeType} but was actually {v}" |> Error
    // registerRequest adds a request to the list of things a user will be requested to fill. Associating
    // a request with logic to retry will happen separately and later, when the caller fails to synchronously
    // receive a value.
    let registerRequest request = fun state -> (), state |> Delta.execute (RegisterRequest request)

    let private _get (rowId: Id, propertyName: Name, fallback, getter) (ribbit: Ribbit) =
        let rec recur rowId' =
            match ribbit.data.scope.rows with
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

    let getAsync<'t> (rowId, propertyName, defaultValue: 't option, castRuntimeValue) (ribbit: Ribbit) =
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
        let! currentValue = Ribbit.GetM (_get (rowId, propertyName, thunk None, Some))
        do! Set(PropertyAddress (rowId, propertyName), upsert currentValue) |> Ribbit.ExecuteM
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

    type Ribbit with
        static member Fresh = Delta.create((fun () -> RibbitData.fresh), update) |> Ribbit

type NumberProperty(name, defaultValue: _ option) =
    inherit Property<int, Ribbit>(name, RuntimeType.Number)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Number value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Number n -> Ok n | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Number n -> Ok n | v -> castFailure RuntimeType.Number rowId name v)
    new(name, defaultValue: int) = NumberProperty(name, Some defaultValue)
    new(name) = NumberProperty(name, None)

type BoolProperty(name, defaultValue: _ option) =
    inherit Property<bool, Ribbit>(name, RuntimeType.Bool)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Bool value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Bool b -> Ok b | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Bool b -> Ok b | v -> castFailure RuntimeType.Bool rowId name v)
    new(name, defaultValue: bool) = BoolProperty(name, Some defaultValue)
    new(name) = BoolProperty(name, None)

type RollProperty(name, defaultValue) =
    inherit Property<RollSpec, Ribbit>(name, RuntimeType.Roll)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Roll value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Roll r -> Ok r | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Roll r -> Ok r | v -> castFailure RuntimeType.Roll rowId name v)
    new(name, defaultValue: RollSpec) = RollProperty(name, Some defaultValue)
    new(name) = RollProperty(name, None)

type RollsProperty(name, defaultValue) =
    inherit Property<RollSpec list, Ribbit>(name, RuntimeType.Rolls)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Rolls value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Rolls rs -> Ok rs | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Rolls rs -> Ok rs | v -> castFailure RuntimeType.Rolls rowId name v)
    new(name, defaultValue: RollSpec list) = RollsProperty(name, Some defaultValue)
    new(name) = RollsProperty(name, None)

type FlagsProperty<'t>(name, defaultValue: string Set) =
    inherit Property<string Set, Ribbit>(name, RuntimeType.Flags)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Flags value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, Some defaultValue, function Flags f -> Ok f | _ -> Ok defaultValue) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, Some defaultValue, function Flags b -> Ok b | v -> castFailure RuntimeType.Flags rowId name v)
    member this.SetAll(rowId, value) = (this :> Property<string Set, Ribbit>).Set(rowId, value)
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
    member this.Check(rowId, targetFlag:'t) (ribbit: Ribbit) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        match ribbit |> getSynchronously (rowId, name, (Some false), function Flags flags -> check flags |> Ok | v -> castFailure RuntimeType.Flags rowId name v) with
        | Ok v -> v
        | Error err -> shouldntHappen err
    member this.CheckM(rowId, targetFlag) (ribbit: Ribbit) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        ribbit |> getAsync (rowId, name, (Some false), function Flags flags -> check flags |> Ok | v -> castFailure RuntimeType.Flags rowId name v)
    new(name) = FlagsProperty(name, Set.empty)

type IdProperty(name, defaultValue) =
    inherit Property<Id, Ribbit>(name, RuntimeType.Id)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Id value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Id id -> Ok id | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Id id -> Ok id | v -> castFailure RuntimeType.Id rowId name v)
    new(name, defaultValue: int) = IdProperty(name, Some defaultValue)
    new(name) = IdProperty(name, None)

type TextProperty(name, defaultValue) =
    inherit Property<string, Ribbit>(name, RuntimeType.Text)
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Text value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, function Text t -> Ok t | v -> castFailure RuntimeType.Number rowId name v) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, function Text t -> Ok t | v -> castFailure RuntimeType.Text rowId name v)
    new(name, defaultValue: string) = TextProperty(name, Some defaultValue)
    new(name) = TextProperty(name, None)

//type BinaryExpression<'t, 'lhs, 'rhs>(lhs: 'lhs Expression, rhs: 'rhs Expression, f: 'lhs -> 'rhs -> Result<'t, RibbitError>) =
//    inherit Expression<'t>()
//    override this.Eval (ctx: Ribbit EvaluationContext) =
//        lhs.Eval ctx |> Result.bind (fun lhs ->
//            rhs.Eval ctx |> Result.bind (fun rhs ->
//                f lhs rhs))

//type PropertyExpression<'t>(rowId: Expression<Id>, prop: Property<'t>) =
//    inherit Expression<'t>()
//    override this.Eval (ctx: Ribbit EvaluationContext) =
//        rowId.Eval ctx |> Result.bind (fun rowId -> prop.GetM rowId ctx)

//type LocalExpression<'t>(paramName: string, prop: Property<'t>) =
//    inherit Expression<'t>()
//    override this.Eval (ctx: EvaluationContext) =
//        match ctx.locals |> Map.tryFind paramName with
//        | Some v -> Ok v
//        | _ ->
//            notImpl()
            //let req = DataRequest(LocalAddress(), )
            //Error (Awaiting req)

//type UpcastExpression<'t>(inner: Expression<'t>, upcast': 't -> RuntimeValue) =
//    inherit Expression<RuntimeValue>()
//    override this.Eval (ctx: Ribbit EvaluationContext) =
//        inner.Eval ctx |> Result.map (fun inner -> upcast' inner)

//module Ops2 =
//    let numberAssignment (name: Name, expr: int Expression) =
//        Assign(LocalAddress name, UpcastExpression(expr, Number))
