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
        | ClearValue of RowId: Id * PropertyName: Name // clear a property value to prompt re-entry/re-calculation. Currently only used for current initiative. Allowing temporal scopes is probably a better solution, in which case this will turn from ClearValue(Address) to ClearScope(current round)
        | SetRoster of Map<Name, Id>
        | RemoveRosterEntry of Name
        | RenameRosterEntry of Name * newName: Name
        | AddLogEntry of trieIndex: int list * txt: string

    type RibbitData = {
        properties: PropertiesByType // used primarily in parsing and serialization scenarios. Otherwise use props directly via object references.
        kindsOfMonsters: Map<Name, Id> // used for looking up kinds if necessary
        roster: Map<Name, Id> // actual creatures, by specific name like "Orc #1" or "Ugly Orc"
        categories: Map<Name, (Id*Name) list> // mapping from king like "Orc" to specific name. Used for generating unique specific names.
        scope: Scope
        affordances: Map<Name, Ribbit Affordance>
        openRequests: RibbitError list // probably also need some way of knowing what to restart after a request is filled
        events: Map<Id, EventData>
        eventRoots: Id FastList.d // root events for display in the log; should generally map to affordances like choosing to attack, and not to things triggered by them like choosing to parry.
        }
        with
        static member fresh = { scope = Scope.fresh; kindsOfMonsters = Map.empty; roster = Map.empty; categories = Map.empty;
            affordances = Map.empty; properties = PropertiesByType.fresh; openRequests = []; eventRoots = FastList.fresh(); events = Map.empty
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
        member this.rewindTo ix = this.transform (Delta.rewind ix)

    and PropertiesByType = {
        number: Map<Name, Property<int, Ribbit>>
        id: Map<Name, Property<Id, Ribbit>>
        roll: Map<Name, Property<RollSpec, Ribbit>>
        rolls: Map<Name, Property<RollSpec list, Ribbit>>
        flags: Map<Name, Property<string Set, Ribbit>>
        bool: Map<Name, Property<bool, Ribbit>>
        }
        with static member fresh = { number = Map.empty; id = Map.empty; roll = Map.empty; rolls = Map.empty; flags = Map.empty; bool = Map.empty }

    and Expression<'t> = Evaluation<'t, Ribbit> // expressions CANNOT modify state or the evaluation context variables, they can only succeed or fail.

    and Statements = Sequence of Statement list | While of Expression<bool> * Statements
    and CompiledStatements = Statement array
    and Statement =
        | Assign of Address * Expression<RuntimeValue>
        | Jump of int
    and 'Ribbit Affordance = {
        name: Name
        action: 'Ribbit Execution
        }
    and LogCategory = Good | Bad | Neither
    and LogEntry = { msg: string; important: bool; category: LogCategory }
        with
        static member create msg = { msg = msg; important = false; category = Neither }
        static member create(msg, important, category) = { msg = msg; important = important; category = category }
    and EventData = {
        id: int // ribbit.data.events[ix].id should equal ix
        log: LogEntry option // will be None before the event gets resolved
        timeTravelIndex: int // the number of state change events which have occurred when event is resolved. Used for time travel
        returnValue: RuntimeValue option
        childEvents: Id list
        }

    type FightResult = Victory | Defeat | Ongoing
    type RoundResult = { outcome: FightResult; msgs: LogEntry list; ribbit: Ribbit }

    let getRibbit (ctx: Ribbit EvaluationContext) = ctx.ribbit

    let withEvaluation (f: Evaluation<_,_>) (ctx: Ribbit ExecutionContext) =
        let ribbit = ctx.state
        (f { ribbit = ribbit; locals = ctx.locals }), ctx

    // registerRequest adds a request to the list of things a user will be requested to fill. Associating
    // a request with logic to retry will happen separately and later, when the caller fails to synchronously
    // receive a value.
    let registerRequest request = fun state -> (), state |> Delta.execute (RegisterRequest request)

    let private (|Id|) = function Generic ix -> unbox<int> ix

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

    let update history msg (ribbit: RibbitData) =
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
            let rows = ribbit.scope.rows |> Map.change rowId (function
                | Some row -> row |> Map.add propertyName value |> Some
                | None -> Map.ofList [propertyName, value] |> Some
                )
            { ribbit with scope = { ribbit.scope with rows = rows } }
        | Set(address, value) -> notImpl()
        | SetRoster(roster) -> { ribbit with roster = roster }
        | RemoveRosterEntry name -> { ribbit with roster = ribbit.roster |> Map.filter (fun k v -> k <> name) }
        | RenameRosterEntry(name, name') ->
            match ribbit.roster |> Map.tryFind name with
            | Some id ->
                { ribbit with roster = ribbit.roster |> Map.remove name |> Map.add name' id }
            | None -> ribbit
        | ClearValue(rowId, propertyName) ->
            let rows = ribbit.scope.rows |> Map.change rowId (Option.map (Map.remove propertyName))
            { ribbit with scope = { ribbit.scope with rows = rows } }
        | AddLogEntry(trieIxs, txt) ->
            let id = ribbit.events.Count
            let logEntry = LogEntry.create txt
            let event = { EventData.id = id; log = Some logEntry; timeTravelIndex = history |> List.length; returnValue = None; childEvents = [] }
            match trieIxs with
            | [] -> // add at root level
                { ribbit with eventRoots = ribbit.eventRoots.Add(id); events = ribbit.events |> Map.add id event }
            | rootIndex::restIxs ->
                // first, find the parent event. Then append the log entry to it.
                let readEventChildren id =
                    id, Some ribbit.events[id].childEvents
                let parentEventId = ribbit.eventRoots.inOrder()[rootIndex] |> Trie.read readEventChildren restIxs
                match parentEventId with
                | None -> ribbit // invalid index is a noop
                | Some parentId ->
                    let parentEvent = ribbit.events[parentId]
                    { ribbit with events = ribbit.events |> Map.add id event |> Map.add parentId { parentEvent with childEvents = parentEvent.childEvents @ [id] } }

    type Ribbit with
        static member Fresh = Delta.create((fun () -> RibbitData.fresh), update) |> Ribbit

    let getLog ix (ribbit:Ribbit) = ribbit.data.events[ribbit.data.eventRoots[ix]].log
    let getLogMsg ix (ribbit:Ribbit) = ribbit.data.events[ribbit.data.eventRoots[ix]].log.Value.msg // convenience helper for the common case where log is guaranteed to be completed, as for unit tests

type GenericProperty<'t>(name, defaultValue: _ option, tryUnbox: obj -> 't option) =
    inherit Property<'t, Ribbit>(name)
    let extract = function Generic v -> Ok (unbox v)
    // default conversion function for convenience--99% of the time you should use this via the convenience ctors
    override this.Set(rowId, value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Generic value) |> Ribbit.Update)
    override this.Get(rowId) (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, extract) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM(rowId) =
        getRibbit >> getAsync (rowId, name, defaultValue, extract)
    // default conversion function for convenience--99% of the time you should use this via the convenience ctors
    static member TypeConvert = unbox<'t> >> Some
    new(name, defaultValue: 't) = GenericProperty<'t>(name, Some defaultValue, GenericProperty.TypeConvert)
    new(name) = GenericProperty<'t>(name, None, GenericProperty.TypeConvert)
    member this.Clear (rowId: Id) (ribbit: Ribbit) =
        ribbit |> (ClearValue(rowId, name) |> Ribbit.Update)

type GlobalGenericProperty<'t>(name, defaultValue: _ option, tryUnbox: obj -> 't option) =
    inherit GlobalProperty<'t, Ribbit>(name)
    let rowId = 0
    let extract = function Generic v -> Ok (unbox v)
    override this.Set(value) (state: Ribbit) =
        state |> (Set(PropertyAddress(rowId, name), Generic value) |> Ribbit.Update)
    override this.Get (ribbit: Ribbit) =
        match ribbit |> getSynchronously (rowId, name, defaultValue, extract) with
        | Ok value -> value
        | Error (BugReport msg) -> failwith msg // shouldn't use synchronous Get on a property that's lazy
        | Error _ -> shouldntHappen() // shouldn't use synchronous Get on a property that's lazy
    override this.GetM =
        getRibbit >> getAsync (rowId, name, defaultValue, extract)
    // default conversion function for convenience--99% of the time you should use this via the convenience ctors
    static member TypeConvert = unbox<'t> >> Some
    new(name, defaultValue: 't) = GlobalGenericProperty<'t>(name, Some defaultValue, GlobalGenericProperty.TypeConvert)
    new(name) = GlobalGenericProperty<'t>(name, None, GlobalGenericProperty.TypeConvert)
    member this.Clear (rowId: Id) (ribbit: Ribbit) =
        ribbit |> (ClearValue(rowId, name) |> Ribbit.Update)

type NumberProperty = GenericProperty<int>
type BoolProperty = GenericProperty<bool>
type RollProperty = GenericProperty<RollSpec>
type RollsProperty = GenericProperty<RollSpec list>
type IdProperty = GenericProperty<Id>
type TextProperty = GenericProperty<string>

type FlagsProperty<'t>(name, defaultValue: string Set) =
    inherit GenericProperty<string Set>(name, Some defaultValue, GenericProperty.TypeConvert)
    member this.SetAll(rowId, value) = this.Set(rowId, value)
    member this.SetFlag(rowId, targetFlag:'t, value) (state: Ribbit) =
        let target = targetFlag.ToString()
        let (|Flags|) = function Generic v -> unbox<string Set> v
        let toFlag = box >> Generic
        state |> upsertScope (rowId, name, Generic defaultValue, function
            | (Some (Flags set)) ->
                if value = (set.Contains target) then toFlag set
                elif value then set.Add target |> toFlag
                else set |> Set.filter ((<>) target) |> toFlag
            | _ -> (if value then [target] else []) |> Set.ofList |> toFlag
            )
    member this.SetAllM(rowId, value) (state: Ribbit) = (), this.SetAll(rowId, value) state
    member this.SetFlagM(rowId, targetFlag, value) (state: Ribbit) = (), this.SetFlag(rowId, targetFlag, value) state
    member this.Check(rowId, targetFlag:'t) (ribbit: Ribbit) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        let (|Flags|) = function Generic v -> unbox<string Set> v
        match ribbit |> getSynchronously (rowId, name, (Some false), function Flags flags -> check flags |> Ok) with
        | Ok v -> v
        | Error err -> shouldntHappen err
    member this.CheckM(rowId, targetFlag) (ribbit: Ribbit) =
        let target = targetFlag.ToString()
        let check = fun (set: string Set) ->
            set.Contains target
        let (|Flags|) = function Generic v -> unbox<string Set> v
        ribbit |> getAsync (rowId, name, (Some false), function Flags flags -> check flags |> Ok)
    new(name) = FlagsProperty(name, Set.empty)

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
