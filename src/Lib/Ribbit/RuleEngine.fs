namespace Domain.Engine
module Ribbit0 =
    open Domain.Model.Ribbit0

    let describeId = (function None -> "None" | Some v -> v.ToString())

    let evaluate
        (api: Api<'state>)
        (ctx: ExecutionContext)
        (state: 'state)
        expr
        : CurrentExpressionValue =
            let rec eval : Expression -> CurrentExpressionValue =
                function
                | Const v -> Ok v
                | BinaryOp(lhs, rhs, op) ->
                    match eval lhs, eval rhs with
                    | Ok lhs, Ok rhs ->
                        let binary = function
                            | String lhs, String rhs, Plus -> lhs + rhs |> String
                            | Number lhs, Number rhs, Plus -> lhs + rhs |> Number
                            | Boolean lhs, Boolean rhs, Plus -> (lhs || rhs) |> Boolean
                            | Number lhs, Number rhs, Minus -> lhs - rhs |> Number
                            | Number lhs, Number rhs, Times -> lhs * rhs |> Number
                            | Boolean lhs, Boolean rhs, Times -> (lhs && rhs) |> Boolean
                            | Number lhs, Number rhs, Divide -> lhs / rhs |> Number
                            | Number lhs, Number rhs, Equals -> (lhs = rhs) |> Boolean
                            | String lhs, String rhs, Equals -> (lhs = rhs) |> Boolean
                            | Boolean lhs, Boolean rhs, Equals -> lhs = rhs |> Boolean
                            | Number lhs, Number rhs, AtLeast -> lhs >= rhs |> Boolean
                            | Number lhs, Number rhs, AtMost -> lhs <= rhs |> Boolean
                            | _ -> Undefined
                        binary(lhs, rhs, op) |> Ok
                    | Error _ as lhs, Ok _ -> lhs
                    | Ok _, (Error _ as rhs) -> rhs
                    | Error lhs, Error rhs -> Error (lhs @ rhs)
                | Dereference ref ->
                    api.dereference ctx ref state
                | Roll _ | StartEvent _ ->
                    // By evaluation time, Roll and StartEvent should have already been rewritten to Dereference expressions
                    Ok Undefined
            eval expr

    let execute
        (api: Api<'state>)
        (ctx: ExecutionContext)
        (state: 'state)
        (statements: Statement list)
            : 'state * Statement list * CurrentExpressionValue * ExecutionContext =
            let evalApi = {| dereference = api.dereference |}
            let eval = evaluate api
            let rec loop state ctx statements =
                match statements with
                | current::rest as stack ->
                    match current with
                    | Return expr ->
                        match eval ctx state expr with
                        | Ok v -> state, [], Ok v, ctx
                        | awaiting -> state, [current], awaiting, ctx
                    | Assign(ref, StartEvent(eventName, args)) ->
                        let args = args |> List.map (fun (name, expr) -> name, (eval ctx state expr))
                        let awaiting = args |> List.collect (function (_, Error awaiting) -> awaiting | _ -> [])
                        match awaiting with
                        | [] ->
                            let actualParameters = args |> List.map (function (name, Ok v) -> name, v | _ -> shouldntHappen())
                            let eventId, state = api.start eventName actualParameters state
                            loop state ctx (Assign(ref, Const (Id eventId))::rest)
                        | awaiting ->
                            state, stack, Error awaiting, ctx
                    | Assign(ref, expr) ->
                        match eval ctx state expr with
                        | Ok v ->
                            let state, unblocked = api.supply ctx ref v state
                            let ctx = { ctx with workQueue = unblocked @ ctx.workQueue }
                            loop state ctx rest
                        | awaiting -> state, stack, awaiting, ctx
                    | Sequence statements ->
                        // unpack the statements onto the head of the instruction stack
                        loop state ctx (statements @ rest)
                    | If(test, andThen, orElse) ->
                        match eval ctx state test with
                        | Ok (Boolean true) -> loop state ctx (andThen :: rest)
                        | Ok (Boolean false) ->
                            match orElse with
                            | Some expr -> loop state ctx (expr :: rest)
                            | _ -> loop state ctx rest
                        | Ok _ -> shouldntHappen() // may need some "compile-time" checking to prevent this
                        | awaiting -> state, stack, awaiting, ctx
                | [] ->
                    state, [], Ok Undefined, ctx // returning a value from an event is optional, could just be assignments
            loop state ctx statements

    let progressToFixedPoint
        (api: Api<'state>)
        (state: 'state, ctx: ExecutionContext) =
            let mutable queue = ctx.workQueue
            let mutable state = state
            while queue.IsEmpty |> not do
                match queue with
                | currentEvent::rest ->
                    queue <- rest
                    let statements = api.resume currentEvent state
                    match execute
                            api
                            { currentEvent = Some currentEvent; workQueue = [] }
                            state
                            statements
                          with
                    | state', _, Ok v, ctx ->
                        match api.supply { ExecutionContext.currentEvent = Some currentEvent; ExecutionContext.workQueue = [] } (EventRef currentEvent) v state' with
                        | state', unblockedWorkItems ->
                            state <- state'
                            queue <- ctx.workQueue @ unblockedWorkItems @ queue
                    | state', statements, Error dependencies, ctx ->
                        state <- api.defer currentEvent statements dependencies state'
                | _ -> ()
            state

    let query
        (api: Api<'state>)
        (state: 'state) =
            notImpl()

    let supply
        (api: Api<'state>)
        (agent: AgentId, property: PropertyName)
        (v: RuntimeValue)
        (state: 'state)
        =
            let state, unblocked = api.supply ExecutionContext.fresh (DataRef(agent, property)) v state
            progressToFixedPoint
                api
                (state, { ExecutionContext.fresh with workQueue = unblocked })

    module Game =
        let fresh: Domain.Model.Ribbit0.Game = {
            roster = Map.empty
            rosterReverse = Map.empty
            data = Map.empty
            eventDefinitions = Map.empty
            events = Map.empty
            nextAgentId = 1
            nextEventId = 1
            waitingEvents = Map.empty
            dataDependencies = []
            }
        let add name (g:Game) =
            let id, g = g.nextAgentId, { g with nextAgentId = g.nextAgentId + 1 }
            id, { g with roster = g.roster |> Map.change name (function Some ids -> Some(id::ids) | None -> Some [id]); rosterReverse = g.rosterReverse |> Map.add id name }

        let rec dereference (ctx: ExecutionContext) (ref:VariableReference) (g:Game) =
            match ref with
            | DataRef(agentId, propName) ->
                match g.data |> Map.tryFind agentId with
                | Some scope ->
                    match scope.properties |> Map.tryFind propName with
                    | Some v -> Ok v
                    | _ -> Error [ref]
                | _ -> Error [ref]
            | IndirectDataRef(agentIdRef, propName) ->
                match dereference ctx (LocalRef agentIdRef) g with
                | Ok (Id agentId) ->
                    dereference ctx (DataRef(agentId, propName)) g
                | Ok agentId -> RibbitRuntimeException $"{agentIdRef} ({agentId}) is not a valid agentId" |> raise
                | error -> error
            | IndirectEventRef(eventIdRef) ->
                match dereference ctx (LocalRef eventIdRef) g with
                | Ok (Id eventId) ->
                    dereference ctx (EventRef eventId) g
                | Ok eventId -> RibbitRuntimeException $"{eventId} is not a valid eventId" |> raise
                | error -> error
            | LocalRef guid ->
                match ctx.currentEvent with
                | None -> shouldntHappen()
                | Some eventId ->
                    match g.events |> Map.tryFind eventId with
                    | Some (EventState state) ->
                        match state.scope.properties |> Map.tryFind guid with
                        | Some v -> Ok v
                        | _ -> Error [ref]
                    | _ -> shouldntHappen()
            | EventRef eventId ->
                match g.events |> Map.tryFind eventId with
                | Some (EventState state) ->
                    Error [ref]
                | Some (EventResult v) -> Ok v
                | None -> shouldntHappen()

        let define (eventName: Name) instructions (g:Game) =
            { g with
                eventDefinitions = g.eventDefinitions |> Map.add eventName { name = eventName; mandatoryParams = []; instructions = instructions }
                }
        let defer eventId statements refs (g:Game) =
            let updatedEvents =
                g.events |>
                    Map.change eventId (function
                        | Some(EventState state) ->
                            Some(EventState { state with instructionStack = statements })
                        | v -> v
                        )
            let updateRef g ref =
                {
                    g with
                        waitingEvents = g.waitingEvents |> Map.change ref (function
                            | Some(lst) -> lst |> Set.add eventId |> Some
                            | None -> Some (Set.ofList [eventId])
                            )
                        dataDependencies =
                            // add to dataDependencies if this is a new dependency, so it can be added to the UI
                            match ref with
                            | DataRef(agentId, propertyName) ->
                                if g.waitingEvents |> Map.containsKey ref then g.dataDependencies
                                else g.dataDependencies @ [agentId, propertyName]
                            | _ -> g.dataDependencies
                }
            refs |> List.fold updateRef { g with events = updatedEvents }
        let rec set (ctx: ExecutionContext) (ref:VariableReference) (value:RuntimeValue) (g:Game) =
            match ref with
            | DataRef(agentId, propName) ->
                let data =
                    g.data |> Map.change agentId (function
                    | None -> { properties = Map.ofList [propName, value] } |> Some
                    | Some scope -> { scope with properties = scope.properties |> Map.add propName value } |> Some)
                { g with data = data }
            | IndirectDataRef(agentIdRef, propName) ->
                match dereference ctx (LocalRef agentIdRef) g with
                | Ok agentId ->
                    match agentId with
                    | Id agentId ->
                        set ctx (DataRef(agentId, propName)) value g
                    | _ -> RibbitRuntimeException $"{agentIdRef} ({agentId}) is not a valid agentId" |> raise
                | error -> notImpl() // may
            | LocalRef(propName) ->
                let events =
                    match ctx.currentEvent with
                    | None -> shouldntHappen()
                    | Some eventId ->
                        g.events |> Map.change eventId (function
                        | Some (EventState state) ->
                            let scope = { state.scope with properties = state.scope.properties |> Map.add propName value }
                            Some (EventState { state with scope = scope })
                        | _ -> RibbitRuntimeException $"Can't set variable on an event that doesn't exist yet" |> raise
                        )
                { g with events = events }

            | EventRef(eventId) ->
                let events =
                    g.events |> Map.change eventId (function
                    | Some (EventState state) ->
                        Some (EventResult value)
                    | _ -> shouldntHappen()
                    )
                { g with events = events }
            | IndirectEventRef _ -> shouldntHappen()

        let supply (ctx: ExecutionContext) (ref:VariableReference) (value:RuntimeValue) (g:Game) =
            match g.waitingEvents |> Map.tryFind ref with
            | None ->
                set ctx ref value g, []
            | Some dependencies ->
                let g = { g with waitingEvents = g.waitingEvents |> Map.remove ref }
                let g =
                    match ref with
                    | DataRef(agentId, propName) ->
                        let dd = g.dataDependencies |> List.filter ((<>) (agentId, propName))
                        { g with dataDependencies = dd }
                    | _ -> g
                set ctx ref value g, dependencies |> List.ofSeq
        let resume (eventId: EventId) (g:Game) =
            match g.events |> Map.tryFind eventId with
            | Some (EventState state) ->
                state.instructionStack
            | _ -> shouldntHappen()

        let rec runEvent eventId (g:Game) =
            progressToFixedPoint
                api
                (g, { workQueue = [eventId]; currentEvent = Some eventId })

        and start instructions (args: (Name * RuntimeValue) list) (g:Game) =
            let eventId, g = g.nextEventId, { g with nextEventId = g.nextEventId + 1 }
            let g =
                {
                    g with
                        events = g.events |> Map.change eventId (function
                            | None -> Some(EventState { scope = { properties = Map.ofSeq args }; instructionStack = instructions })
                            | _ -> shouldntHappen()
                            )
                }
            eventId, (runEvent eventId g)

        and startByName (name: Name) (args: (Name * RuntimeValue) list) (g:Game) =
            match g.eventDefinitions |> Map.tryFind name with
            | Some def ->
                let eventId, g = start def.instructions args g
                eventId, g
            | None ->
                RibbitRuntimeException $"No such event: '{name}'" |> raise
        and api = { dereference = dereference; defer = defer; resume = resume; supply = supply; start = startByName }



    let compile = id

module Ribbit =
    open Domain.Model.Ribbit
    open Domain.Model.Ribbit.Prop
    let numberProp = prop isNumber
    let textProp = prop isText
    let resourceProp = prop isResource
    let prototypeProp = prop isId "Prototype"
    let update msg (state: InnerState) =
        let reserve (collection: ResizeArray<_>) (ix: int) =
            if ix >= collection.Count then
                collection.AddRange(Seq.init (1 + ix - collection.Count) (thunk None))
        match msg with
        | Set(EventProperty(EventId evid, PropertyName pid), v) ->
            reserve state.events evid
            let scope =
                match state.events[evid] with
                | None ->
                    let scope = Scope<string>()
                    state.events[evid] <- Some scope
                    scope
                | Some scope -> scope
            scope[pid] <- v
        | Set(CreatureProperty(CreatureId cid, PropertyName pid), v) ->
            reserve state.events cid
            let scope =
                match state.creatureData[cid] with
                | None ->
                    let scope = Scope<string>()
                    state.creatureData[cid] <- Some scope
                    scope
                | Some scope -> scope
            scope[pid] <- v
        state

    let read address (innerState:InnerState) =
        let deref = function
            | EventProperty(EventId evid, PropertyName pid) ->
                if evid < innerState.events.Count then
                    match innerState.events[evid] with
                    | None -> None
                    | Some scope ->
                        match scope.TryGetValue pid with
                        | true, v -> Some v
                        | false, _ -> None
                else
                    None
            | CreatureProperty(CreatureId cid, PropertyName pid) ->
                if cid < innerState.creatureData.Count then
                    match innerState.creatureData[cid] with
                    | None -> None
                    | Some scope ->
                        match scope.TryGetValue pid with
                        | true, v -> Some v
                        | false, _ -> None
                else
                    None
        match deref address with
        | Some v -> Some v
        | None ->
            let rec searchInParent (childAddress: Address) =
                let parent = childAddress.mapProperty prototypeProp.name |> deref
                match parent with
                | None -> None
                | Some (Id parentId) ->
                    let parentProp = childAddress.mapRow parentId
                    match deref parentProp with
                    | Some v -> Some v
                    | None -> searchInParent parentProp
                | Some _ -> shouldntHappen()
            searchInParent address
    let execute msg (state:State) = (), Stateful.execute msg state
    let readOrCreateAndRead address  = state {
        // this version of read will do something if read fails: ask the user, generate new values, etc.
        let! state = Stateful.deref
        match read address state with
        | Some v -> return v
        | None ->
            // attempt to generate a value
            let propName = address.propertyName
            match state.properties.TryGetValue propName with
            | false, _ ->
                return shouldntHappen()
            | true, prop ->
                match prop.fallbackBehavior with
                | AskUser ->
                    do! execute (AskFor address)
                    return notImpl()
                | Generate f ->
                    return notImpl()
                | Derive f ->
                    return notImpl()
        }
