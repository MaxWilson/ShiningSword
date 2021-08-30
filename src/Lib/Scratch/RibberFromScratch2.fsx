module Rewrite

#I "."
#I ".."
#load "Optics.fs"
#load "Common.fs"
open Optics
open type Optics.Operations
(*
Concepts: Expression eval

*)

type AgentId = int
type EventId = int
type RuntimeValue = String of string | Number of int | Boolean of bool | Id of int | Undefined
type Name = string
type PropertyName = string
type VariableReference =
    | DataRef of agent: AgentId * property: PropertyName
    | LocalRef of name: Name
    | EventRef of event:EventId
    | IndirectDataRef of agentIdLocalRef: Name * property: PropertyName
    | IndirectEventRef of eventIdLocalRef: Name

type Expression =
    | Const of RuntimeValue
    | BinaryOp of Expression * Expression * BinaryOperator
    | Dereference of VariableReference
    | Roll of n:int * dSize: int * plus: int
    | StartEvent of eventName: Name * args: (Name * Expression) list

and BinaryOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Equals
    | AtLeast
    | AtMost

type Statement =
    | Return of Expression
    | Assign of VariableReference * Expression
    | Sequence of Statement list
    | If of test: Expression * andThen: Statement * orElse: Statement option
type CurrentExpressionValue = Result<RuntimeValue, VariableReference list>

type ExecutionContext =
    {
    workQueue: EventId list // LIFO queue for ease of implementation but it probably doesn't matter what the order is
    currentEvent: EventId option
    }
    with
    static member fresh = { workQueue = []; currentEvent = None }
    static member forEvent eventId = { ExecutionContext.fresh with currentEvent = Some eventId }

type DereferenceF<'state> = ExecutionContext -> VariableReference -> 'state -> CurrentExpressionValue
type DeferF<'state> = EventId -> Statement list -> VariableReference -> 'state -> 'state

type CompleteF<'state> = EventId -> RuntimeValue -> 'state -> 'state
type SupplyF<'state> = ExecutionContext -> VariableReference -> RuntimeValue -> 'state -> ('state * EventId list)
type ResumeF<'state> = EventId -> 'state -> Statement list
type RunF<'state> = EventId -> 'state -> 'state

type RibbitRuntimeException (msg: string) =
    inherit System.Exception(msg)

let describeId = (function None -> "None" | Some v -> v.ToString())

let evaluate
    (api: {|
            dereference: DereferenceF<'state>
        |})
    (ctx: ExecutionContext)
    (state: 'state)
    expr
    : CurrentExpressionValue =
        let rec eval : Expression -> CurrentExpressionValue=
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
    (api: {|
            dereference: DereferenceF<'state>
            supply: SupplyF<'state>
        |})
    (ctx: ExecutionContext)
    (state: 'state)
    (statements: Statement list)
        : 'state * Statement list * CurrentExpressionValue * ExecutionContext =
        let evalApi = {| dereference = api.dereference |}
        let eval = evaluate evalApi
        let rec loop state ctx = function
            | current::rest as stack ->
                match current with
                | Return expr ->
                    match eval ctx state expr with
                    | Ok v -> state, [], Ok v, ctx
                    | awaiting -> state, [current], awaiting, ctx
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
    (api: {|
             dereference: DereferenceF<'state>
             defer: DeferF<'state>
             resume: ResumeF<'state>
             supply: SupplyF<'state>
        |})
    (state: 'state, ctx: ExecutionContext) =
        let mutable queue = ctx.workQueue
        let mutable state = state
        while queue.IsEmpty |> not do
            match queue with
            | currentEvent::rest ->
                queue <- rest
                let statements = api.resume currentEvent state
                match execute
                        {| dereference = api.dereference; supply = api.supply |}
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
                    let mutable state'' = state'
                    for d in dependencies do
                        state'' <- api.defer currentEvent statements d state''
                    state <- state''
            | _ -> ()
        state

let query
    (api: {|
            placeholder: 'state -> VariableReference list
        |})
    (state: 'state) =
        notImpl()

let supply
    (api: {|
            supply: SupplyF<'state>
            dereference: DereferenceF<'state>
            defer: DeferF<'state>
            resume: ResumeF<'state>
            supply: SupplyF<'state>
        |})
    (agent: AgentId, property: PropertyName)
    (v: RuntimeValue)
    (state: 'state)
    =        
        let state, unblocked = api.supply ExecutionContext.fresh (DataRef(agent, property)) v state
        progressToFixedPoint
            {| dereference = api.dereference; defer = api.defer; resume = api.resume; supply = api.supply |}
            (state, { ExecutionContext.fresh with workQueue = unblocked })

type Scope = {
    properties: Map<PropertyName, RuntimeValue>
    }
type Event = EventResult of RuntimeValue | EventState of EventState
and EventState = { scope: Scope; instructionStack: Statement list }
and EventDefinition = { name: Name; mandatoryParams: PropertyName list; instructions: Statement list }

type Game = {
    roster: Map<string, AgentId list>
    rosterReverse: Map<AgentId, string>
    data: Map<AgentId, Scope>
    eventDefinitions: Map<Name, EventDefinition>
    events: Map<EventId, Event>
    nextAgentId: AgentId // an id generator
    nextEventId: EventId // an id generator
    waitingEvents: Map<VariableReference, Set<EventId>> // to support execution chaining
    dataDependencies: (AgentId*PropertyName) list // for display in UI. Local variables and event results can't be input by the user so don't need to go in this list.
    }
    with
    static member fresh = {
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
    static member add name (g:Game) =
        let id, g = g.nextAgentId, { g with nextAgentId = g.nextAgentId + 1 }
        id, { g with roster = g.roster |> Map.change name (function Some ids -> Some(id::ids) | None -> Some [id]); rosterReverse = g.rosterReverse |> Map.add id name }
    static member runEvent eventId (g:Game) =
        progressToFixedPoint
            {| dereference = Game.dereference; defer = Game.defer; resume = Game.resume; supply = Game.supply |}
            (g, { workQueue = [eventId]; currentEvent = Some eventId })
    static member define (eventName: Name) instructions (g:Game) =
        { g with
            eventDefinitions = g.eventDefinitions |> Map.add eventName { name = eventName; mandatoryParams = []; instructions = instructions }
            }
    static member start instructions (g:Game) =
        let eventId, g = g.nextEventId, { g with nextEventId = g.nextEventId + 1 }
        let g =
            {
                g with
                    events = g.events |> Map.change eventId (function
                        | None -> Some(EventState { scope = { properties = Map.empty }; instructionStack = instructions })
                        | _ -> shouldntHappen()
                        )
            }
        eventId, (Game.runEvent eventId g)
    static member defer eventId statements ref (g:Game) =
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
    static member set (ctx: ExecutionContext) (ref:VariableReference) (value:RuntimeValue) (g:Game) =
        match ref with
        | DataRef(agentId, propName) ->
            let data =
                g.data |> Map.change agentId (function
                | None -> { properties = Map.ofList [propName, value] } |> Some
                | Some scope -> { scope with properties = scope.properties |> Map.add propName value } |> Some)
            { g with data = data }
        | IndirectDataRef(agentIdRef, propName) ->
            match Game.dereference ctx (LocalRef agentIdRef) g with
            | Ok agentId ->
                match agentId with
                | Id agentId ->
                    Game.set ctx (DataRef(agentId, propName)) value g
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

    static member supply (ctx: ExecutionContext) (ref:VariableReference) (value:RuntimeValue) (g:Game) =
        match g.waitingEvents |> Map.tryFind ref with
        | None ->
            Game.set ctx ref value g, []
        | Some dependencies ->
            let g = { g with waitingEvents = g.waitingEvents |> Map.remove ref }
            let g =
                match ref with
                | DataRef(agentId, propName) ->
                    let dd = g.dataDependencies |> List.filter ((<>) (agentId, propName))
                    { g with dataDependencies = dd }
                | _ -> g
            Game.set ctx ref value g, dependencies |> List.ofSeq
    static member resume (eventId: EventId) (g:Game) =
        match g.events |> Map.tryFind eventId with
        | Some (EventState state) ->
            state.instructionStack
        | _ -> shouldntHappen()
    static member dereference (ctx: ExecutionContext) (ref:VariableReference) (g:Game) =
        match ref with
        | DataRef(agentId, propName) ->
            match g.data |> Map.tryFind agentId with
            | Some scope ->
                match scope.properties |> Map.tryFind propName with
                | Some v -> Ok v
                | _ -> Error [ref]
            | _ -> Error [ref]
        | IndirectDataRef(agentIdRef, propName) ->
            match Game.dereference ctx (LocalRef agentIdRef) g with
            | Ok (Id agentId) ->
                Game.dereference ctx (DataRef(agentId, propName)) g
            | Ok agentId -> RibbitRuntimeException $"{agentIdRef} ({agentId}) is not a valid agentId" |> raise
            | Error _ -> RibbitRuntimeException $"{agentIdRef} is not set on event #{ctx.currentEvent |> describeId}" |> raise
        | IndirectEventRef(eventIdRef) ->
            match Game.dereference ctx (LocalRef eventIdRef) g with
            | Ok (Id eventId) ->
                Game.dereference ctx (EventRef eventId) g
            | Ok eventId -> RibbitRuntimeException $"{eventId} is not a valid eventId" |> raise
            | Error _ -> RibbitRuntimeException $"{eventIdRef} is not set on event #{ctx.currentEvent |> describeId}" |> raise
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
            match ctx.currentEvent with
            | None -> shouldntHappen()
            | Some eventId ->
                match g.events |> Map.tryFind eventId with
                | Some (EventState state) ->
                    Error [ref]
                | Some (EventResult v) -> Ok v
                | None -> shouldntHappen()

let compile = id

withState Game.fresh (state {
    
    let! bob = transform1 (Game.add "Bob")
    do! transform (Game.define "getShieldBonus" [
        If(BinaryOp(Dereference(IndirectDataRef("self", "sp")), Const(Number(2)), AtLeast),
            Sequence [
                Assign(IndirectDataRef("self", "sp"),
                            BinaryOp(Dereference(IndirectDataRef("self", "sp")), Const(Number(2)), Minus))
                Return (Const (Number 5))
            ],
            Some(Return (Const (Number 0))))
    ])
    let! eventId =
        transform1 (
          Game.start
            [
                Assign(LocalRef "__event_1", StartEvent("getShieldBonus", ["self", Const (Id bob)]))
                Assign(LocalRef "ac", BinaryOp(Dereference(DataRef(bob, "AC")), Dereference(IndirectEventRef("__event_1")), Plus))
                Return (Dereference (LocalRef "ac"))
            ] |> compile)
    let api = {| supply = Game.supply; dereference = Game.dereference; defer = Game.defer;
                 resume = Game.resume; supply = Game.supply |}
    do! transform (supply api (bob, "AC") (Number 18) >> supply api (bob, "sp") (Number 5))
    let! (g: Game) = get()
    return (g.events.[eventId])
})
