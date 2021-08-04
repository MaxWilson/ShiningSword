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

type RuntimeValue = String of string | Number of int | Boolean of bool | Undefined
type AgentId = int
type EventId = int
type PropertyName = string
type VariableReference = DataRef of agent: AgentId * property: PropertyName | LocalRef of guid: string | EventRef of event:EventId

type Expression =
    | Const of RuntimeValue
    | BinaryOp of Expression * Expression * BinaryOperator
    | Conditional of test: Expression * andThen: Expression * orElse: Expression option
    | Dereference of VariableReference
    | Roll of n:int * dSize: int * plus: int
    | StartEvent of eventName: string * args: (PropertyName * Expression) list

and BinaryOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Equals
    | AtLeast
    | AtMost

type Statement =
    | Eval of Expression
    | Sequence of Statement list
type CurrentExpressionValue = Result<RuntimeValue, VariableReference list>

let evaluate
    (api: {|
            dereference: VariableReference -> 'state -> CurrentExpressionValue;
            progress: 'state -> 'state
        |})
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
            | Conditional(test, andThen, orElse) ->
                match eval test with
                | Ok (Boolean true) -> eval andThen
                | Ok (Boolean false) ->
                    match orElse with
                    | Some expr -> eval expr
                    | _ -> Ok Undefined
                | Ok _ -> Ok Undefined
                | err -> err
            | Dereference ref ->
                api.dereference ref state
            | Roll _ | StartEvent _ ->
                // By evaluation time, Roll and StartEvent should have already been rewritten to Dereference expressions
                Ok Undefined 
        eval expr

type ExecutionContext =
    {
    workQueue: EventId list // LIFO queue for ease of implementation but it probably doesn't matter what the order is
    currentEvent: EventId option
    }

let execute
    (api: {|
            dereference: VariableReference -> 'state -> CurrentExpressionValue
        |})
    (state: 'state)
    statements
        : 'state * Statement list * CurrentExpressionValue =
        notImpl()

type DeferF<'state> = EventId -> Statement list -> VariableReference -> 'state -> 'state
type CompleteF<'state> = EventId -> RuntimeValue -> 'state -> 'state
type SupplyF<'state> = ExecutionContext -> VariableReference -> RuntimeValue -> 'state -> ('state * EventId list)
type ResumeF<'state> = EventId -> 'state -> Statement list

let progressToFixedPoint
    (api: {|
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
                match execute ({| dereference = fun _ -> notImpl() |}) state statements with
                | state', _, Ok v ->
                    match api.supply { ExecutionContext.currentEvent = Some currentEvent; ExecutionContext.workQueue = [] } (EventRef currentEvent) v state' with
                    | state', unblockedWorkItems ->
                        state <- state'
                        queue <- unblockedWorkItems @ queue
                | state', statements, Error dependencies ->
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
            data_: (AgentId * PropertyName) -> Lens<'state, RuntimeValue>
            query: 'state -> VariableReference list
        |})
    (agent: AgentId, property: PropertyName)
    (v: RuntimeValue)
    (state: 'state)
    =
        let satisfies = api.query state
        let lens = api.data_ (agent, property)
        let state = state |> write lens v
        state

type Scope = {
    properties: Map<PropertyName, RuntimeValue>
    }
type Event = EventResult of RuntimeValue | EventState of EventState
and EventState = { scope: Scope; instructionStack: Statement list; dependencies: Set<VariableReference> }

type Game = {
    roster: Map<string, AgentId list>
    rosterReverse: Map<AgentId, string>
    data: Map<AgentId, Scope>
    events: Map<EventId, Event>
    nextEventId: EventId // an id generator
    dependencies: Map<VariableReference, EventId list> // to support execution chaining
    dataDependencies: (AgentId*PropertyName) list // for display in UI. Local variables and event results can't be input by the user so don't need to go in this list.
    }
    with
    static member start instructions (g:Game) =
        let eventId, g = g.nextEventId, { g with nextEventId = g.nextEventId + 1 }
        {
            g with
                events = g.events |> Map.change eventId (function
                    | None -> Some(EventState { scope = { properties = Map.empty }; instructionStack = instructions; dependencies = Set.empty })
                    | _ -> shouldntHappen()
                    )
        }
    static member defer eventId statements ref (g:Game) =
        {
            g with
                events = g.events |> Map.change eventId (function
                    | Some(EventState(state)) -> Some(EventState { state with dependencies = state.dependencies |> Set.add ref; instructionStack = statements })
                    | _ -> shouldntHappen()
                    )
                dependencies = g.dependencies |> Map.change ref (function
                    | Some(lst) -> eventId::lst |> Some
                    | None -> Some [eventId]
                    )
                dataDependencies =
                    // add to dataDependencies if this is a new dependency, so it can be added to the UI
                    match ref with
                    | DataRef(agentId, propertyName) ->
                        if g.dependencies |> Map.containsKey ref then g.dataDependencies
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
        | LocalRef(propName) ->
            let events =
                match ctx.currentEvent with
                | None -> shouldntHappen()
                | Some eventId ->
                    g.events |> Map.change eventId (function
                    | Some (EventState state) ->
                        let scope = { state.scope with properties = state.scope.properties |> Map.add propName value }
                        Some (EventState { state with scope = scope })
                    | _ -> shouldntHappen()
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
        match g.dependencies |> Map.tryFind ref with
        | None ->
            Game.set ctx ref value g, []
        | Some dependencies ->
            let g =
                match ref with
                | DataRef(agentId, propName) ->
                    let dd = g.dataDependencies |> List.filter ((<>) (agentId, propName))
                    { g with dataDependencies = dd; dependencies = g.dependencies |> Map.remove ref }
                | _ -> { g with dependencies = g.dependencies |> Map.remove ref }
            Game.set ctx ref value g, dependencies
    static member resume (eventId: EventId) (g:Game) =
        match g.events |> Map.tryFind eventId with
        | Some (EventState state) ->
            state.instructionStack
        | _ -> shouldntHappen()

let foo(game: Game, id: EventId, ref:VariableReference) =
    progressToFixedPoint {| defer = Game.defer; resume = Game.resume; supply = Game.supply |} (game, { workQueue = []; currentEvent = None })
