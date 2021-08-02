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

let execute
    (api: {|
            dereference: VariableReference -> 'state -> CurrentExpressionValue;
            progress: 'state -> 'state
        |})
    (state: 'state)
    statements =
        notImpl()

type DeferF<'state> = EventId -> VariableReference -> 'state -> 'state

let resume
    (api: {|
            placeholder: 'state -> 'state
        |})
    (state: 'state) =
        notImpl()


let progress
    (api: {|
             defer: DeferF<'state>
        |})
    (state: 'state) =
        notImpl()

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
and EventState = { scope: Scope; instructionStack: Statement list; dependencies: VariableReference list }
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
                    | None -> Some(EventState { scope = { properties = Map.empty }; instructionStack = instructions; dependencies = [] })
                    | _ -> shouldntHappen()
                    )
        }
    static member defer eventId ref (g:Game) =
        {
            g with
                events = g.events |> Map.change eventId (function
                    | Some(EventState(state)) -> Some(EventState { state with dependencies = ref::state.dependencies })
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

let foo(game: Game, id: EventId, ref:VariableReference) =
    defer {| defer = Game.defer |} id ref game
