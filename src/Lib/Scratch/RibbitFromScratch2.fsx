module Rewrite

#I __SOURCE_DIRECTORY__
#I ".."
#load "Optics.fs"
#load "Common.fs"
open Optics
open type Optics.Operations
(*
Lessons learned from this POC:
Rolling needs a better interface. Maybe should be directly integrated a la data awaiting/supply. Is a roll a runtime value or not?
Log output can be clarified.
Argument propagation between events: need to store some kind of event chain, what are the parents of a given event, etc.
Passing around an API in lieu of Game class seems to be overkill, since they're intimately tied anyway.
   Instead, maybe just start with defining game, and then defining dereference, and going from there while using let rec
   as necessary.
The basic framework of using withState, explicitly triggering attacks, etc., seems to work well enough as a POC. Are we
   ready to transform this into an actual code module for running adventures? I'm not so sure about that but maybe we're
   close enough to try writing a defineCombat() function that uses it internally, and then a game loop that runs it.
   And of course there'd be no withState call--instead there'd be a game loop.

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

type Roll =
    | StaticBonus of n:int
    | Roll of n:int * d:int * rest: Roll option
    with
    static member create(n,d) = Roll(n,d,None)
    static member create(n,d,plus) = Roll(n,d,Some(StaticBonus plus))
    member this.eval() =
        match this with
        | StaticBonus n -> n
        | Roll(n,d,rest) ->
            let v = [for _ in 1..n -> rand d] |> List.sum
            match rest with
            | None -> v
            | Some r -> v + r.eval()
type Expression =
    | Const of RuntimeValue
    | Random of Roll
    | BinaryOp of Expression * Expression * BinaryOperator
    | Dereference of VariableReference
    | Native of f: (Expression -> Expression) * arg: Expression

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
    | Launch of VariableReference * eventName: Name * args: (Name * Expression) list
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
type DeferF<'state> = EventId -> Statement list -> VariableReference list -> 'state -> 'state

type CompleteF<'state> = EventId -> RuntimeValue -> 'state -> 'state
type SupplyF<'state> = ExecutionContext -> VariableReference -> RuntimeValue -> 'state -> ('state * EventId list)
type ResumeF<'state> = EventId -> 'state -> Statement list
type RunF<'state> = EventId -> 'state -> 'state
type StartByNameF<'state> = Name -> (Name * RuntimeValue) list -> 'state -> EventId * 'state
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
                        | result -> shouldntHappen()
                    binary(lhs, rhs, op) |> Ok
                | Error _ as lhs, Ok _ -> lhs
                | Ok _, (Error _ as rhs) -> rhs
                | Error lhs, Error rhs -> Error (lhs @ rhs)
            | Dereference ref ->
                api.dereference ctx ref state
            | Random r -> Ok(Number (r.eval()))
            | Native(f, arg) ->
                eval (f arg)
        eval expr

let execute
    (api: {|
            dereference: DereferenceF<'state>
            supply: SupplyF<'state>
            start: StartByNameF<'state>
        |})
    (ctx: ExecutionContext)
    (state: 'state)
    (statements: Statement list)
        : 'state * Statement list * CurrentExpressionValue * ExecutionContext =
        let evalApi = {| dereference = api.dereference |}
        let eval = evaluate evalApi
        let rec loop state ctx statements =
            match statements with
            | current::rest as stack ->
                match current with
                | Return expr ->
                    match eval ctx state expr with
                    | Ok v -> state, [], Ok v, ctx
                    | awaiting -> state, [current], awaiting, ctx
                | Launch(ref, eventName, args) ->
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
                    | Ok result -> shouldntHappen() // may need some "compile-time" checking to prevent this
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
             start: StartByNameF<'state>
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
                        {| dereference = api.dereference; supply = api.supply; start = api.start |}
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
            start: StartByNameF<'state>
        |})
    (agent: AgentId, property: PropertyName)
    (v: RuntimeValue)
    (state: 'state)
    =        
        let state, unblocked = api.supply ExecutionContext.fresh (DataRef(agent, property)) v state
        progressToFixedPoint
            {| dereference = api.dereference; defer = api.defer; resume = api.resume; supply = api.supply; start = api.start |}
            (state, { ExecutionContext.fresh with workQueue = unblocked })

type Scope = {
    properties: Map<Name, RuntimeValue>
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
            {| dereference = Game.dereference; defer = Game.defer; resume = Game.resume; supply = Game.supply; start = Game.startByName |}
            (g, { workQueue = [eventId]; currentEvent = Some eventId })
    static member define (eventName: Name) instructions (g:Game) =
        { g with
            eventDefinitions = g.eventDefinitions |> Map.add eventName { name = eventName; mandatoryParams = []; instructions = instructions }
            }
    static member start instructions (args: (Name * RuntimeValue) list) (g:Game) =
        let eventId, g = g.nextEventId, { g with nextEventId = g.nextEventId + 1 }
        let g =
            {
                g with
                    events = g.events |> Map.change eventId (function
                        | None -> Some(EventState { scope = { properties = Map.ofSeq args }; instructionStack = instructions })
                        | _ -> shouldntHappen()
                        )
            }
        eventId, (Game.runEvent eventId g)
    static member startByName (name: Name) (args: (Name * RuntimeValue) list) (g:Game) =
        match g.eventDefinitions |> Map.tryFind name with
        | Some def ->
            let eventId, g = Game.start def.instructions args g
            eventId, g
        | None ->
            RibbitRuntimeException $"No such event: '{name}'" |> raise
    static member defer eventId statements refs (g:Game) =
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
            | error -> error
        | IndirectEventRef(eventIdRef) ->
            match Game.dereference ctx (LocalRef eventIdRef) g with
            | Ok (Id eventId) ->
                Game.dereference ctx (EventRef eventId) g
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

let compile = id // no optimizations for now

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
                Launch(LocalRef "__event_1", "getShieldBonus", ["self", Const (Id bob)])
                Assign(LocalRef "ac", BinaryOp(Dereference(DataRef(bob, "AC")), Dereference(IndirectEventRef("__event_1")), Plus))
                Return (Dereference (LocalRef "ac"))
            ] [] |> compile)
    let api = {| supply = Game.supply; dereference = Game.dereference; defer = Game.defer;
                 resume = Game.resume; supply = Game.supply; start = Game.startByName |}
    do! transform (supply api (bob, "AC") (Number 18) >> supply api (bob, "sp") (Number 5))
    let! (g: Game) = get()
    return (g.events.[eventId])
})

type deref =
    static member prop (paramName: string) property = IndirectDataRef(paramName, property) |> Dereference
    static member local localName = LocalRef localName |> Dereference
    static member event localName = IndirectEventRef localName |> Dereference
open type deref

withState Game.fresh (state {
    let! bob = transform1 (Game.add "Bob")
    let! shrek = transform1 (Game.add "Ogre")
    let stats = [
        bob, "AC", Number 5
        shrek, "AC", Number 6
        bob, "THAC0", Number 16
        shrek, "THAC0", Number 15
        bob, "HP", Number (Roll.create(6,10,12).eval())
        shrek, "HP", Number (Roll.create(4,8).eval())
        ]

    let api = {| supply = Game.supply; dereference = Game.dereference; defer = Game.defer;
                 resume = Game.resume; supply = Game.supply; start = Game.startByName |}

    for (character, property, value) in stats do
        do! transform (supply api (character, property) value)
    let start eventName args =
        Launch(LocalRef ("_" + eventName), eventName, args)
    let assign localName v = Assign(LocalRef localName, v)
    do! transform (Game.define "attack" [
        assign "targetAC" (deref.prop "target" "AC")
        assign "THAC0" (deref.prop "actor" "THAC0")
        assign "targetRollNumber" (BinaryOp(deref.local "THAC0", deref.local "targetAC", Minus))
        start "roll" ["value", Roll.create(1,20) |> Random]
        If(BinaryOp(deref.event "_roll", deref.local"targetRollNumber", AtLeast),            
            start "hit" ["target", deref.local "target"; "actor", deref.local "actor"],
            Some(start "miss" ["target", deref.local "target"; "actor", deref.local "actor"])
            )
    ])
    do! transform (Game.define "roll" [
        Return (deref.local "value") // This is probably the wrong way to do it: the value was already evaluated at call-time, but we want it to be able to be generated by the user
    ])
    do! transform (Game.define "miss" [
        // nothing happens on a miss unless there are special circumstances
    ])
    do! transform (Game.define "hit" [
        start "roll" ["value", Roll.create(2,6) |> Random]
        start "takeDamage" ["self", deref.local "target"; "amount", deref.event "_roll"]
    ])
    do! transform (Game.define "takeDamage" [
        let hp = IndirectDataRef("self", "HP")
        Assign(hp, BinaryOp(Dereference hp, deref.local "amount", Minus))
    ])

    let mutable ongoing = true
    let unsafeEval id prop =
        transform1 (fun (state:Game) -> state.data[id].properties[prop], state)
    while ongoing do
        let id = Const << Id
        do! transform (Game.start [start "attack" ["actor", id bob; "target", id shrek]] [] >> snd)
        do! transform (Game.start [start "attack" ["actor", id shrek; "target", id bob]] [] >> snd)
        let! bobHP = unsafeEval bob "HP"
        let! shrekHP = unsafeEval shrek "HP"
        (printfn "BobHP: %A\nShrekHP: %A" bobHP shrekHP) |> ignore
        let isPositive = function (Number n) when n > 0 -> true | _ -> false
        ongoing <- (isPositive bobHP) && (isPositive shrekHP)
    return! (fun state -> state.events |> Seq.map (function KeyValue(id, EventResult r) -> (id, r) | KeyValue(id, EventState s) -> state.waitingEvents.Keys |> List.ofSeq |> List.filter (fun key -> state.waitingEvents.[key].Contains id) |> sprintf "Awaiting %A" |> fun v -> (id, String v)) |> Array.ofSeq, ())
})
