module Domain.RuleEngine
open Domain.Model
open Domain.Model.Ribbit

module Logic =
    // Like Task.Continue or Async.Map
    let continueWith f logic =
        let rec continueWith logic state =
            match logic state with
            | state, Ready v ->
                f v state
            | state, Awaiting(demand, logic) ->
                state, Awaiting (demand, logic |> continueWith)
        continueWith logic

    let read id (prop: Prop<'t>) (state: State) =
        match state.data |> Map.tryFind (id, prop.name) with
        | Some (:? 't as v) -> Some v
        | _ -> None

    let demand (id, propName as key) logic state =
        match state.outstandingQueries |> Map.tryFind key with
        | None ->
            { state with outstandingQueries = state.outstandingQueries |> Map.add (id, propName) [logic] }
        | Some current ->
            { state with outstandingQueries = state.outstandingQueries |> Map.add (id, propName) (current@[logic]) }

    let addToQueue logic state =
        { state with workQueue = Queue.append logic state.workQueue }

    let fulfill (id, prop: Prop<'t>) (value: 't) state =
        let state = { state with data = state.data |> Map.add (id, prop.name) (box value) }
        match state.outstandingQueries |> Map.tryFind (id, prop.name) with
        | None | Some [] -> state
        | Some unblocked ->
            unblocked |> List.fold (flip addToQueue) ({ state with outstandingQueries = state.outstandingQueries |> Map.remove (id, prop.name) })

    let andLog logic =
        logic |> continueWith (fun msg state -> { state with log = state.log @ [msg] }, Ready ())

    let processLogic = function
        | state, Ready () ->
            state
        | state, Awaiting(Some demand', logic) ->
            state |> demand demand' logic
        | state, Awaiting(None, logic) ->
            state |> addToQueue logic

    let spawn (logic: Logic<string>) state =
        (logic |> andLog) state |> processLogic

    let rec untilFixedPoint state =
        let queue = state.workQueue
        let state = { state with workQueue = Queue.empty }
        match queue |> List.fold (fun state logic -> logic state |> processLogic) state with
        | { workQueue = [] } as state -> state
        | state -> state |> untilFixedPoint

    module Builder =
        type Builder() =
            member _.Return x =
                fun state -> state, Ready x
            member _.ReturnFrom logic = logic
            member _.Bind (logic: Logic<'t>, rhs: 't -> Logic<'r>) : Logic<'r> =
                continueWith rhs logic

        let logic = Builder()
        let read id prop state =
            match read id prop state with
            | Some v -> state, Ready v
            | None ->
                let actualRead state =
                    let value = read id prop state |> Option.get
                    state, Ready value
                state, Awaiting(Some (id, prop.name), actualRead)
