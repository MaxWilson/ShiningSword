module CQRS

type Marker = int

type CQRS<'msg, 'state>(initialState: 'state, execute) =
    let mutable state = initialState
    let mutable stateHistoryRev = [0, (None, state)]
    let mutable stateFuturesRev = []
    let currentMarker = stateHistoryRev.Length
    member _.State = state
    member this.Execute(msg: 'msg): unit =
        let state' = execute msg state
        state <- state'
        let marker = (stateHistoryRev |> List.tryHead |> function Some (m, _) -> m + 1 | None -> 0)
        stateHistoryRev <- (marker,(Some msg, state'))::stateHistoryRev
        stateFuturesRev <- []
    member _.Checkpoint(): Marker =
        stateHistoryRev.Head |> fst
    member this.Rewind(mark: Marker): unit =
        match stateHistoryRev |> List.tryFindIndex (fun (marker, _) -> marker = mark) with
        | Some ix ->
            let removal = stateHistoryRev[..ix-1]
            stateHistoryRev <- stateHistoryRev[ix..]
            stateFuturesRev <- removal@stateFuturesRev
            state <- stateHistoryRev.Head |> (snd >> snd)
        | None -> failwith $"Invalid marker: {mark}"
    member this.FastForward(mark: Marker): unit =
        match stateFuturesRev |> List.tryFindIndex (fun (marker, _) -> marker = mark) with
        | Some ix ->
            let removal = stateFuturesRev[..ix-1]
            stateFuturesRev <- stateFuturesRev[ix..]
            stateHistoryRev <- removal@stateHistoryRev
            state <- stateFuturesRev.Head |> (snd >> snd)
        | None -> failwith $"Invalid marker: {mark}"
    member this.Log(): 'state list = stateHistoryRev |> List.map (snd >> snd) |> List.rev
    member this.LogWithMessages() : List<'msg option * 'state> = stateHistoryRev |> List.rev |> List.map snd
    static member Create(st, f) : CQRS<_,_> = CQRS(st, f)


