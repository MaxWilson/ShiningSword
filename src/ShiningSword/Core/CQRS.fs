module POC.CQRS // POC = not used by anything else yet except scripts

type Marker = int
type CQRS<'msg, 'state> =
    abstract member State : 'state
    abstract member Execute : 'msg -> unit
    abstract member Checkpoint : unit -> Marker
    abstract member Rewind : Marker -> unit
    abstract member FastForward : Marker -> unit

type EventSource<'msg, 'state>(initialState: 'state, execute) =
    let mutable state = initialState
    let mutable stateHistoryRev = [0, state]
    let mutable stateFuturesRev = []
    let currentMarker = stateHistoryRev.Length
    interface CQRS<'msg, 'state> with
        member _.State = state
        member this.Execute(msg: 'msg): unit =
            let state' = execute msg state
            state <- state'
            let marker = (stateHistoryRev |> List.tryHead |> function Some (m, _) -> m + 1 | None -> 0)
            stateHistoryRev <- (marker,state')::stateHistoryRev
            stateFuturesRev <- []
        member _.Checkpoint(): Marker =
            stateHistoryRev.Head |> fst
        member this.Rewind(mark: Marker): unit =
            match stateHistoryRev |> List.tryFindIndex (fun (marker, _) -> marker = mark) with
            | Some ix ->
                let removal = stateHistoryRev[..ix-1]
                stateHistoryRev <- stateHistoryRev[ix..]
                stateFuturesRev <- removal@stateFuturesRev
                state <- stateHistoryRev.Head |> snd
            | None -> failwith $"Invalid marker: {mark}"
        member this.FastForward(mark: Marker): unit =
            match stateFuturesRev |> List.tryFindIndex (fun (marker, _) -> marker = mark) with
            | Some ix ->
                let removal = stateFuturesRev[..ix-1]
                stateFuturesRev <- stateFuturesRev[ix..]
                stateHistoryRev <- removal@stateHistoryRev
                state <- stateFuturesRev.Head |> snd
            | None -> failwith $"Invalid marker: {mark}"
    static member Create(st, f) : CQRS<_,_> = EventSource(st, f)


