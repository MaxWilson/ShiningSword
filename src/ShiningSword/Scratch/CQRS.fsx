#if INTERACTIVE
#r "nuget: Unquote"
#else
module CQRS
#endif
open System
open Swensen.Unquote

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

let tests() =
    let test1 = EventSource.Create(0, fun f n -> f n)
    test <@ test1.State = 0 @>
    let zeroish = test1.Checkpoint()
    test1.Execute ((+) 3)
    test <@ test1.State = 3 @>
    let threeish= test1.Checkpoint()
    test1.Execute ((*) 2)
    test <@ test1.State = 6 @>
    let sixish = test1.Checkpoint()
    test1.Rewind threeish
    test <@ test1.State = 3 @>
    test1.Rewind zeroish
    test <@ test1.State = 0 @>
    test1.FastForward sixish
    test <@ test1.State = 6 @>
    test1.Rewind threeish
    test1.Rewind threeish // should be idempotent
    test <@ test1.State = 3 @>
    test1.Execute ((+) 4)
    test1.Execute ((-) 15)
    test <@ test1.State = 8 @>
    raises <@ test1.FastForward sixish @> // should fail
#if INTERACTIVE
tests()
#endif
