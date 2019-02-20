// In this file are functions (including lenses) which conceptually go along with Model.Types, but it's a separate file for readability
module Model.Functions
open Model.Types
open Common

module Log =
    open Model.Types.Log
    let empty = [], []
    let log msg (log: Data) : Data =
        match log with
        | buffer, log -> msg::buffer, log
    let logMany msgs (log:Data) =
        match log with
        | buffer, log -> msgs@buffer, log
    let flush (log:Data) : Data =
        match log with
        | buffer, (h::rest) -> [], (h@(List.rev buffer))::rest
        | buffer, [] -> [], [List.rev buffer]
    let advance (log:Data) : Data =
        match flush log with
        | _, rest -> [], []::rest
    let extract = flush >> snd >> List.rev
module Battle2 =
    open Model.Types.Battle2
    let ldata = Lens.lens (fun (s:State) -> s.data) (fun v s -> { s with data = v })
    let lview = Lens.lens (fun (s:State) -> s.view) (fun v s -> { s with view = v })
    let llog f = Lens.lens (fun (s:Data) -> s.log) (fun v s -> { s with log = v }) f
    let lfinished f = Lens.lens (fun (s:ViewState) -> s.finished) (fun v s -> { s with finished = v }) f
    let log (msg:string) (state:State) : State =
        state |> Lens.over (ldata << llog) (Log.log msg)
    let init() =
        {   data = {
                log = Log.empty
                properties = Map.empty
                }
            view = {
                lastInput = None
                lastCommand = None
                lastOutput = None
                selected = None
                finished = false
                }
            }
