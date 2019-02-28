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
    module Value =
        let toString = function
            | Value.Number n -> n.ToString()
            | Value.Text v -> v
    module Property =
        let set id (propertyName: PropertyName) value (data:Data) =
            { data with properties = data.properties |> Map.add (id, propertyName.ToLowerInvariant()) value }
        let get id (propertyName: PropertyName) data =
            data.properties |> Map.tryFind (id, propertyName.ToLowerInvariant())
    module Expression =
        let text txt = Expression.Value(Value.Text txt)
    module Roster =
        let tryId id (roster: Roster) =
            (fst roster) |> Map.tryFind id
        let tryName (name:string) (roster: Roster) =
            (snd roster) |> Map.tryFind (name.ToLowerInvariant()) // name lookup is normalized in lowercase
        let empty = Map.empty, Map.empty
        let add name ((idLookup, nameLookup) as roster : Roster) : Result<Roster, string> =
            match tryName name roster with
            | Some v -> Error (sprintf "%s already exists" name)
            | None ->
                let newId =
                    let ids = fst roster |> Map.toSeq
                    if Seq.isEmpty ids then 1
                    else 1 + (ids |> Seq.map fst |> Seq.min)
                Ok((idLookup |> Map.add newId name), (nameLookup |> Map.add (name.ToLowerInvariant()) newId))

    let ldata = Lens.lens (fun (s:State) -> s.data) (fun v s -> { s with data = v })
    let lview = Lens.lens (fun (s:State) -> s.view) (fun v s -> { s with view = v })
    let llog f = Lens.lens (fun (s:Data) -> s.log) (fun v s -> { s with log = v }) f
    let lroster = Lens.lens (fun (s:Data) -> s.roster) (fun v s -> { s with roster = v })
    let lfinished f = Lens.lens (fun (s:ViewState) -> s.finished) (fun v s -> { s with finished = v }) f
    let logCmd (msg: string) = Log [Expression.text msg]
    let log (msg:string) (state:State) : State =
        state |> Lens.over (ldata << llog) (Log.log msg)
    let emptyView =
        {
            lastInput = None
            lastCommand = None
            lastOutput = None
            selected = None
            finished = false
            }
    let init() =
        {   data = {
                log = Log.empty
                properties = Map.empty
                roster = Roster.empty
                }
            view = emptyView
            }

