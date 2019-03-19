// In this file are functions (including lenses) which conceptually go along with Model.Types, but it's a separate file for readability
module Model.Functions
open Model.Types
open Common
open Common.Hierarchy

module Log =
    open Model.Types.Log
    let empty = [], []
    let log (msg:string) (log: Data<_>) : Data<_> =
        match log with
        | buffer, log -> ((), Leaf msg)::buffer, log
    let logDetailed logEntry (log : Data<_>) : Data<_> =
        match log with
        | buffer, log -> logEntry::buffer, log
    let logMany (msgs:string list) (log : Data<_>) =
        match log with
        | buffer, log -> (msgs |> List.map (fun x -> (), Leaf x))@buffer, log
    let flush (log : Data<_>) : Data<_> =
        match log with
        | buffer, (h::rest) -> [], (h@(List.rev buffer))::rest
        | buffer, [] -> [], [List.rev buffer]
    let advance (log : Data<_>) : Data<_> =
        match flush log with
        | _, rest -> [], []::rest
    let getText = (function Leaf txt | Nested(txt, _) -> txt)
    let extractEntries (x : Log.Data<_>) : Log.Entries<_> = x |> flush |> snd |> List.rev
    let mapEntries f (d: Log.Entries<_>) = List.map (List.map f) d
    let page nth entries =
        match nth with
        | None -> entries |> List.collect id
        | Some n ->
            let pageCount = entries.Length
            if betweenInclusive 1 pageCount n |> not then []
            elif n < 0 then // support negative indexing: -1 is last, -pageCount is first
                entries.[pageCount+n]
            else entries.[n-1]
    let truncateDetail detailLevel entries =
        let rec getEntry detailLevel = function
            | Nested(txt, children) ->
                if detailLevel > 0 then
                    Nested(txt, children |> List.map (getEntry (detailLevel - 1)))
                else
                    Leaf(txt)
            | chunk -> chunk
        entries |> mapEntries (Tuple.mapsnd (getEntry detailLevel))
    let getEntriesAsText x = x |> (extractEntries >> mapEntries (snd >> getText)) // code smell: this feels like not quite the right abstraction to expose

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
                    let ids = fst roster |> Map.toSeq |> Array.ofSeq // workaround for Fable bug: use Array instead of seq, because Seq.isEmpty sometimes incorrectly returns false the first time it is called on ids
                    if Array.isEmpty ids then 1
                    else
                        let biggest = Seq.maxBy fst ids
                        1 + fst biggest
                Ok((idLookup |> Map.add newId name), (nameLookup |> Map.add (name.ToLowerInvariant()) newId))

    let ldata = Lens.lens (fun (s:State) -> s.data) (fun v s -> { s with data = v })
    let lview = Lens.lens (fun (s:State) -> s.view) (fun v s -> { s with view = v })
    let llog f = Lens.lens (fun (s:Data) -> s.log) (fun v s -> { s with log = v }) f
    let lroster = Lens.lens (fun (s:Data) -> s.roster) (fun v s -> { s with roster = v })
    let lfinished f = Lens.lens (fun (s:ViewState) -> s.finished) (fun v s -> { s with finished = v }) f
    let logCmd (msg : string) = Log [Expression.text msg]
    let log (logEntry : Log.Entry<_>) (state:State) : State =
        state |> Lens.over (ldata << llog) (Log.logDetailed logEntry)
    let emptyView =
        {
            lastInput = None
            lastCommand = None
            lastOutput = []
            outputDetailLevel = None
            logDetailLevel = 0
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

