module Log
open Common
open Common.Hierarchy

type Chunk = Hierarchy.Hierarchy<string, string>
type Entry<'t> = 't * Chunk
type Page<'t> = Entry<'t> list
type Entries<'t> = Page<'t> list
type Data<'t> = Page<'t> * Entries<'t>

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
let extractEntries (x : Data<_>) : Entries<_> = x |> flush |> snd |> List.rev
let mapEntries f (d: Entries<_>) = List.map (List.map f) d
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
