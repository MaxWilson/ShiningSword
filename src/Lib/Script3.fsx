

// This script is purely for messing around with graph visualization.
#I __SOURCE_DIRECTORY__
#load @"Optics.fs"
#load @"Common.fs"

(*
Roadmap:
open files with Notepad (done),
then create GraphViz files by hand and open them (done),
then generate graphs and transform them into Graphviz files and open them (done),
then make it interactive (nothing to do).
Stretch goals:
make graphviz show layouts spatially in some kind of grid where north/south are actually aligned that way.
eliminate double-printing of directions by using two colors (done).
*)

open System.Diagnostics
open System.IO

let openNotepad notepadPath filePath =
    Process.Start(notepadPath, filePath)
let dumpToFile txt =
    let path = System.IO.Path.GetTempFileName()
    System.IO.File.WriteAllText(path, txt)
    path

let notepad = openNotepad """d:\usr\bin\auto\notepad2.exe"""
"Hello world" |> dumpToFile |> notepad

let dot filePath =
    let dotpath = """d:\usr\bin\Graphviz\bin\dot.exe"""
    if not <| File.Exists dotpath then failwith $"You must install dot at '{dotpath}' first."
    let path' = System.IO.Path.ChangeExtension(filePath, "png")
    let proc =
        let info = ProcessStartInfo(dotpath, $"""{filePath} -Tpng -o{path'}""")
        info.RedirectStandardOutput <- true
        info.UseShellExecute <- false
        Process.Start(info)
    proc.WaitForExit()
    if proc.ExitCode = 0 then
        Process.Start("""c:\Windows\System32\mspaint.exe""", path') |> ignore


"""
digraph G {
 size ="4,4";
 main [shape=box]; /* this is a comment */
 main -> parse [weight=8];
 parse -> execute;
 main -> init [style=dotted];
 main -> cleanup;
 execute -> { make_string; printf}
 init -> make_string;
 edge [color=red]; // so is this
 main -> printf [style=bold,label="100 times"];
 make_string [label="make a\nstring"];
 node [shape=box,style=filled,color=".7 .3 1.0"];
 execute -> compare;
}
""" |> dumpToFile |> dot

module DungeonRoom =
    type Direction = North | South | East | West
        with
        static member opposite direction =
            match direction with
            | North -> South
            | South -> North
            | West -> East
            | East -> West
    type RoomID = int
    type Room =
        Room of Label: string
        with
        member this.label =
            let (Room(label)) = this
            label
    open Optics
    open type Optics.Operations
    type IdGenerator = NextId of int
        with
        static member fresh = NextId 1
        static member newId (idGenerator_: Lens<'m, IdGenerator>) (model: 'm) =
            let mutable id' = 0
            let model' = model |> over idGenerator_ (fun (NextId id) -> id' <- id; NextId (id'+1))
            id', model'
    type Dungeon = {
        newId: IdGenerator
        rooms: Map<RoomID, Room>
        exits: Map<RoomID, Map<Direction, RoomID>>
        }
    let Exits_ = Lens.create (fun d -> d.exits) (fun v d -> { d with exits = v } )
    let Rooms_ = Lens.create (fun d -> d.rooms) (fun v d -> { d with rooms = v } )
    let NewId_ = Lens.create (fun d -> d.newId) (fun v d -> { d with newId = v } )        
    let newId =
        IdGenerator.newId NewId_
    let connect origin direction destination dungeon =
        let check nodeId direction =
            let exits = (dungeon |> read Exits_) |> Map.findForce nodeId
            if exits |> Map.containsKey direction then
                failwith $"Invalid connection. That direction ({direction}) is already taken for room {dungeon.rooms.[nodeId].label}."
        let reverse = (Direction.opposite direction)
        check origin direction
        check destination reverse
        dungeon
            |> over (Exits_) (Map.addForce origin (Map.add direction destination))
            |> over (Exits_) (Map.addForce destination (Map.add reverse origin))
    let emptyDungeon = { rooms = Map.empty; exits = Map.empty; newId = IdGenerator.fresh }
        
    let generate numberOfEdges =
        let addRoom dungeon =
            let id, dungeon' = newId dungeon
            let room = Room($"Room #{id}")
            id, dungeon' |> over Rooms_ (Map.add id room)

        let rec addEdge dungeon countDown =
            let mutable dungeon = dungeon
            let rooms = dungeon.rooms |> Map.keys
            let origin = chooseRandom rooms
            let dest =
                if rand 100 < dungeon.rooms.Count * 10 then
                    chooseRandom rooms
                else
                    let id, dungeon' = addRoom dungeon
                    dungeon <- dungeon'
                    id
            let candidates =
                let occupied nodeId direction =
                    (dungeon |> read Exits_) |> Map.findForce nodeId |> Map.containsKey direction
                [North; South; East; West]
                |> List.filter (fun d -> not (occupied origin d || occupied dest (Direction.opposite d)))
            match candidates with
            | [] -> addEdge dungeon countDown
            | directions ->
                iteri &dungeon (connect origin (chooseRandom directions) dest)
                if countDown > 0 then addEdge dungeon (countDown - 1)
                else dungeon
        addEdge (emptyDungeon |> addRoom |> snd) numberOfEdges

    let toGraphViz dungeon =
        let r = dungeon.rooms
        let k = r |> Map.keys
        let nodes =
            dungeon.rooms
            |> Seq.map (fun (KeyValue(id, Room(label))) -> $"  {id} [shape=box,label=\"{label}\"];")
            |> String.join "\n"
        let colorOf = function
            | North -> "red;0.5:blue;0.5" // blue exit is northbound, red is southbound
            | West -> "green;0.5:yellow;0.5" // yellow exit is westbound, green is eastbound
            | _ -> failwith "not needed"
        let edges =
            [for KeyValue(id, exits) in dungeon.exits do
                for KeyValue(direction, dest) in exits do
                    if direction = North || direction = West then
                        $"  {id}->{dest}[color=\"{colorOf direction}\",dir=none];"
                ]
            |> String.join "\n"
        sprintf "digraph G { %s\n%s }" nodes edges

open DungeonRoom
// demonstrate usage:
generate 20 |> toGraphViz |> dumpToFile |> dot

let gnolls n =
    let space = Array2D.create 30 30 '.'
    let rec addGnoll() =
        let x, y = random.Next 30, random.Next 30
        if space.[x, y] = '.' then
            space.[x, y] <- 'G'
        else
            addGnoll()
    for _ in 1..n do
        addGnoll()
    [for x in 0..Array2D.length1 space - 1 do
        space.[x, *] |> System.String
        ]
    |> String.join "\n"

gnolls 50 |> dumpToFile |> notepad
