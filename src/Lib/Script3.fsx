

// This script is purely for messing around with graph visualization.
#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"

(*
Roadmap:
open files with Notepad (done),
then create GraphViz files by hand and open them (done),
then generate graphs and transform them into Graphviz files and open them,
then make it interactive.
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
    let proc = Process.Start(dotpath, $"""{filePath} -Tpng -o{path'}""")
    proc.WaitForExit()
    Process.Start("""c:\Windows\System32\mspaint.exe""", path')

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
