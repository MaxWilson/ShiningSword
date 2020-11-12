// This script is purely for messing around with graph visualization.
#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"

(*
Roadmap:
open files with Notepad (done),
then create GraphViz files by hand and open them,
then generate graphs and transform them into Graphviz files and open them,
then make it interactive.
*)

open System.Diagnostics

let openNotepad notepadPath filePath =
    Process.Start(notepadPath, filePath)
let dumpToFile txt =
    let path = System.IO.Path.GetTempFileName()
    System.IO.File.WriteAllText(path, txt)
    path

let notepad = openNotepad """d:\usr\bin\auto\notepad2.exe"""
notepad
"Hello world" |> dumpToFile |> notepad

