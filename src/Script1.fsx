#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#r @"C:\Users\maxw\.nuget\packages\fable.jsonconverter\1.0.8\lib\net45\Fable.JsonConverter.dll"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Abstractions\DataStorage.fs"

open System
open Interaction

let consoleResolve q = printfn "%A" q; Console.ReadLine()

DataStorage.showRaw "thingTracker" |> Eventual.resolveSynchronously consoleResolve
type Thing = { name: string; instances: (string * int) list }
DataStorage.list<Thing> "thingTracker"
DataStorage.load<Thing> "thingTracker" "Fasting"
type PC = { name: string; XP: int }
DataStorage.save "ShiningSwordPC" "Lariel" { name = "Lariel K'poli"; XP = 10000}
DataStorage.load<PC> "ShiningSwordPC" "Lariel"
DataStorage.delete "ShiningSwordPC" "Bob"
