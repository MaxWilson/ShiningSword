#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#r @"C:\Users\maxw\.nuget\packages\fable.jsonconverter\1.0.8\lib\net45\Fable.JsonConverter.dll"
#load @"Common.fs"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Abstractions\DataStorage.fs"

open System
open Common
open Interaction

let consoleResolve q = printfn "%A" q; Console.ReadLine()

DataStorage.showRaw "thingTracker" |> Eventual.resolveSynchronously consoleResolve
type Thing = { name: string; instances: (string * int) list }
DataStorage.list<Thing> "thingTracker"
DataStorage.load<Thing> "thingTracker" "Fasting"
type Class = Fighter | Wizard
type PC = { name: string; XP: int; HP: int; rolls: int[]; levels: (Class list) }
let combatBonus stat = (stat/2) - 5 // based on 5E tables
let computeHP con classList =
    let bonus = combatBonus con
    let dieSize characterClass = match characterClass with Fighter -> 10 | Wizard -> 6
    classList |> Seq.mapi (fun l cl -> if l = 0 then (dieSize cl) + bonus else (dieSize cl)/2 + 1 + bonus) |> Seq.sum
let computeLevel xp =
    let table = [
        1,0
        2,300
        3,900
        4,2700
        5,6000
        20,400000
        ]
    table |> List.findBack(fun (level, xpReq) -> xp >= xpReq) |> fst
let stats() =
    Array.init 6 (fun _ -> Seq.init 4 (thunk1 rand 6) |> Seq.sortDescending |> Seq.take 3 |> Seq.sum)
let PC name characterClass xp =
    let stats = stats()
    let levels = List.init (computeLevel xp) (thunk characterClass)
    { name = name; XP = xp; HP = (computeHP stats.[3] levels); rolls = stats; levels = levels }
let PCTag = "ShiningSwordPC"
let savePC pc = DataStorage.save PCTag pc.name pc |> Eventual.resolveSynchronously consoleResolve
let loadPC name = DataStorage.load<PC> PCTag name |> Eventual.resolveSynchronously consoleResolve
PC "Vaughn Shawnessey" Fighter 3000 |> savePC
loadPC "Vaughn Shawnessey"

DataStorage.showRaw (PCTag + "/Vaughn Shawnessey") |> Eventual.resolveSynchronously consoleResolve


