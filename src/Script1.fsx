#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#r @"C:\Users\maxw\.nuget\packages\fable.jsonconverter\1.0.8\lib\net45\Fable.JsonConverter.dll"
#load @"Common.fs"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Abstractions\DataStorage.fs"
#load @"Model\Types.fs"
#load @"Model\Tables.fs"

open System
open Common
open Interaction
open Model.Types
open Model.Tables

let consoleResolve q = printfn "%A" q; Console.ReadLine()

DataStorage.showRaw "thingTracker" |> Eventual.resolveSynchronously consoleResolve
type Thing = { name: string; instances: (string * int) list }
DataStorage.list<Thing> "thingTracker"
DataStorage.load<Thing> "thingTracker" "Fasting"
type Class = Fighter | Wizard
type PC = { name: Name; XP: int; HP: int; rolls: int[]; levels: (Class list); description: Description }
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
    (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
let stats() =
    Array.init 6 (fun _ -> Seq.init 4 (thunk1 rand 6) |> Seq.sortDescending |> Seq.take 3 |> Seq.sum)
let PC name characterClass xp =
    let stats = stats()
    let levels = List.init (computeLevel xp) (thunk characterClass)
    { name = name; XP = xp; HP = (computeHP stats.[2] levels); rolls = stats; levels = levels; description = "" }
let PCTag = "ShiningSwordPC"
let savePC pc = DataStorage.save PCTag pc.name pc |> Eventual.resolveSynchronously consoleResolve
let loadPC name = DataStorage.load<PC> PCTag name |> Eventual.resolveSynchronously consoleResolve
PC "Vaughn Shawnessey" Fighter 3000 |> savePC
loadPC "Vaughn Shawnessey"

let exec =
    Eventual.continueWith (printfn "Final = %A")
    >> Eventual.resolveSynchronously consoleResolve

let show pc =
    sprintf "Name: %s\n%dth level %A (%d XP)\nStr %d Dex %d Con %d Int %d Wis %d Cha %d HP %d " pc.name (pc.levels.Length) (pc.levels.[0]) pc.XP pc.rolls.[0] pc.rolls.[1] pc.rolls.[2] pc.rolls.[3] pc.rolls.[4] pc.rolls.[5] pc.HP

(PC "Vlad" Wizard ((rand 20)*(rand 20)*(rand 20) * 100)) |> show |> printfn "%s"



