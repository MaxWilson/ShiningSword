#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#r @"bin\Fable.JsonConverter.dll"
#load @"Common.fs"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Model\Types.fs"
#load @"Model\Names.fs"
#load @"Model\Tables.fs"
#load @"Model\Operations.fs"
#load @"Model\Chargen.fs"
#load @"Model\Gameplay.fs"
#load @"Abstractions\DataStorage.Globals.fs"
#load @"Abstractions\DataStorage.fs"
#load @"Model\Types.fs"
#load @"Model\Tables.fs"


open System
open Common
open Interaction
open Model.Types
open Model.Tables
