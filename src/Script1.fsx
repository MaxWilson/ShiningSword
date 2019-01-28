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
#load @"Model\Battle.fs"
#load @"Model\MonsterManual.fs"
#load @"Model\Gameplay.fs"
#load @"Abstractions\DataStorage.Globals.fs"
#load @"Abstractions\DataStorage.fs"

Model.MonsterManual.lookup "trex" ()
open Model
open Model.Types
let me = Model.Operations.CharSheet.create "Max" Male (18,18,18,14,12,13) false None (Model.Chargen.Templates.charTemplates.["Brute"])
let b = Battle.create |> Battle.addExistingCharacter TeamId.Blue (Model.Operations.CharInfo.ofCharSheet me)
