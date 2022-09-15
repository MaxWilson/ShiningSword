#I __SOURCE_DIRECTORY__
#I @".."
#I @"..\.."
#load @"Lib\Core\Optics.fs"
#load @"Lib\Core\Common.fs"
#load @"Lib\Core\Packrat.fs"
#load @"Lib\Core\DerivedTraits.fs"
#load @"Lib\Domain\Metrics.fs"
#load @"Lib\Domain\Random.fs"
#load @"Lib\Domain\Treasure.fs"
#load @"Lib\Domain\Onomastikon.fs"
#load @"Lib\Domain\Namespace.fs"
#load @"Lib\Domain\Character\Character.fs"
#load @"Lib\Domain\Character\Character2e.fs"
#load @"Lib\Domain\Character\Character5e.fs"
#load @"Lib\Domain\Character\CharacterUniversal.fs"
#load @"Lib\Domain\Ribbit\DataStore.fs"
#load @"Lib\Domain\Ribbit\Core.fs"
#load @"Lib\Domain\Ribbit\Operations.fs"
#load @"Lib\Domain\Ribbit\Commands.fs"
#load @"Lib\Domain\Ribbit\Rules2e.fs"
#load @"Lib\Domain\Ribbit\Rules5e.fs"
#load @"Lib\Adventure.fs"
#load @"Dev\Tracker\Game.fs"
#r "nuget: Unquote"
#load @"Dev\Tracker\Commands.fs"

open Domain
open Domain.Ribbit
open Domain.Ribbit.Core
open Domain.Ribbit.Operations
// let's see how easy it would be to serialize character sheets stored using Ribbit

let ageP = NumberProperty("age")
let stP = NumberProperty("ST")
let dxP = NumberProperty("DX")
let htP = NumberProperty("HT")
let iqP = NumberProperty("IQ")
let moodP = TextProperty("Mood", "fine") // should reset to fine when round-tripped through JSON because we won't store mood between adventures
let stats = [stP; dxP; htP; iqP]
let inline set (prop: Property<_, Ribbit>) v r = prop.Set(1, v) r
let r =
    Ribbit.Fresh
    |> set personalNameP "Bob"
    |> set hpP 20
    |> set ageP 18
    |> (fun r -> stats |> List.fold (fun r stat -> set stat 10 r) r)
    |> set moodP "happy"

type DF_DTO = // Data transfer object for DF
    {
        name: string
        ST: int
        DX: int
        HT: int
        IQ: int
        hp: int
        age: int option
        }
let toDto (r:Ribbit) : DF_DTO =
    let inline get (prop: Property<_,_>) =
        prop.Get 1 r
    let ctx = EvaluationContext.Create r
    let tryGet (prop: Property<_,_>) =
        prop.GetM 1 ctx |> Result.toOption
    {
        name = get personalNameP
        ST = get stP
        DX = get dxP
        HT = get htP
        IQ = get iqP
        age = tryGet ageP
        hp = get hpP
        }
let loadDto id (dto:DF_DTO) (r:Ribbit) =
    let inline set (prop: Property<_, Ribbit>) v r = prop.Set(id, v) r
    r
        |> set personalNameP dto.name
        |> set hpP dto.hp
        |> set stP dto.ST
        |> set dxP dto.DX
        |> set htP dto.HT
        |> set iqP dto.IQ
        |> (match dto.age with Some age -> set ageP age | None -> Microsoft.FSharp.Core.Operators.id)

#r "nuget: Newtonsoft.Json"
open Newtonsoft.Json
let json = JsonConvert.SerializeObject(toDto r)
printfn "%s" json
let r' = JsonConvert.DeserializeObject<DF_DTO>(json) |> fun dto -> loadDto 2 dto Ribbit.Fresh
r' |> moodP.Get 2 // fine, not happy any more, because mood isn't serialized.
