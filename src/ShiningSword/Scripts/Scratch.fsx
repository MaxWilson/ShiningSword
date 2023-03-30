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
#r "nuget: Unquote"

type Key = string list
let clearUnder (key: Key) (queue: Map<Key,_>) =
    let len = key.Length
    let strictlyUnder (key': Key) =
        let rec under (k: Key) =
            if k = key then true
            elif k.Length > len then under k.Tail
            else false
        if key'.Length > len then // in order to be strictly under, it has to be longer
            if key = key' then
                printfn "Verdict: remove %A" key'
                true
            else
                under key'
        else
            printfn "Verdict: skip %A" key'
            false
    queue |> Map.filter (fun k v -> k |> strictlyUnder |> not)

let map = Map.ofList [
    ["Weapon Master"; "Advantages"; "Swashbuckler"], ""
    ["OneWeapon";"Weapon Master"; "Advantages"; "Swashbuckler"], ""
    ]
map |> clearUnder ["Weapon Master"; "Advantages"; "Swashbuckler"]

