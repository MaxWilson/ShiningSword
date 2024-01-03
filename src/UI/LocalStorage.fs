module UI.LocalStorage
open Thoth.Json
open Browser.Dom

let inline jsonParse<'t> fallback str : 't =
    match Decode.Auto.fromString str with
    | Ok result -> result
    | Result.Error err ->
        fallback()

let inline read (key: string) fallback =
    try
        Browser.Dom.window.localStorage[key] |> jsonParse<'t> fallback
    with _ ->
        fallback()
let inline write (key: string) value =
    Browser.Dom.window.localStorage[key] <- Encode.Auto.toString<'t>(0, value)

module Cache =
    let create<'t>() =
        let mutable cache = None
        let read onCacheMiss =
            match cache with
            | Some (v: 't) -> v
            | None -> onCacheMiss()
        let invalidate() =
            cache <- None
        read, invalidate

open Cache

open Domain.ADND.PriestSpells
module Spheres =
    let key = "Spheres"
    let cacheRead, cacheInvalidate = Cache.create()
    let read (): Sphere list =
        cacheRead (thunk2 read key defaultSpheres)
    let write (v: Sphere list) =
        write key v
        cacheInvalidate()
module Notes =
    let key = "Notes"
    let cacheRead, cacheInvalidate = Cache.create()
    let read (): Map<SpellName, string> =
        cacheRead (thunk2 read key (thunk Map.empty))
    let write (v: Map<SpellName, string>) =
        write key v
        cacheInvalidate()
module Deities =
    let key = "Deities"
    let cacheRead, cacheInvalidate = Cache.create()
    let read (): Deity list =
        cacheRead (thunk2 read key defaultDeities)
    let write (v: Deity list) =
        write key v
        cacheInvalidate()
module SpellPicks =
    let key = "Picks"
    let cacheRead, cacheInvalidate = Cache.create()
    let read (): Map<SpellName, int> =
        cacheRead (thunk2 read key (thunk Map.empty))
    let write (v: Map<SpellName, int>) =
        write key v
        cacheInvalidate()

