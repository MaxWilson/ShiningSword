module UI.LocalStorage
open Thoth.Json
open Browser.Dom
open Domain.Character.Universal

let inline jsonParse<'t> fallback str : 't =
    match Decode.Auto.fromString str with
    | Ok result -> result
    | Result.Error err ->
        fallback

let inline read (key: string) fallback =
    try
        Browser.Dom.window.localStorage[key] |> jsonParse<'t> fallback
    with _ ->
        fallback
let inline write (key: string) value =
    Browser.Dom.window.localStorage[key] <- Encode.Auto.toString<'t>(0, value)

module PCs =
    let key = "PCs"
    let mutable cachedRead = None
    let read (): CharacterSheet array =
        match cachedRead with
        | Some cached -> cached
        | None ->
            let read = read key Array.empty
            cachedRead <- Some read
            read
    let write (v: CharacterSheet array) =
        write key v
        cachedRead <- None

module Graveyard =
    let key = "Graveyard"
    let read (): CharacterSheet array = read key Array.empty
    let write (v: CharacterSheet array) = write key v
