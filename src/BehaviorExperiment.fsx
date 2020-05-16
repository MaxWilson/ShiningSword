#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"
open Optics
open Optics.Operations

type Todo() =
    do
    failwith "Not implemented"
type Ref = Id
type Value = string
type Awaitable<'Ref, 't> = Await of 'Ref | InProgress | Done of 't
type Thing = { value: Value }
type Model = {
    Blocking: Map<Ref, Ref list>
    Blocked: Map<Ref, Ref list>
    Queue: Ref list
    Things: Map<Ref, Awaitable<Ref * Model -> Model, Thing>>
    }

let tryAwait (lens: 'addr -> Lens<_, _>) lookup queue myAddress (state: Model) =
    let b = state |> read (lens myAddress)
    match b with
    | Done _ | InProgress ->
        state
    | Await (ref, progress: 'continuation) ->
        match lookup ref with
        | Done v ->
            state |> write (lens myAddress) InProgress |> queue myAddress progress
        | _ -> state
