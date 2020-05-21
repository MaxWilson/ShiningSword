#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"
open Optics
open Optics.Operations

let inv f = f ()
type Effect<'t> =
    | Log of string * (unit -> 't Effect)
    | Result of 't

let rec bind f = function
    | Log(msg, cont) ->
        Log(msg, fun() -> cont() |> bind f)
    | Result v -> f v
type EffectBuilder() =
    member this.Bind(effect, f) = bind f effect
    member this.Return e = Result e
let log msg = Log(msg, fun() -> Result())
let logf msg fmt = Printf.kbprintf msg

let effect = EffectBuilder()

let tryExtractResult e =
    match e with
    | Result v -> Some v
    | _ -> None

// asserts that we're already at a fixed point
let extractResult e =
    match tryExtractResult e with
    | Some v -> v
    | None -> failwith "That effect hasn't finished all of its side effects yet"

let extract e =
    let rec loop accum = function
        | Result v -> v, List.rev accum
        | Log(msg, cont) ->
            cont() |> loop (msg::accum)
    loop [] e

let handle e =
    let rec loop accum = function
        | Result v -> v, List.rev accum
        | Log(msg, f) -> loop (msg::accum) (inv f)
    loop [] e

let rec fib = function
    | 0 | 1 -> effect { return 1 }
    | n -> effect {
        let! a = fib (n-1)
        let! b = fib (n-2)
        do! (log (sprintf "%d(%d) |X| %d(%d) ==> %d(%d)" (n-1) a (n-2) b n (a+b)))
        return (a + b)
        }

fib 10 |> handle
fib 10 |> extractResult
fib 10 |> extract
