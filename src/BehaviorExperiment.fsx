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
let effect = EffectBuilder()

let handle e =
    let rec loop accum = function
        | Result v -> v, List.rev accum
        | Log(msg, f) -> loop (msg::accum) (inv f)
    loop [] e
let log msg = Log(msg, fun() -> Result())

let rec fib = function
    | 0 | 1 -> effect { return 1 }
    | n -> effect {
        let! a = fib (n-1)
        let! b = fib (n-2)
        do! (log (sprintf "%d(%d) |X| %d(%d) ==> %d(%d)" (n-1) a (n-2) b n (a+b)))
        return (a + b)
        }

fib 10 |> handle
