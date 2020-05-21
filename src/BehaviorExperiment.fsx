#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"
open Optics
open Optics.Operations

let inv f = f ()
type Input = string
type Effect<'t> =
    | Log of string * (unit -> 't Effect)
    | Read of query: string * continuation: (Input -> 't Effect)
    | Result of 't

let rec bind f = function
    | Log(msg, cont) ->
        Log(msg, fun() -> cont() |> bind f)
    | Read(query, cont) ->
        Read(query, fun input -> cont input |> bind f)
    | Result v -> f v
type EffectBuilder() =
    member this.Bind(effect, f) = bind f effect
    member this.Return e = Result e
let log msg = Log(msg, fun() -> Result())
let logf fmt = Printf.ksprintf log fmt

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

type Eventual<'q, 'input, 't> = Ready of 't | Awaiting of ('q * ('input -> Eventual<'q, 'input, 't>))
let step e =
    let rec loop accum = function
        | Result v -> Ready(v, List.rev accum)
        | Read(query, cont: Input -> Effect<_>) ->
            Awaiting(({| query = query; logOutput = List.rev accum |}), (fun e -> cont e |> loop []))
        | Log(msg, cont) ->
            cont() |> loop (msg::accum)
    loop [] e
module String =
    let join (input: string seq) = System.String.Join("\n", input)
type Capability<'t> = Capability of label: string * (unit -> 't)
let closure e =
    match step e with
    | Ready(v, logOut) -> v.ToString(), []
    | Awaiting(data, cont) ->
        let makeCapability (i: int) : Capability<Eventual<_,_,_>> =
            Capability(i.ToString(), fun () -> i.ToString() |> cont)
        sprintf "%s\n%s" (String.join data.logOutput) data.query, [1..10] |> List.map makeCapability
let rec fixedPoint = function
    | output, [] -> output |> printfn "Value: %s"
    | output, capabilities ->
        printfn "%s" output
        let (Capability(label, cap)) = capabilities |> chooseRandom
        printfn "Exercising %s" label
        cap() |> closure |> fixedPoint



let rec fib = function
    | 0 | 1 -> effect { return 1 }
    | n -> effect {
        let! a = fib (n-1)
        let! b = fib (n-2)
        do! logf "%d(%d) |X| %d(%d) ==> %d(%d)" (n-1) a (n-2) b n (a+b)
        return (a + b)
        }

fib 10 |> extractResult
fib 10 |> closure |> fixedPoint
match fib 10 |> closure with
| label, Capability(clabel, cont)::rest ->
    printfn "%s\n%s" label clabel
    cont() |> printfn "%A"
| l, [] -> printfn "%s" l

