#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Optics.fs"
open Optics
open Optics.Operations

let inv f = f ()
type Effect<'input, 't> =
    | Log of string * (unit -> Effect<'input, 't>)
    | Read of query: string * continuation: ('input -> Effect<'input, 't>)
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
let read prompt = Read(prompt, fun ans -> Result(ans))
let readf fmt = Printf.ksprintf read fmt

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
        | Result v -> Ready({| log = List.rev accum; value = v |})
        | Read(query, cont: _ -> Effect<_,_>) ->
            Awaiting(({| query = query; logOutput = List.rev accum |}), (fun e -> cont e |> loop []))
        | Log(msg, cont) ->
            cont() |> loop (msg::accum)
    loop [] e
module String =
    let join (input: string seq) = System.String.Join("\n", input)
type Model<'t> = 't * (Capability<'t>) list
and Capability<'t> = Capability of label: string * exercise: (unit -> Model<'t>)
//type Capability<'t> = Capability of label: string * (unit -> string * Capability<'t> list)
let viewOfEventual model =
    let rec view model : string Model =
        match model with
        | Ready(result: {| log: string list; value: _ |}) ->
            sprintf "%s\n%s" (result.log |> String.join) (inv result.value.ToString), []
        | Awaiting(data : {| query: string; logOutput: string list |}, cont) ->
            let makeCapability (i: int) : Capability<string> =
                let label = i.ToString()
                let dispatch cmd : string Model =
                    let model = cont cmd
                    view model
                Capability(label, fun() -> dispatch i)
            sprintf "%s\n%s" (String.join data.logOutput) data.query, [1..1000] |> List.map makeCapability
    view model
let rec fixedPoint = function
    | output, [] -> output |> printfn "Value: %s"
    | output, capabilities ->
        printfn "%s" output
        let (Capability(label, exercise)) = capabilities |> chooseRandom
        printfn "Exercising %s" label
        let result = exercise()
        result |> fixedPoint

let rec fib = function
    | 0 | 1 -> effect { return 1 }
    | n -> effect {
        let! a = fib (n-1)
        let! b = fib (n-2)
        do! logf "%d(%d) |X| %d(%d) ==> %d(%d)" (n-1) a (n-2) b n (a+b)
        return (a + b)
        }

fib 10 |> step |> viewOfEventual |> fixedPoint

let getNum =
    let rec loop accum = effect {
        if accum % 13I = 0I then return accum
        else
            let! (v: int) = readf "%A is not divible by 13. Enter another number" accum
            let! retval = loop (accum + (System.Numerics.BigInteger v))
            return retval
        }
    loop 1I
getNum |> step |> viewOfEventual |> fixedPoint

