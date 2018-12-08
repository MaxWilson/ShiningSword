// Learn more about F# at http://fsharp.org
namespace Interaction
open Common

type Eventual<'arg, 'intermediate, 'result> =
    | Final of 'result
    | Intermediate of question:'intermediate * provideAnswer:('arg -> (Eventual<'arg, 'intermediate, 'result>))

// An Eventual that has not completed yet, and will have a side effect when it does complete
type Operation<'t, 'msg> = Operation of 't * ('msg -> Eventual<'msg, 't, unit>)

module Eventual =
    open Fable.PowerPack.Keyboard
    open Fable.PowerPack.Keyboard

    let bind m f =
        let rec chain m =
            match m with
            | Final v -> f v // prereq already satisfied: evaluate continuation immediately
            | Intermediate(q, continuation) ->
                Intermediate(q, continuation >> chain)
        chain m

    /// Trampoline until Final state is reached, resolving queries back
    /// to answers using the fResolveQuery. E.g. fResolveQuery might
    /// turn an Interact<'t> into a 't by calling Console.WriteLine + Readline()
    let resolveSynchronously (fResolveQuery: 'intermediate -> 'arg) =
        let rec resolve monad =
            match monad with
            | Final v -> v
            | Intermediate(q, f) ->
                let answer = fResolveQuery q
                match f answer with
                | Final v -> v
                | Intermediate(s, _) as m ->
                    resolve m
        resolve

    let continueWith follow = flip bind (fun v -> let v = follow v in Final v)

    // Stateful, Mailbox-style integration for Eventual. Do onEnd immediately,
    //  or post a pending Operation somewhere where it will get progressed later.
    let toOperation onStart onEnd eventual =
        let finish v =
            onEnd v
            Final ()
        match bind eventual finish with
        | Intermediate(q, f) ->
            let op = Operation(q, f)
            onStart op
        | Final v -> () // onEnd should have already executed

type InteractionBuilder<'query, 'input>() =
    let wrap q recognizer continuation  =
        let rec this =
            Intermediate(q, fun arg ->
                match arg |> recognizer with
                | Some arg ->
                    continuation arg
                | _ -> this)
        this
    // todo: make more generic, with inputs other than string
    member this.Bind((q:'query, recognizer: 'input -> 'arg option), continuation: ('arg -> Eventual<'input, 'query, _>)): Eventual<'input, 'query, _> =
        wrap q recognizer continuation
    member this.Bind(interaction: Eventual<'input, 'query, 'a>, continuation: 'a -> Eventual<'input, 'query, 'b>) : Eventual<'input, 'query, 'b> =
        Eventual.bind interaction continuation
    member this.Return(x) = Final (x)
    member this.ReturnFrom(x) = x


