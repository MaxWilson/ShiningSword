// Learn more about F# at http://fsharp.org
namespace Interaction

type Eventual<'arg, 'intermediate, 'result> =
    | Final of 'result
    | Intermediate of question:'intermediate * provideAnswer:('arg -> (Eventual<'arg, 'intermediate, 'result>))

module Eventual =
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
    let resolve (fResolveQuery: 'intermediate -> 'arg) =
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

type Interactive<'answer,'question,'result> = Eventual<'answer, 'question, 'result>

type InteractionBuilder<'input, 'query>() =
    let wrap q recognizer continuation  =
        let rec this =
            Intermediate(q, fun arg ->
                match arg |> recognizer with
                | Some arg ->
                    continuation arg
                | _ -> this)
        this
    // todo: make more generic, with inputs other than string
    member this.Bind((q:'query, recognizer: 'input -> 'arg option), continuation: ('arg -> Interactive<'input, 'query, _>)): Interactive<'input, 'query, _> =
        wrap q recognizer continuation
    member this.Bind(interaction: Interactive<'input, 'query, 'a>, continuation: 'a -> Interactive<'input, 'query, 'b>) : Interactive<'input, 'query, 'b> =
        Eventual.bind interaction continuation
    member this.Return(x) = Final (x)
    member this.ReturnFrom(x) = x

