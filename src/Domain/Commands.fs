module Domain.Commands
open Common

type Command = unit

#nowarn "40" // we're not doing anything funny at initialization-time, like calling functions in the ctor that rely on uninitialized members, so we don't need the warning
module Parse =
    open Domain.Dice.Parse
    open Domain.Properties.Parse
    open Packrat
    let (|PropertyReference|_|) = ((|PropertyReference|_|) id)
    let (|Term|_|) = (|Term|_|) (|PropertyReference|_|)
    let (|Operator|_|) char = function
        | OWS(Str char (OWS(rest))) -> Some(rest)
        | _ -> None
    let (|Keyword|_|) keyword = function Word(word, rest) when String.equalsIgnoreCase word keyword -> Some(rest) | _ -> None
    let (|IoOperation|_|) = pack <| function
        | Keyword "load" (Any(name, rest)) -> Some((), rest)
        | Keyword "save" (Any(name, rest)) -> Some((), rest)
        | Keyword "export" (Keyword "save" (Any(name, rest))) -> Some((), rest)
        | Keyword "load" (Keyword "import" (Any(name, rest))) -> Some((), rest)
        | _ -> None
    let (|Condition|_|) = pack <| function
        | Int(n, Str "a" rest) -> Some((), rest)
        | Int(n, Str "d" rest) -> Some((), rest)
        | Int(n, rest) -> Some((), rest)
        | Term(t, Keyword "at" (Keyword "least" (Int(target, rest)))) -> Some((), rest)
        | Term(t, Keyword "at" (Keyword "most" (Int(target, rest)))) -> Some((), rest)
        | Term(t, rest) -> Some((), rest)
        | _ -> None
    let (|Branch|_|) = pack <| function
        | Condition(c, Operator "?" (Term(b1, Operator ":" (Term(b2, rest))))) -> Some((), rest)
        | Condition(c, Operator "?" (Term(b1, rest))) -> Some((), rest)
        | Condition(c, Operator "?" rest) -> Some((), rest)
        | _ -> None
    let (|Attack|_|) =
        let (|Mod|_|) = function
            | Int(n, Str "a" rest) -> Some((), rest)
            | Int(n, Str "d" rest) -> Some((), rest)
            | Int(n, rest) -> Some((), rest)
            | _ -> None
        pack <| function
            | Str "att" (Int(ac, Mod(m, Term(dmg, rest)))) -> Some((), rest)
            | _ -> None
    let (|DieOperation|_|) = pack <| function
        | Branch(b, rest) -> Some((), rest)
        | Attack(a, rest) -> Some((), rest)
        | Int(n, Str "d" (Int(d, Str "k" (Int(k, rest))))) -> Some((), rest)
        | Term(d, rest) -> Some((), rest)
        | _ -> None
    let (|DieEvaluation|_|) =
        function
        | Int(n, Operator "." (DieOperation(d, rest))) -> Some((), rest)
        | DieOperation(d, rest) -> Some((), rest)
        | _ -> None
    let (|PropertyName|_|) = (|Word|_|)
    let (|SetValue|_|) = pack <| function
        | PropertyReference(ref, Operator "=" (DieOperation(d, rest)))-> Some((), rest)
        | ValidNames(ref, Keyword "has" (DieOperation(d, PropertyName(prop, rest))))-> Some((), rest)
        | ValidNames(ref, Keyword "gains" (DieOperation(d, PropertyName(prop, rest))))-> Some((), rest)
        | ValidNames(ref, Word(AnyCase("loses" | "spends"), (DieOperation(d, PropertyName(prop, rest))))) -> Some((), rest)
        | _ -> None
    let (|LogCommand|_|) =
        let (|EmbeddedRoll|_|) = pack <| function
            | Str "[" (DieOperation(d, (Str "]" rest as finish)) as start) ->
                let txt = readBetween start finish
                Some(txt, rest)
            | _ -> None
        let (|LogText|_|) = pack <| function
            | CharsExcept (Set.ofList['[']) (txt, rest) -> Some((), rest)
            | _ -> None
        let rec (|LogChunks|_|) = pack <| function
            | LogChunks(lst, LogChunks(tail, rest)) -> Some(lst@tail, rest)
            | EmbeddedRoll(r, rest) -> Some([], rest)
            | LogText(txt, rest) -> Some([], rest)
            | _ -> None
        pack <| function
        | Str "/" (LogChunks(chunks, rest)) -> Some((), rest)
        | _ -> None
    let (|Command|_|) = pack <| function
        | LogCommand(cmd, rest) -> Some((), rest)
        | IoOperation(cmd, rest) -> Some((), rest)
        | Keyword "add" (Any(name, rest)) -> Some((), rest)
        | Keyword "avg" (DieEvaluation(d, rest)) -> Some((), rest)
        | SetValue(cmd, rest) -> Some((), rest)
        | DieEvaluation(d, rest) -> Some((), rest)
        | _ -> None

