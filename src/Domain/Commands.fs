module Domain.Commands
open Common
open Domain.Properties

type Evaluation = Ready of int | Awaiting of Key
type Reference = EventRef of Id | PropertyRef of Key
type Value = Number of int | Text of string | Dice of Domain.Dice.Dice<Reference>

type Expression =
    | Literal of Value
    | Ref of Reference
    | BinaryOperation of Expression * ArithmeticOperator * Expression
    | If of {| test: Expression; onTrue: Expression; onFalse: Expression option |}
    | GreaterThan of lhs: Expression * rhs: Expression
    | Roll of Domain.Dice.Dice<Reference>
    | BestN of n:int * Expression list

type TextOrLogExpression = Text of string | LogExpression of text:string * Expression
type Command =
    | Evaluate of ast:Expression
    | AddRow of name:string
    | SetData of Key * Expression
    | Log of TextOrLogExpression list
    | NotImpl // placeholder

#nowarn "40" // we're not doing anything funny at initialization-time, like calling functions in the ctor that rely on uninitialized members, so we don't need the warning
module Parse =
    open Domain.Dice.Parse
    open Domain.Properties.Parse
    open Packrat
    let (|PropertyReference|_|) = ((|PropertyReference|_|) PropertyRef)
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
        | Branch(b, rest) -> notImpl()
        | Attack(a, rest) -> notImpl()
        | Int(n, Str "d" (Int(d, Str "k" (Int(k, rest))))) ->
            Some(BestN(k, List.init n (thunk <| Roll(Domain.Dice.Dice(1,d)))), rest)
        | Term(d, rest) -> Some(Roll d, rest)
        | _ -> None
    let (|DieEvaluation|_|) =
        function
        | Int(n, Operator "." (DieOperation(d, rest))) ->
            let rec help i = if i <= 0 then d else BinaryOperation(d, Plus, help (i-1))
            Some(help n, rest)
        | DieOperation(d, rest) -> Some(d, rest)
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
                Some(LogExpression(txt, d), rest)
            | _ -> None
        let (|LogText|_|) = pack <| function
            | CharsExcept (Set.ofList['[']) (txt, rest) -> Some(Text txt, rest)
            | _ -> None
        let rec (|LogChunks|_|) = pack <| function
            | LogChunks(lst, LogChunks(tail, rest)) -> Some(lst@tail, rest)
            | EmbeddedRoll(r, rest) -> Some([r], rest)
            | LogText(txt, rest) -> Some([txt], rest)
            | _ -> None
        pack <| function
        | Str "/" (LogChunks(chunks, rest)) -> Some(Log chunks, rest)
        | _ -> None
    let (|Command|_|) = pack <| function
        | LogCommand(cmd, rest) -> Some(cmd, rest)
        | IoOperation(cmd, rest) -> Some(NotImpl, rest)
        | Keyword "add" (Any(name, rest)) -> Some(NotImpl, rest)
        | Keyword "avg" (DieEvaluation(d, rest)) -> Some(NotImpl, rest)
        | SetValue(cmd, rest) -> Some(NotImpl, rest)
        | DieEvaluation(d, rest) as start -> Some(Evaluate(d), rest)
        | _ -> None

