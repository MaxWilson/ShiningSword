module Domain.Commands
open Common
open Domain.Properties

type Reference = EventRef of Id | PropertyRef of Key
type Value = Number of int | Text of string | Dice of Domain.Dice.Dice<Reference> with
    static member (+) (lhs:Value, rhs:Value) =
        match lhs, rhs with
        | Number l, Number r -> Number(l+r)
        | Text l, Text r -> Text(l+r)
        | Dice l, Dice r -> Dice.Binary(l, Plus, r) |> Dice
    static member (-) (lhs:Value, rhs:Value) =
        match lhs, rhs with
        | Number l, Number r -> Number(l+r)
        | Text l, Text r -> Text(l+r)
        | Dice l, Dice r -> Dice.Binary(l, Minus, r) |> Dice
    static member Zero = Number 0
    override this.ToString() = match this with Number n -> n.ToString() | Text t -> t | Dice d -> Dice.toString d
type Evaluation = Ready of Value | Awaiting of Key
type Comparison = GreaterThan | GreaterThanEqual | LessThan | LessThanEqual | Equal
type Expression =
    | Literal of Value
    | Ref of Reference
    | BinaryOperation of Expression * ArithmeticOperator * Expression
    | If of {| index: Expression; branches: {| test: Comparison * Expression; consequence: Expression|} list; otherwise: Expression option |}
    | Roll of Domain.Dice.Dice<Reference>
    | BestN of n:int * Expression list
    | Negate of Expression

type TextOrLogExpression = Text of string | LogExpression of text:string * Expression
type Command =
    | Evaluate of ast:Expression
    | Average of ast:Expression
    | AddRow of name:string
    | ChangeProperty of Key list * Expression
    | SetProperty of Key list * Expression
    | Log of TextOrLogExpression list
    | NotImpl // placeholder

#nowarn "40" // we're not doing anything funny at initialization-time, like calling functions in the ctor that rely on uninitialized members, so we don't need the warning
module Parse =
    open Domain.Dice
    open Domain.Dice.Parse
    open Domain.Properties.Parse
    open Packrat
    let (|Term|_|) =
        let (|PropertyReference|_|) = ((|PropertyReference|_|) PropertyRef)
        (|Term|_|) (|PropertyReference|_|)
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
    let (|Condition|_|) =
        let d n d = Dice(n, d)
        let fix x = Literal (Number x)
        pack <| function
        | Int(n, Str "a" (Operator "?" rest)) -> Some((Roll (Max(Dice(1,20), Dice(1,20))), GreaterThanEqual, fix n), rest)
        | Int(n, Str "d" (Operator "?" rest)) -> Some((Roll (Min(Dice(1,20), Dice(1,20))), GreaterThanEqual, fix n), rest)
        | Int(n, (Operator "?" rest)) -> Some((Roll (Dice(1,20)), GreaterThanEqual, fix n), rest)
        | Term(t, Keyword "at" (Keyword "least" (Int(target, (Operator "?" rest))))) -> Some((Roll t, GreaterThanEqual, fix target), rest)
        | Term(t, Keyword "at" (Keyword "most" (Int(target, (Operator "?" rest))))) -> Some((Roll t, LessThanEqual, fix target), rest)
        | _ -> None
    let (|Branch|_|) = pack <| function
        | Condition((lhs, compare, rhs), Term(b1, Operator ":" (Term(b2, rest)))) -> Some(If {| index = lhs; branches = [{| test = (compare, rhs); consequence = Roll b1; |}]; otherwise = Some (Roll b2) |}, rest)
        | Condition((lhs, compare, rhs), Term(b1, rest)) -> Some(If {| index = lhs; branches = [{| test = (compare, rhs); consequence = Roll b1; |}]; otherwise = None |}, rest)
        | Condition((lhs, compare, rhs), rest) -> Some(If {| index = lhs; branches = [{| test = (compare, rhs); consequence = Literal (Number 1); |}]; otherwise = None |}, rest)
        | _ -> None
    let (|Attack|_|) =
        let (|Mod|_|) = function
            | Int(n, Str "a" rest) -> Some((Max(Dice(1,20), Dice(1,20)), n), rest)
            | Int(n, Str "d" rest) -> Some((Min(Dice(1,20), Dice(1,20)), n), rest)
            | Int(n, rest) -> Some((Dice(1,20), n), rest)
            | _ -> None
        let rec double = function
            | Dice(n,d) -> Dice(2*n, d)
            | Binary(lhs, op, rhs) -> Binary(double lhs, op, double rhs)
            | Max(lhs, rhs) -> Max(double lhs, double rhs)
            | Min(lhs, rhs) -> Min(double lhs, double rhs)
            | External(prop) as v -> v // is it incorrect not to double this? What if it's a die roll? But then again Attack is legacy code anyway, not mainline ribbit...
            | Modifier _ as v -> v
        pack <| function
            | Str "att" (Int(ac, Mod((attackRoll, toHit), Term(dmg, rest)))) -> Some(If {| index = Roll attackRoll; branches = [{| test = (Equal, Literal (Number 20)); consequence = Roll (double dmg); |}; {| test = (GreaterThanEqual, Literal (Number (ac - toHit))); consequence = Roll dmg |}]; otherwise = None |}, rest)
            | _ -> None
    let (|DieOperation|_|) = pack <| function
        | Branch(b, rest) -> Some(b, rest)
        | Attack(a, rest) -> Some(a, rest)
        | Int(n, Str "d" (Int(d, Str "k" (Int(k, rest))))) ->
            Some(BestN(k, List.init n (thunk <| Roll(Domain.Dice.Dice(1,d)))), rest)
        | Term(Modifier n, rest) -> Some(Literal(Number n), rest)
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
    let (|SetProperty|_|) =
        let set ids property e = Command.SetProperty(ids |> List.map (fun id -> (id, property)), e)
        let (|PropertyReference|_|) = ((|PropertyMultiReference|_|) id)
        pack <| function
        | PropertyReference(refs, Operator "=" (DieOperation(e, rest)))-> Some(Command.SetProperty(refs, e), rest)
        | ValidNames(ids, Keyword "has" (DieOperation(e, PropertyName(prop, rest))))-> Some(Command.SetProperty(ids |> List.map (fun id -> (id, prop)), e), rest)
        | ValidNames(ids, Keyword "gains" (DieOperation(e, PropertyName(prop, rest))))-> Some(Command.ChangeProperty(ids |> List.map (fun id -> (id, prop)), e), rest)
        | ValidNames(ids, Word(AnyCase("loses" | "spends"), (DieOperation(e, PropertyName(prop, rest))))) -> Some(Command.ChangeProperty(ids |> List.map (fun id -> (id, prop)), Negate(e)), rest)
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
        | Keyword "add" (Any(name, rest)) -> Some(AddRow name, rest)
        | Keyword "avg" (DieEvaluation(e, rest)) -> Some(Average e, rest)
        | SetProperty(cmd, rest) -> Some(cmd , rest)
        | DieEvaluation(d, rest) as start -> Some(Evaluate(d), rest)
        | _ -> None

