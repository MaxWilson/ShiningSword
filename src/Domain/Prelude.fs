[<AutoOpen>]
module Domain.Prelude

type Id = int
type ArithmeticOperator = Plus | Minus | Times | Divide
type PropertyName = string
type Key = Id * PropertyName
type Reference = EventRef of Id | PropertyRef of Key

type Comparison = GreaterThan | GreaterThanEqual | LessThan | LessThanEqual | Equal

type Dice<'externalProperty> =
    | Modifier of int
    | Dice of number: int * kind: int
    | External of 'externalProperty
    | Binary of Dice<'externalProperty> * ArithmeticOperator * Dice<'externalProperty>
    | Min of Dice<'externalProperty> * Dice<'externalProperty>
    | Max of Dice<'externalProperty> * Dice<'externalProperty>
    with
    static member toString(d: Dice<'externalProperty>) =
        match d with
        | Modifier n -> n.ToString()
        | Dice.Dice(n,d) -> sprintf "%dd%d" n d
        | External e -> sprintf "%A" e
        | Binary(l, op, r) -> (Dice.toString l) + (match op with Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/") + (Dice.toString r)
        | Min(l, r) -> sprintf "min(%s,%s)" (Dice.toString l) (Dice.toString r)
        | Max(l, r) -> sprintf "max(%s,%s)" (Dice.toString l) (Dice.toString r)
    override this.ToString() = Dice.toString this

type Value = Number of int | Text of string | DiceValue of Dice<Reference> | Nothing | Err of string with
    static member (+) (lhs:Value, rhs:Value) =
        match lhs, rhs with
        | Number l, Number r -> Number(l+r)
        | Text l, Text r -> Text(l+r)
        | DiceValue l, DiceValue r -> Dice.Binary(l, Plus, r) |> DiceValue
        | _ -> Err(sprintf "Could not compute: '%A' + '%A'" lhs rhs)
    static member (-) (lhs:Value, rhs:Value) =
        match lhs, rhs with
        | Number l, Number r -> Number(l+r)
        | Text l, Text r -> Text(l+r)
        | DiceValue l, DiceValue r -> Dice.Binary(l, Minus, r) |> DiceValue
        | _ -> Err(sprintf "Could not compute: '%A' - '%A'" lhs rhs)
    static member Zero = Number 0
    override this.ToString() = match this with Number n -> n.ToString() | Text t -> t | DiceValue d -> Dice.toString d | Nothing -> "Nothing" | Err msg -> sprintf "Error! <<<%s>>>"  msg

type 't Evaluation = Ready of Value | Awaiting of deferral: 't

type Expression =
    | Literal of Value
    | Ref of Reference
    | BinaryOperation of Expression * ArithmeticOperator * Expression
    | If of {| index: Expression; branches: {| test: Comparison * Expression; consequence: Expression|} list; otherwise: Expression option |}
    | Roll of Dice<Reference>
    | BestN of n:int * Expression list
    | Negate of Expression

type TextOrLogExpression = Text of string | LogExpression of text:string * Expression
type Executable =
    | Evaluate of ast:Expression
    | Average of ast:Expression
    | AddRow of name:string
    | ChangeProperty of Key list * Expression
    | SetProperty of Key list * Expression
    | Log of TextOrLogExpression list
type IOCommand = Save of string * export: bool | Load of string * import: bool
type ConsoleCommand =
    | ExecutableCommand of Executable
    | IOCommand of IOCommand
