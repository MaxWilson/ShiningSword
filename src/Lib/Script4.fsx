// HOAS Scheme interpreter from http://www.fssnip.net/2T/title/Scheme-interpreter-in-F
// This LetRec data recursion blows my mind, does it even work?
// Oh, it works because (Lazy x) is equivalent to x.Force.
// And also because a lazy value is still an actual object,
//   can be passed around without being immediately evaluated.
// What did we learn from this? ... I think we may have learned that
//   Ribbit should be FOAS to support the necessary lazy data entry,
//   so we can inject values into the environment when they're available.

// Primitive operations supported by the interpreter
type Prim =
    | Add
    | Sub
    | Mul
    | Div
    | Eq
    | Not

// Recursively defined types for representing values and expressions
type Value =
    | Bool of bool
    | Int of int
    | Lambda of (list<Expr> -> Expr)

and Expr =
    | Apply of Expr * list<Expr>
    | Call of Prim * list<Expr>
    | Const of Value
    | If of Expr * Expr * Expr
    | Let of assign:Expr * body:(Expr -> Expr)
    | LetRec of (Lazy<Expr> -> Expr * Expr)

// Implements primitive operations
let Op prim =
    match prim with
    | Add ->
        fun [Int x; Int y] -> Int (x + y)
    | Sub ->
        fun [Int x; Int y] -> Int (x - y)
    | Mul ->
        fun [Int x; Int y] -> Int (x * y)
    | Div ->
        fun [Int x; Int y] -> Int (x / y)
    | Eq  ->
        function
        | [Int x; Int y] -> Bool (x = y)
        | [Bool x; Bool y] -> Bool (x = y)
    | Not -> fun [Bool x] ->
        Bool (not x)

// Pattern for recognizing binary expressions 
let (|Binary|_|) (expr: Expr) =
    match expr with
    | Call (p, [x; y]) -> Some (p, x, y)
    | _                -> None

// Recursive evaluation of the expression to get a value
// (calls itself to evaluate sub-expressions)
let rec Eval (expr: Expr) : Value =
    match expr with
    | Apply (f, xs) ->
        match Eval f with
        | Lambda f ->
            Eval (f xs)
    | Call (p, xs) ->
        Op p (List.map Eval xs)
    | Const x ->
        x
    | If (x, y, z) ->
        match Eval x with
        | Bool true  -> Eval y
        | Bool false -> Eval z
    | Let (x, f) ->
        Eval (f (Const (Eval x)))
    | LetRec f ->
        let rec x = lazy fst pair
        and body  = snd pair
        and pair  = f x
        Eval body

// Simple factorial function four our Scheme interpreter
let Fac n =
    let i x = Const (Int x)
    let ( =? ) a b = Call (Eq, [a; b])
    let ( *? ) a b = Call (Mul, [a; b])
    let ( -? ) a b = Call (Sub, [a; b])
    let ( ^^ ) f x = Apply (f, [x])
    LetRec <| fun fac ->
        let fac =
            fun [x] ->
                let (Lazy fac) = fac
                If (x =? i 0, i 1, x *? (fac ^^ (x -? i 1)))
            |> Lambda
            |> Const
        (fac, fac ^^ n)

// show how to use let, binding Fac to the F# parameter factorial
Eval (
    Let(
        Const (Lambda(fun [arg] -> Fac arg)),
        fun factorial -> Apply(factorial, [Const (Int 10)])))
