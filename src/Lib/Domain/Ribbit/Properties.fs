module Domain.Ribbit.Properties

[<AutoOpen>]
module RValue =
    type Id = string
    type Condition =
        | GreaterThan of Expr * value: Expr
        | LessThan of Expr * value: Expr
        | Equals of Expr * value: Expr
    and Expr =
        | Condition of Condition
        | Deref of Id
        | PropertyRef of string
        | Const of int
    type 't RDelta = 't * string
    type 't RValue = { // runtimevalue/rvalue, something that a property can be evaluated to be currently
        baseValue: 't
        description: string option
        modifiers: ('t RDelta) list
        }

[<AutoOpen>]
module LValue =
    type 't Delta =
        | Delta of 't RDelta
        | ConditionalDelta of ('t RDelta) * Condition
    // lvalue, something that an attribute can have
    type 't Primary = {
        baseValue: 't
        description: string option
        modifiers: ('t Delta) list
        }
        with member this.clear = { this with modifiers = [] }
    type 't Secondary = { modifiers: ('t Delta) list }
        with member this.clear = { this with modifiers = [] }

type Create =
    static member primary(baseValue, ?description, ?modifiers) =
        {   Primary.baseValue = baseValue;
            description = description;
            modifiers = modifiers |> Option.map (List.map Delta) |> Option.defaultValue [] }
    static member secondary(?mods) =
       { modifiers = List.map Delta (defaultArg mods []) }
    static member plus(lhs: 't Primary, rhs: ('t Delta) list) =
        { lhs with modifiers = rhs@lhs.modifiers }
    static member plus(lhs: 't Secondary, rhs: ('t Delta) list) =
        { modifiers = rhs@lhs.modifiers }
    static member plus(lhs: 't RValue, rhs: ('t RDelta) list) =
        { lhs with modifiers = (rhs@lhs.modifiers) }
    static member plus(lhs: ('t Delta) list, rhs: 't Primary) =
        Create.plus(rhs, lhs)
    static member plus(lhs: ('t Delta) list, rhs: 't Secondary) =
        Create.plus(rhs, lhs)
    static member plus(lhs: ('t RDelta) list, rhs: 't RValue) =
        Create.plus(rhs, lhs)

open type Create

// so maybe it's really a lens? But if so, it's a different kind of lens for HP than for ST: ST is not derived from anything but HP is
type Eval =
    static member eval (arg: 't Delta) =
        match arg with
        | Delta(v,d) -> v,d
        | ConditionalDelta(v,c) -> notImpl()
    static member eval (arg: 't Primary) : 't RValue =
        {   baseValue = arg.baseValue
            description = arg.description
            modifiers = arg.modifiers |> List.map Eval.eval
            }
    static member eval (lhs: 't RValue, rhs: 't Secondary) : 't RValue =
        Create.plus(lhs, rhs.modifiers |> List.map Eval.eval)
    static member sum (arg: int RValue) =
        arg.baseValue + (arg.modifiers |> List.sumBy fst)
    static member sum (arg: float RValue) =
        arg.baseValue + (arg.modifiers |> List.sumBy fst)

module Primary =
    let plus n because (attr: _ Primary) = Create.plus(attr, [Delta(n, because)])
module Secondary =
    let plusSecondary n because (attr: _ Secondary) = Create.plus(attr, [Delta(n, because)])

