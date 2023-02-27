// This script is purely for messing around with graph visualization.
#I __SOURCE_DIRECTORY__
#I ".."
#I "..\Core"
#load @"Optics.fs"
#load @"Common.fs"

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
    type 't Secondary = { modifiers: ('t Delta) list }
type Create =
    static member primary(baseValue, ?description, ?modifiers) =
        {   Primary.baseValue = baseValue;
            description = description;
            modifiers = modifiers |> Option.map (List.map Delta) |> Option.defaultValue [] }
    static member secondary(expr, ?descr, ?mods) =
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

type Ctx = { ST: int Primary; HP: int Secondary }

// now, how do we make sure we can eval either a whole forest or just a single tree? Different eval
// functions? Maybe a forest is a Map<Id, Ctx> and you just lookup the right Id and then eval that?

// is a property a function? If so what's its signature?
// Is it a lens?

// a property is capable of taking a ctx and returning a rvalue.
let ST' ctx = ctx.ST

// but a property also needs to be capable of being set! Temporary bonuses/penalties, permanent value changes.
module Change =
    let ST f ctx = { ctx with ST = f ctx.ST }
    let HP f ctx = { ctx with HP = f ctx.HP }
let exampleCtx = { ST = primary 10; HP = secondary(PropertyRef "ST", mods=[+3, "Dwarven HP"]) }

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

let plusPrimary n because (attr: _ Primary) =
    { attr with modifiers = Delta(n,because)::attr.modifiers }
let plusSecondary n because (attr: _ Secondary) =
    { attr with modifiers = Delta(n,because)::attr.modifiers }

open type Eval

let ST ctx = ctx.ST |> eval
let HP ctx = eval (ST ctx, ctx.HP)

let sum = function
    | { RValue.baseValue = a; modifiers = b } -> a + (b |> List.sumBy fst)

(exampleCtx |> HP |> sum)
    = 13
(exampleCtx |> Change.HP (plusSecondary 5 "Purchased") |> HP |> sum)
    = 13

// resolved: you can change ATTRIBUTES, not properties, but properties are derived from attributes

