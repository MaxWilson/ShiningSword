module Rewrite

#I "."
#I ".."
#load "Optics.fs"
#load "Common.fs"

(*
Concepts: Expression eval

*)

type RuntimeValue = String of string | Number of int | Boolean of bool | Undefined
type AgentId = int
type EventId = int
type PropertyName = string
type VariableReference = DataRef of agent: AgentId * property: PropertyName | LocalRef of guid: string | EventRef of event:EventId

type Expression =
    | Const of RuntimeValue
    | BinaryOp of Expression * Expression * BinaryOperator
    | Conditional of test: Expression * andThen: Expression * orElse: Expression option
    | Dereference of VariableReference
    | Roll of n:int * dSize: int * plus: int
    | StartEvent of eventName: string * args: (PropertyName * Expression) list

and BinaryOperator =
    | Plus
    | Minus
    | Times
    | Divide
    | Equals
    | AtLeast
    | AtMost

type CurrentExpressionValue = Result<RuntimeValue, VariableReference list>

let evaluate
    (api: {|
            dereference: VariableReference -> 'state -> CurrentExpressionValue;
            progress: VariableReference -> 'state -> 'state
        |})
    (state: 'state)
    expr
    : CurrentExpressionValue =
        let binary = function
        | String lhs, String rhs, Plus -> lhs + rhs |> String
        | Number lhs, Number rhs, Plus -> lhs + rhs |> Number
        | Boolean lhs, Boolean rhs, Plus -> (lhs || rhs) |> Boolean
        | Number lhs, Number rhs, Minus -> lhs - rhs |> Number
        | Number lhs, Number rhs, Times -> lhs * rhs |> Number
        | Boolean lhs, Boolean rhs, Times -> (lhs && rhs) |> Boolean
        | Number lhs, Number rhs, Divide -> lhs / rhs |> Number
        | Number lhs, Number rhs, Equals -> (lhs = rhs) |> Boolean
        | String lhs, String rhs, Equals -> (lhs = rhs) |> Boolean
        | Boolean lhs, Boolean rhs, Equals -> lhs = rhs |> Boolean
        | Number lhs, Number rhs, AtLeast -> lhs >= rhs |> Boolean
        | Number lhs, Number rhs, AtMost -> lhs <= rhs |> Boolean
        | _ -> Undefined

        let rec eval : Expression -> CurrentExpressionValue=
            function
            | Const v -> Ok v
            | BinaryOp(lhs, rhs, op) ->
                match eval lhs, eval rhs with
                | Ok lhs, Ok rhs -> binary(lhs, rhs, op) |> Ok
                | Error _ as lhs, Ok _ -> lhs
                | Ok _, (Error _ as rhs) -> rhs
                | Error lhs, Error rhs -> Error (lhs @ rhs)
            | Conditional(test, andThen, orElse) ->
                match eval test with
                | Ok (Boolean true) -> eval andThen
                | Ok (Boolean false) ->
                    match orElse with
                    | Some expr -> eval expr
                    | _ -> Ok Undefined
                | Ok _ -> Ok Undefined
                | err -> err
            | Dereference ref ->
                api.dereference ref state
            | Roll _ | StartEvent _ ->
                // By evaluation time, Roll and StartEvent should have already been rewritten to Dereference expressions
                Ok Undefined 
        eval expr
