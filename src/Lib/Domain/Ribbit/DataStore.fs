/// operations related to raw data, such as evaluating expressions and statements. Prerequisite to Ribbit Core.
[<AutoOpen>]
module Domain.Ribbit.DataStore
open Domain
open Domain.Random
open Domain.Character
open Delta

type Id = int

type RuntimeType = Number | Id  | Text | Roll | Rolls | Flags | Bool | Generic
type RuntimeValue = Number of int | Id of Id | Text of string | Roll of RollSpec | Rolls of RollSpec list | Flags of string Set| Bool of bool | Generic of obj
type Address = LocalAddress of variableName: Name | PropertyAddress of Id * propertyName: Name
type RibbitRequest = DataRequest of Id * propertyName: Name
type RibbitError = Awaiting of RibbitRequest | BugReport of msg: string

type Row = Map<Name, RuntimeValue>

type 'Ribbit ExecutionContext = {
    agent: Id option // the creature, if any, which is acting to create this expression. E.g. the attacker during an attack.
    target: Id option // the creature, if any, which is the target of the action.
    locals: Row // e.g. arguments to the event within which the expression is embedded
    instructionPointer: int
    state: 'Ribbit
    }

module ExecutionContext =
    let Create state = { locals = Map.empty; instructionPointer = 0; state = state; agent = None; target = None }
    let TransformM f ctx = (), { ctx with state = ctx.state |> f }

type 'Ribbit EvaluationContext = {
    locals: Row // e.g. arguments to the event within which the expression is embedded
    ribbit: 'Ribbit
    }

module EvaluationContext =
    let Create(state) = { locals = Map.empty; ribbit = state }

type RValue<'t> = Result<'t, RibbitError>
type Evaluation<'t, 'ribbit> = 'ribbit EvaluationContext -> RValue<'t> // In this context, rvalue = something that can be bound to a let! variable. The Evaluation is the thing that we might be able to use to create the RValue, or else a RibbitError like Awaiting DataRequest
type ExecutionWithResult<'ribbit, 'result> = StateChange<'ribbit ExecutionContext, RValue<'result>>
type 'ribbit Execution = ExecutionWithResult<unit, 'ribbit>
type 'ribbit AtomicChange = StateChange<'ribbit ExecutionContext, unit>

type Scope = {
    rows: Map<Id, Row>
    biggestIdSoFar: Id option
    }
    with static member fresh = { rows = Map.empty; biggestIdSoFar = None }

type IProperty =
    abstract Name: string
    abstract Type: RuntimeType

type [<AbstractClass>] Property<'t, 'DataSource>(name, runtimeType) =
    interface IProperty with
        member this.Name = name
        member this.Type = runtimeType
    abstract Get: Id -> 'DataSource -> 't
    abstract GetM: Id -> Evaluation<'t, 'DataSource>
    abstract Set: Id *'t -> 'DataSource -> 'DataSource
    member this.SetM(rowId, value) : StateChange<'DataSource, unit> =
        stateChange {
            do! (fun ribbit -> (), ribbit |> this.Set(rowId, value))
            }
    member this.SetContextM(rowId, value) : ExecutionWithResult<'DataSource, unit> =
        stateChange {
            do! ExecutionContext.TransformM (this.Set(rowId, value))
            return Ok ()
            }

type [<AbstractClass>] GlobalProperty<'t, 'DataSource>(name, runtimeType) =
    interface IProperty with
        member this.Name = name
        member this.Type = runtimeType
    abstract Get: 'DataSource -> 't
    abstract GetM: Evaluation<'t, 'DataSource>
    abstract Set: 't -> 'DataSource -> 'DataSource
    member this.SetM(rowId, value) : StateChange<'DataSource, unit> =
        stateChange {
            do! (fun ribbit -> (), ribbit |> this.Set(value))
            }
    member this.SetContextM(rowId, value) : ExecutionWithResult<'DataSource, unit> =
        stateChange {
            do! ExecutionContext.TransformM (this.Set(value))
            return Ok ()
            }

