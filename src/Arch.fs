module Arch
open Optics

type Todo() =
    do notImpl()
type Id = int // an Id is something which is unique within a given context
type GenerateId<'state> = Lens<'state, Id> -> 'state -> Id * 'state
type Insert<'state, 'value> = Lens<'state, Id> -> Lens<'state, Map<Id, 'value>> -> 'value -> 'state -> Id * 'state
type Roll = Todo
type Expression = Todo
type Statement = Todo
type PropertyId = Id
type PropertyDefinition = Todo
type Address = Todo
type Value = Todo
type PropertyKey = Id * PropertyId
type AffordanceChoice = Todo
type BlockableComputation<'t, 'addr> = Complete of 't | BlockedOn of 'addr
type GameState = {
    lastCreatureId: Id option
    properties: Map<PropertyKey, Value>
    propertyDefinitions: PropertyDefinition array
    }
type IntermediateGameState = {
    game: GameState
    processingQueue: Address list
    }
type IterateToFixedPoint = IntermediateGameState -> GameState
type BehaviorChoosesAffordance<'state, 'affordance, 'addr> = 'state -> BlockableComputation<'state * 'affordance, 'addr>
type AffordanceTriggersAction = Todo

