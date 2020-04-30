module Arch
open Optics

type Todo() =
    do notImpl()
type Id = int
type Roll = Todo
type Expression = Todo
type Statement = Todo
type PropertyId = Id
type PropertyDefinition = Todo
type Ref = Todo
type Value = Todo
type PropertyKey = Id * PropertyId
type AffordanceChoice = Todo
type IncrementId<'state, 'id> = Lens<'state, 'id> -> 'state -> 'state * 'id
type BlockableComputation<'t> = Complete of 't | BlockedOn of Ref
type GameState = {
    lastCreatureId: Id option
    properties: Map<PropertyKey, Value>
    propertyDefinitions: PropertyDefinition array
    }
type IntermediateGameState = {
    game: GameState
    processingQueue: Ref list
    }
type IterateToFixedPoint = IntermediateGameState -> GameState
type BehaviorChoosesAffordance<'behavior, 'behaviorState, 'affordanceChoice> = 'behavior * 'behaviorState * GameState -> BlockableComputation<'behaviorState * 'affordanceChoice>
type AffordanceTriggersAction = Todo

