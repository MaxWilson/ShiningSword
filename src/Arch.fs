module Arch

type Id = int
type Roll = exn
type Expression = exn
type Statement = exn
type PropertyId = Id
type PropertyDefinition = exn
type Ref = exn
type Value = exn
type PropertyKey = Id * PropertyId
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
