[<AutoOpen>]
module Domain.Prelude

// Here's where primitive types go that were refactored out of multiple other modules.
// Nothing should start out here. Only promote to here if it makes the types simpler.
type Id = int
type ArithmeticOperator = Plus | Minus | Times | Divide
