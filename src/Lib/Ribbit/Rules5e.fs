module Domain.Ribbit.Rules5e
open Domain.Ribbit.Types
open Domain.Ribbit.Operations
open Domain.Character.DND5e

let traitsP = FlagsProperty<Trait>("Traits", notImpl)
