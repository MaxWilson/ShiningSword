module Domain.Ribbit.Types

open Domain.Character

// todo, make this more generic using existing prototypes
type Scope = {
    hp: Map<Name, int>
    damage: Map<Name, int>
    }
    with static member fresh = { hp = Map.empty; damage = Map.empty }

type State = {
    rows: Scope
    }
    with static member fresh = { rows = Scope.fresh }
