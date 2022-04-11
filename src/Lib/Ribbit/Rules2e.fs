module Domain.Ribbit.Rules2e
open Domain.Ribbit.Types
open Domain.Ribbit.Operations
open Domain.Character.ADND2nd

let traitsP = FlagsProperty<Trait>("Traits", notImpl)

let s = Scope.fresh |> toHitP.Set(1, 20) |> acP.Set(1, 5) |> traitsP.Set(1, SwordBowBonus 1, true)

type Monster = {
    name: string
    }
let load (monster:Monster) (scope: Scope) =

s |> toHitP.Get(1)
s |> acP.Get(1)
s |> toHitP.Get(2)
s |> acP.Get(2)
s |> traitsP.Get(1, WeaponSpecialist)
s |> traitsP.Get(1, SwordBowBonus 1)

let lookup personalName f =
    getF (fun (state: State) -> f state.roster[personalName] state)
withState State.fresh (state {
    let! kobold = addKind "Kobold" (fun id scope -> scope)
    let! k1 = addMonster "Kobold"
    let! k2 = addMonster "Kobold"
    let! k3 = addMonster "Kobold"
    let! orc = addKind "Orc" (fun id scope -> scope)
    let! m4 = addMonster "Orc"
    do! transform (hpP.Set (orc, 15))
    let! hp = (lookup m4 hpP.Get)
    return k1, k2, k3, m4, hp
    })
