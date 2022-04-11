module Domain.Ribbit.Operations
open Domain.Ribbit.Types
open Domain.Character

// todo: make lazy data possible
let prototypeP = IdProperty("prototype", 0)
let hpP = NumberProperty("HP", shouldntHappen)
let damageTakenP = NumberProperty("DamageTaken", 0)
let acP = NumberProperty("AC", shouldntHappen)
let toHitP = NumberProperty("ToHit", shouldntHappen)
let attacksP = NumberProperty("Attacks", shouldntHappen)
let weaponDamageP = RollProperty("WeaponDamage", shouldntHappen)
let personalNameP = TextProperty("Name", shouldntHappen)
let idP = IdProperty("UniqueId", shouldntHappen)

let nextId(): StateChange<State, Id> = state {
    let! nextId = getF <| fun state -> (defaultArg state.scope.biggestIdSoFar 0) + 1
    do! transform <| fun state -> { state with scope = { state.scope with biggestIdSoFar = Some nextId } }
    return nextId
    }

let addKind (name: Name) initialize = state {
    let! nextId = nextId()
    let! state = get()
    let scope' = { state.scope with biggestIdSoFar = Some nextId } |> initialize nextId
    do! set { state with scope = scope'; kindsOfMonsters = state.kindsOfMonsters |> Map.add name nextId }
    return nextId
    }

let addMonster (kindOfMonster: Name) = state {
    let! nextId = nextId()
    let! kinds, categories = getF (fun st -> st.kindsOfMonsters, st.categories)
    if not <| kinds.ContainsKey kindOfMonster then
        shouldntHappen()
    do! transform (prototypeP.Set(nextId, kinds[kindOfMonster]))
    let rec makeUnique ix =
        // todo, make these personal names more interesting, like "fat troll" vs. "white-spotted troll"
        let candidateName = $"{kindOfMonster} #{ix}"
        match categories |> Map.tryFind kindOfMonster with
        | None -> candidateName, categories |> Map.add kindOfMonster [nextId, candidateName]
        | Some existing ->
            if existing |> List.exists (snd >> (=) candidateName) then
                makeUnique (ix+1)
            else candidateName, categories |> Map.add kindOfMonster ((nextId, candidateName)::existing)
    let personalName, categories' = makeUnique 1
    do! transform (fun state -> { state with categories = categories'; roster = state.roster |> Map.add personalName nextId })
    return personalName
    }

