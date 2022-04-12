module Domain.Adventure
open Domain.Character
open Domain.Character.Universal

type Encounter = {
    description: string
    monsters: (Name * RollSpec option) list
    }

type OngoingEncounter = {
    monsters: (Name * int) list
    isFinished: bool
    }

type AdventureSpec = {
    description: string
    encounters: Encounter list
    allies: Domain.Character.Universal.CharacterSheet list
    bonusXP: int
    bonusGP: int
    }
    with static member fresh description encounters = { bonusXP = 0; bonusGP = 0; allies = []; description = description; encounters = encounters  }

type AdventureState = {
    mainCharacter: CharacterSheet
    currentEncounter: OngoingEncounter option
    scheduledEncounters: Encounter list
    ribbit: Ribbit.Types.State
    }

let downtime sheet =
    { mainCharacter = sheet; currentEncounter = None; scheduledEncounters = []; ribbit = Ribbit.Types.State.fresh }

let embark (spec: AdventureSpec) sheet =
    { mainCharacter = sheet; currentEncounter = None; scheduledEncounters = spec.encounters; ribbit = Ribbit.Types.State.fresh }

let finishAdventure (spec: AdventureSpec) state =
    let mainCharacter' =
        state.mainCharacter.map2e(fun char -> { char with xp = char.xp + 1<xp>*spec.bonusXP; wealth = char.wealth + 1<gp>*spec.bonusGP })
            .map5e(fun char -> { char with xp = char.xp + 1<xp>*spec.bonusXP; wealth = char.wealth + 1<gp>*spec.bonusGP })
    let msg =
        if spec.bonusXP > 0 && spec.bonusGP > 0 then
            $"You gain an extra {spec.bonusXP} XP and {spec.bonusGP} gold pieces for completing the adventure!"
        elif spec.bonusXP > 0 then
            $"You gain an extra {spec.bonusXP} XP for completing the adventure!"
        elif spec.bonusGP > 0 then
            $"You gain an extra {spec.bonusGP} gold pieces for completing the adventure!"
        else
            "Congratulations on a successful adventure!"
    msg, { state with mainCharacter = mainCharacter' }

let toOngoing (encounter:Encounter) =
    let rollQty = function
    | name, Some (roll:RollSpec) -> name, roll.roll()
    | name, None ->
        // we're pulling default encounter sizes from the 2E ruleset, but since 5E doesn't have Number Appearing as a stat, that's okay for now.
        // Eventually we will have 5E-only monsters and then we'll have to figure something out. For now we just defensively default to 1 in that case.
        let qty = match Domain.Ribbit.Rules2e.monsterKinds |> Map.tryFind name with
                    | Some k -> k.numberAppearing.roll()
                    | None -> 1
        name, qty
    {
        monsters = encounter.monsters
            |> List.map rollQty
        isFinished = false
    }

let beginEncounter next rest state =
    { state with scheduledEncounters = rest; currentEncounter = Some next }

let victory (encounter:OngoingEncounter) state =
    let sheet = state.mainCharacter
    let xp, treasureDescription, sheet =
        match sheet with
        | Detail2e sheet ->
            // XP rewards are different in AD&D vs. 5e
            let xpReward =
                let getReward(monsterKind, qty) =
                    Domain.Ribbit.Rules2e.monsterKinds[monsterKind].xp * qty
                encounter.monsters |> List.map getReward |> List.sum
            let gpReward, treasureDescription =
                let getTreasure(monsterKind, qty) =
                    let monsterKind = Domain.Ribbit.Rules2e.monsterKinds[monsterKind]
                    monsterKind.lairTreasure@(List.replicate qty (monsterKind.treasureType) |> List.collect id)
                encounter.monsters |> List.collect getTreasure
                |> Domain.Ribbit.Operations.treasureValue
            xpReward, treasureDescription, { sheet with xp = sheet.xp + xpReward * 1<xp>; wealth = sheet.wealth + gpReward } |> Detail2e
        | Detail5e sheet ->
            let xpReward =
                let getReward(monsterKind, qty) =
                    Domain.Ribbit.Rules5e.monsterKinds[monsterKind].xp * qty
                encounter.monsters |> List.map getReward |> List.sum
            let gpReward, treasureDescription =
                let getTreasure(monsterKind, qty) =
                    let monsterKind = Domain.Ribbit.Rules5e.monsterKinds[monsterKind]
                    monsterKind.lairTreasure@(List.replicate qty (monsterKind.treasureType) |> List.collect id)
                encounter.monsters |> List.collect getTreasure
                |> Domain.Ribbit.Operations.treasureValue
            xpReward, treasureDescription, { sheet with xp = sheet.xp + xpReward * 1<xp> } |> Detail5e
    { state with mainCharacter = sheet }, $"You earn {xp} XP! As for material rewards... you find {treasureDescription}."

let easy() =
    [
    AdventureSpec.fresh
        "You hire on as a caravan guard."
        [
            {
                description = "One night, kobolds attack! Your companions cravenly flee but you fight bravely."
                monsters = ["Kobold", Some (RollSpec.create(1,6))]
                }
            ]
    AdventureSpec.fresh
        "You go to visit your brother."
        [
            {
                description = "You are attacked by wolves on the Connecticut turnpike!"
                monsters = ["Wolf", Some (RollSpec.create(1,6))]
                }
            ]
    ]
    |> chooseRandom

let hard() =
    [
    AdventureSpec.fresh
        "You are hungry for honey. You decide to go get some."
        [
            {
                description = "Bears don't like it when people steal honey from their trees!"
                monsters = ["Black Bear", Some (RollSpec.create(1,3))]
                }
            ]
    AdventureSpec.fresh
        "You are hungry for honey. You decide to go get some."
        [
            {
                description = "Owlbears don't like it when people steal honey from their trees!"
                monsters = ["Owlbear", None]
                }
            ]
    { AdventureSpec.fresh
        "An animal trainer is offering a 1000 gp reward for owlbear eggs."
        [
            {
                description = "You find owlbear eggs. Unfortunately the eggs are guarded!"
                monsters = ["Owlbear", Some (RollSpec.create(1,2))]
                }
            ]
        with bonusGP = 1000
        }
    ]
    |> chooseRandom

let deadly() =
    [
    AdventureSpec.fresh
        "Rumors say the kobolds are growing restless"
        [
            {
                description = "Kobolds attack you in your home!"
                monsters = ["Kobold", Some (RollSpec.create(2,4))]
                }
            {
                description = "Kobolds attack the town!"
                monsters = ["Kobold", None]
                }
            ]
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble."
        [
            {
                description = "The hill giants are not pleased to see you!"
                monsters = ["Hill Giant", None]
                }
            ]
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble."
        [
            {
                description = "The frost giants are not pleased to see you!"
                monsters = ["Frost Giant", None]
                }
            ]
    ] |> chooseRandomExponentialDecay 0.5
