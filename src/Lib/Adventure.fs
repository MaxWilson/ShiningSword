module Domain.Adventure
open Domain.Character
open Domain.Character.Universal
open Domain.Ribbit.Operations
open Domain.Ribbit.Operations.Treasure
open Domain.Ribbit

type Encounter = {
    description: string
    monsters: (Name * RollSpec option) list
    }

type OngoingEncounter = {
    monsters: (Name * int) list
    outcome: FightResult
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
    allies: Domain.Character.Universal.CharacterSheet list
    currentEncounter: OngoingEncounter option
    scheduledEncounters: Encounter list
    ribbit: Ribbit.State
    }

let loadCharacters (characters: CharacterSheet list) (adventureState: AdventureState) =
    let addCharacter state' ribbit =
        match ribbit with
        | Detail2e (char: CharacterSheet2e) ->
            state {
                let! id = addCharacterToRoster char.name
                do! hpP.SetM(id, char.hp |> Array.sumBy(fun (lhs, rhs) -> lhs + rhs))
                do! acP.SetM(id, char.ac)
                do! numberOfAttacksP.SetM(id, char.attacks)
                do! toHitP.SetM(id, char.toHitBonus)
                do! weaponDamageP.SetM(id, [char.damage])
                do! Domain.Ribbit.Rules5e.traitsP.SetAllM(id, char.traits.summary |> Set.map string)
                }
        | Detail5e (char: CharacterSheet5e) ->
            state {
                let! id = addCharacterToRoster char.name
                do! hpP.SetM(id, char.hp |> Array.sumBy(fun (lhs, rhs) -> lhs + rhs))
                do! acP.SetM(id, char.ac)
                do! numberOfAttacksP.SetM(id, 1)
                do! toHitP.SetM(id, char.toHit)
                do! weaponDamageP.SetM(id, [char.damage])
                do! Domain.Ribbit.Rules5e.initBonusP.SetM(id, char.Dex |> DND5e.statBonus)
                do! Domain.Ribbit.Rules5e.traitsP.SetAllM(id, char.traits.summary |> Set.map string)
                }
        |> runNoResult state'
    { adventureState with ribbit = characters |> List.fold addCharacter adventureState.ribbit }

let downtime sheet =
    { mainCharacter = sheet; allies = []; currentEncounter = None; scheduledEncounters = []; ribbit = Ribbit.State.fresh }

let embark (spec: AdventureSpec) sheet =
    { mainCharacter = sheet; allies = spec.allies; currentEncounter = None; scheduledEncounters = spec.encounters; ribbit = Ribbit.State.fresh }
    |> loadCharacters (sheet::spec.allies)

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
        outcome = Ongoing
    }

let beginEncounter (next: OngoingEncounter) rest (adventureState: AdventureState) =
    let ribbit =
        state {
            for monsterKind, qty in next.monsters do
                if adventureState.mainCharacter.isADND then
                    do! Domain.Ribbit.Rules2e.createByName monsterKind qty
                else
                    do! Domain.Ribbit.Rules5e.createByName monsterKind qty
            }
        |> runNoResult adventureState.ribbit
    { adventureState with scheduledEncounters = rest; currentEncounter = Some next; ribbit = ribbit }

let victory (encounter:OngoingEncounter) state =
    let sheet = state.mainCharacter
    let divisor = 1 + state.allies.Length
    let xp, gpReward, treasureDescription, state' =
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
                |> treasureValue
            let reward (char: CharacterSheet) = char.map2e(fun char -> { char with xp = char.xp + (xpReward/divisor*1<xp>); wealth = char.wealth + (gpReward/divisor) })
            xpReward, gpReward, treasureDescription, { state with mainCharacter = state.mainCharacter |> reward; allies = state.allies |> List.map reward }
        | Detail5e sheet ->
            let divisor = 1 + state.allies.Length
            let xpReward =
                let getReward(monsterKind, qty) =
                    Domain.Ribbit.Rules5e.monsterKinds[monsterKind].xp * qty
                encounter.monsters |> List.map getReward |> List.sum
            let gpReward, treasureDescription =
                let getTreasure(monsterKind, qty) =
                    let monsterKind = Domain.Ribbit.Rules5e.monsterKinds[monsterKind]
                    monsterKind.lairTreasure@(List.replicate qty (monsterKind.treasureType) |> List.collect id)
                encounter.monsters |> List.collect getTreasure
                |> treasureValue
            let reward (char: CharacterSheet) = char.map5e(fun char -> { char with xp = char.xp + (xpReward/divisor*1<xp>); wealth = char.wealth + (gpReward/divisor) })
            xpReward, gpReward, treasureDescription, { state with mainCharacter = state.mainCharacter |> reward; allies = state.allies |> List.map reward }
    if divisor = 1 then
        state', $"You earn {xp} XP! As for material rewards... you find {treasureDescription}."
    elif gpReward = 0<gp> then
        state', $"You earn {xp} XP! ({xp/divisor} XP each.) As for material rewards... you find {treasureDescription}."
    else
        state', $"You earn {xp} XP! ({xp/divisor} XP each.) As for material rewards... you find {treasureDescription} and split it amongst yourselves ({gpReward/divisor} gp each)."

// for 2E this is just "execute everybody's declared actions", for 5E it could be the same or it could be "fight until you get to a PC's turn" depending on the settings
let fightUntilFixedPoint (adventureState: AdventureState) =
    let result =
        if adventureState.mainCharacter.isADND then
            adventureState.ribbit |> Domain.Ribbit.Rules2e.fightOneRound
        else
            adventureState.ribbit |> Domain.Ribbit.Rules5e.fightUntilFixedPoint
    let adv = {
        adventureState
        with
            currentEncounter = adventureState.currentEncounter |> Option.map (fun e -> { e with outcome = result.outcome })
            ribbit = result.ribbit
        }
    match result.outcome with
    | Victory ->
        let adv, msg = victory adv.currentEncounter.Value adv
        result.outcome, result.msgs@["Victory!!!";msg], adv
    | Defeat ->
        result.outcome, result.msgs@["You have been defeated!!! The worms now feast on your flesh."], adv
    | _ ->
        result.outcome, result.msgs, adv

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
