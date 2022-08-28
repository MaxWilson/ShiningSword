module Domain.Adventure
open Domain.Metrics
open Domain.Random
open Domain.Character
open Domain.Character.Universal
open Domain.Ribbit.Operations
open Domain.Treasure
open Domain.Ribbit

type Encounter = {
    description: string
    monsters: (Name * RollSpec option) list
    inLair: bool
    }
    with
    static member wandering description monsters = { description = description; monsters = monsters; inLair = false }
    static member lair description monsters = { description = description; monsters = monsters; inLair = true }

type OngoingEncounter = {
    monsters: (Name * int) list
    outcome: FightResult
    inLair: bool
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
    ribbit: Ribbit
    }

let loadCharacters (characters: CharacterSheet list) (adventureState: AdventureState) =
    let addCharacter state' ribbit =
        match ribbit with
        | Detail2e (char: CharacterSheet2e) ->
            stateChange {
                let! id = addCharacterToRoster char.name
                do! hpP.SetM(id, char.hp |> Array.sumBy(fun (lhs, rhs) -> lhs + rhs))
                do! acP.SetM(id, char.ac)
                do! numberOfAttacksP.SetM(id, char.attacks)
                do! toHitP.SetM(id, char.toHitBonus)
                do! weaponDamageP.SetM(id, [char.damage])
                do! Domain.Ribbit.Rules5e.traitsP.SetAllM(id, char.traits.summary |> Set.map string)
                }
        | Detail5e (char: CharacterSheet5e) ->
            stateChange {
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
    { mainCharacter = sheet; allies = []; currentEncounter = None; scheduledEncounters = []; ribbit = Ribbit.Fresh }

let embark (spec: AdventureSpec) sheet =
    { mainCharacter = sheet; allies = spec.allies; currentEncounter = None; scheduledEncounters = spec.encounters; ribbit = Ribbit.Fresh }
    |> loadCharacters (sheet::spec.allies)

let clearEnemies adventureState =
    let ribbit =
        stateChange {
            let! ribbit = Ribbit.GetM id
            let friendlies = ribbit.roster |> Map.filter (fun name id -> isFriendlyP.Get id ribbit)
            // clear enemies from last encounter off the UI because they're all dead
            do! SetRoster friendlies |> Ribbit.ExecuteM
            }
        |> adventureState.ribbit.transform
    { adventureState with ribbit = ribbit }

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
    msg, { (state |> clearEnemies) with mainCharacter = mainCharacter' |> levelUp; allies = state.allies |> List.map levelUp }

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
        inLair = encounter.inLair
    }

let beginEncounter (next: OngoingEncounter) rest (adventureState: AdventureState) =
    let ribbit =
        stateChange {
            let! ribbit = Ribbit.DataM
            for monsterKind, qty in next.monsters do
                if adventureState.mainCharacter.isADND then
                    do! Domain.Ribbit.Rules2e.createByName monsterKind qty
                else
                    do! Domain.Ribbit.Rules5e.createByName monsterKind qty
            }
        |> adventureState.ribbit.transform
    { (adventureState |> clearEnemies) with scheduledEncounters = rest; currentEncounter = Some next; ribbit = ribbit }

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
                    (if encounter.inLair then monsterKind.lairTreasure else [])@(List.replicate qty (monsterKind.treasureType) |> List.collect id)
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
                    (if encounter.inLair then monsterKind.lairTreasure else [])@(List.replicate qty (monsterKind.treasureType) |> List.collect id)
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
    |> fun (state, msg) -> state, LogEntry.create(msg, true, Good)

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
        result.outcome, result.msgs@[LogEntry.create("Victory!!!", true, Good);msg], adv
    | Defeat ->
        result.outcome, result.msgs@[LogEntry.create("You have been defeated!!! The worms now feast on your flesh.", true, Bad)], adv
    | _ ->
        result.outcome, result.msgs, adv

let easy ruleSet =
    [
    AdventureSpec.fresh
        "You hire on as a caravan guard."
        [
            Encounter.wandering "One night, kobolds attack! Your fellow guards cravenly flee but you fight bravely."
                ["Kobold", Some (RollSpec.create(1,3))]
            ]
    AdventureSpec.fresh
        "You go to visit your brother."
        [
            Encounter.wandering "You are attacked by wolves on the Connecticut turnpike!"
                ["Wolf", Some (RollSpec.create(1,2))]
            ]
    AdventureSpec.fresh
        "You go on a safari hunting antelopes in the Pridelands."
        [
            Encounter.wandering "Vicious jackals attack you!"
                ["Jackal", Some (RollSpec.create(1,4,1))]
            ]
    AdventureSpec.fresh
        "Weird noises are coming from a widow's back yard."
        [
            Encounter.lair "When you enter the yard, rabid porcupines attack!"
                ["Porcupine", Some (RollSpec.create(1,2,1))]
            ]
    ]
    |> chooseRandom

let hard ruleSet =
    [
    AdventureSpec.fresh
        "You hire on as a caravan guard."
        [
            Encounter.wandering "One night, kobolds attack! Your fellow guards cravenly flee but you fight bravely!"
                ["Kobold", Some (RollSpec.create(2,6))]
            ]
    AdventureSpec.fresh
        "You hire on as a caravan guard."
        [
            Encounter.wandering "One night, kobolds attack! Your fellow guards betray you and fight with the kobolds!"
                ["Kobold", Some (RollSpec.create(2,6)); "Guard", Some(RollSpec.create(1,4))]
            ]
    AdventureSpec.fresh
        "You hire on as a caravan guard."
        [
            Encounter.wandering "One night, hobgoblins attack! Your fellow guards cravenly flee but you fight bravely."
                ["Hobgoblin", Some (RollSpec.create(2,6))]
            ]
    AdventureSpec.fresh
        "You hire on as a caravan guard."
        [
            Encounter.wandering "As you near Venture Pass, hill giants attack! Your fellow guards cravenly flee but you steel your nerves and fight."
                ["Hill Giant", Some (RollSpec.create(1,2))]
            ]
    AdventureSpec.fresh
        "You go to visit your brother."
        [
            Encounter.wandering "You are attacked by many wolves on the Connecticut turnpike!"
                ["Wolf", Some (RollSpec.create(2, 4))]
            ]
    AdventureSpec.fresh
        "You are hungry for honey. You decide to go get some."
        [
            Encounter.wandering "Bears don't like it when people steal honey from their trees!"
                ["Black Bear", Some (RollSpec.create(1,3))]
            ]
    AdventureSpec.fresh
        "You are hungry for honey. You decide to go get some."
        [
            Encounter.wandering "Owlbears don't like it when people steal honey from their trees!"
                ["Owlbear", None]
            ]
    { AdventureSpec.fresh
        "An animal trainer is offering a 1000 gp reward for owlbear eggs."
        [
            Encounter.lair "You find owlbear eggs. Unfortunately the eggs are guarded!"
                ["Owlbear", Some (RollSpec.create(1,2))]
            ]
        with bonusGP = 1000
        }
    ]
    |> chooseRandom

let deadly ruleSet =
    [
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble."
        [
            Encounter.lair "The hill giants are not pleased to see you!"
                ["Hill Giant", Some (RollSpec.create(1,6))]
            ]
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble."
        [
            Encounter.lair "The hill giants are not pleased to see you!"
                ["Hill Giant", Some (RollSpec.create(2,6))]
            ]
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble."
        [
            Encounter.lair "The frost giants are not pleased to see you!"
                ["Frost Giant", Some (RollSpec.create(2,4))]
            ]
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble."
        [
            Encounter.lair "The frost giants are not pleased to see you!"
                ["Frost Giant", Some (RollSpec.create(2,8))]
            ]
    AdventureSpec.fresh
        "You climb a giant beanstalk looking for trouble. You have a bad feeling about this beanstalk."
        [
            Encounter.lair "The giants are not pleased to see you!"
                ["Frost Giant", Some (RollSpec.create(2,8))
                 "Hill Giant", Some (RollSpec.create(2,6))]
            ]
    ] |> chooseRandomExponentialDecay 0.3 Seq.head
