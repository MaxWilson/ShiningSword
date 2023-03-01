module Domain.Adventure
open Domain.Metrics
open Domain.Random
open Domain.Character
open Domain.Treasure
open Domain.Ribbit

type Encounter = {
    description: string
    monsters: (RibbitName * RollSpec option) list
    inLair: bool
    }
    with
    static member wandering description monsters = { description = description; monsters = monsters; inLair = false }
    static member lair description monsters = { description = description; monsters = monsters; inLair = true }

type OngoingEncounter = {
    monsters: (RibbitName * int) list
    outcome: FightResult
    inLair: bool
    }

type AdventureSpec = {
    description: string
    encounters: Encounter list
    bonusXP: int
    bonusGP: int
    }
    with static member fresh description encounters = { bonusXP = 0; bonusGP = 0; description = description; encounters = encounters  }

type AdventureState = {
    currentEncounter: OngoingEncounter option
    scheduledEncounters: Encounter list
    ribbit: Ribbit
    }

let downtime sheet =
    { currentEncounter = None; scheduledEncounters = []; ribbit = Ribbit.Fresh }

let embark (spec: AdventureSpec) sheet =
    { currentEncounter = None; scheduledEncounters = spec.encounters; ribbit = Ribbit.Fresh }

let clearEnemies adventureState =
    let ribbit =
        stateChange {
            let! ribbit = Ribbit.GetM id
            let friendlies = ribbit.data.roster // |> Map.filter (fun name id -> isFriendlyP.Get id ribbit)
            // clear enemies from last encounter off the UI because they're all dead
            do! SetRoster friendlies |> Ribbit.ExecuteM
            }
        |> adventureState.ribbit.transform
    { adventureState with ribbit = ribbit }

let finishAdventure (spec: AdventureSpec) state =
    let msg =
        if spec.bonusXP > 0 && spec.bonusGP > 0 then
            $"You gain an extra {spec.bonusXP} XP and {spec.bonusGP} gold pieces for completing the adventure!"
        elif spec.bonusXP > 0 then
            $"You gain an extra {spec.bonusXP} XP for completing the adventure!"
        elif spec.bonusGP > 0 then
            $"You gain an extra {spec.bonusGP} gold pieces for completing the adventure!"
        else
            "Congratulations on a successful adventure!"
    msg, (state |> clearEnemies)

let toOngoing (encounter:Encounter) =
    let rollQty = function
    | name, Some (roll:RollSpec) -> name, roll.roll()
    | name, None ->
        let qty = 1
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
                () // add monsters
            }
        |> adventureState.ribbit.transform
    { (adventureState |> clearEnemies) with scheduledEncounters = rest; currentEncounter = Some next; ribbit = ribbit }

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
