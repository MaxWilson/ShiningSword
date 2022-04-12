module Domain.Adventure
open Domain.Character
open Domain.Character.Universal

type Encounter = {
    description: string
    monsters: (Name * RollSpec option) list
    }

type AdventureSpec = {
    description: string
    encounters: Encounter list
    allies: Domain.Character.Universal.CharacterSheet list
    }

type AdventureState = {
    mainCharacter: CharacterSheet
    currentEncounter: Encounter option
    scheduledEncounters: Encounter list
    ribbit: Ribbit.Types.State
    }

let createAdventure sheet =
    { mainCharacter = sheet; currentEncounter = None; scheduledEncounters = []; ribbit = Ribbit.Types.State.fresh }

let beginAdventure encounters state =
    match encounters with
    | [] -> state
    | first::rest ->
        { state with scheduledEncounters = encounters; currentEncounter = Some first }

let easy() =
    {
        description = "You hire on as a caravan guard."
        allies = []
        encounters = [
            let kobolds = rand 6
            let x = 6<gp> + 3<gp>
            {
                description = "One night, kobolds attack! Your companions cravenly flee but you fight bravely."
                monsters = ["Kobold", kobolds]
            }
        ]
    }

