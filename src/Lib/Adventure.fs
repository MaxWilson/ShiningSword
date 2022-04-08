module Domain.Adventure
open Domain.Character.Universal

type Encounter = {
    monsters: (string * int) list
    rewardGp: int
    }

type AdventureState = {
    mainCharacter: CharSheet
    currentEncounter: Encounter option
    scheduledEncounters: Encounter list
    ribbit: Ribbit.Types.State
    }

let createAdventure sheet =
    { mainCharacter = sheet; currentEncounter = None; scheduledEncounters = []; ribbit = Ribbit.Types.State.fresh }
