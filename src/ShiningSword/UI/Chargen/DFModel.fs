module UI.Chargen.DF
open Domain.Ribbit.Properties
open type Eval
open Domain.Character
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Feliz
open UI
open UI.CommonUI
open Optics
open type Optics.Operations

// ad hoc placeholder, needs to become Ribbit combatant eventually
module AdHoc =
    type Combatant = {
        name: string
        team: int
        stats: Stats.Attributes
        traits: Trait list
        }
    type FightModel = {
        me: Combatant
        them: Combatant list
        }

type Model =
    {   char: Character
        queue: Map<Key, string>
        practiceFight: AdHoc.FightModel option
        }

let init constraints =
    {   char = createRandom (defaultArg constraints { Constraints.fresh with randomizationMethod = NonRandom })
        queue = Map.empty
        practiceFight = None }

type Msg =
    | Reroll of Constraints
    | FwdRoleplaying of UI.Roleplaying.Msg
    | ChangeRace of Race
    | ChangeProfession of Profession
    | TraitChange of TraitMsg
    | PracticeFight of AdHoc.FightModel option

let update msg model =
    let Char = Lens.create (fun m -> m.char) (fun v m -> { m with char = v })
    let Queue = Lens.create (fun m -> m.queue) (fun v m -> { m with queue = v })
    let Header = Lens.create (fun c -> c.header) (fun v c -> { c with header = v })
    let Profession = Lens.create (fun c -> c.profession) (fun v c -> { c with profession = v })
    match msg with
    | Reroll random -> init (Some random)
    | FwdRoleplaying msg ->
        model |> over (Char => Header) (UI.Roleplaying.update msg)
    | ChangeProfession prof ->
        model |> over Char (changeProfession prof)
    | ChangeRace race ->
        model |> over Char (changeRace race)
    | TraitChange (TraitMsg.Queue key) ->
        model |> over Queue (Map.add key "")
    | TraitChange (TraitMsg.QueueData(key,data)) ->
        model |> over Queue (Map.add key data)
    | TraitChange (TraitMsg.Unqueue key) ->
        model |> over Queue (Map.remove key)
    | TraitChange (TraitMsg.ClearStrictlyUnder key) ->
        let clearUnder (queue: Map<Key,_>) =
            let len = key.Length
            let rec under (k: Key) =
                if k = key then true
                elif k.Length > len then under k.Tail
                else false
            let rec strictlyUnder (key': Key) =
                if key'.Length > len then // in order to be strictly under, key' has to be longer (more specific) than key
                    under key'.Tail
                else false
            queue |> Map.filter (fun k v -> k |> strictlyUnder |> not)
        model |> over Queue clearUnder
    | PracticeFight fight ->
        { model with practiceFight = fight }

[<AutoOpen>]
module Helpers =
    let raceName = function
        | Catfolk -> "Cat-folk"
        | HalfElf -> "Half-elf"
        | HalfOgre -> "Half-ogre"
        | HalfOrc -> "Half-orc"
        | v -> v.ToString()

    let professionName (prof: Profession) = String.uncamel (prof.ToString())

    open AdHoc
    let goblin n =
        { name = sprintf "Goblin %d" n; team = 1; stats = Stats.freshFrom(11, 12, 9, 12); traits = [] }
    let goblinFight (me: Character) =
        let me: Combatant = {
            name =  me.header.name
            team = 0
            stats = me.stats
            traits = me.traits
            }
        let them = [goblin 1; goblin 2; goblin 3]
        { me = me; them = them }
