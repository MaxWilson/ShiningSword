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
    type RuntimeValue =
        | Text of string | Random of Domain.Random.RollSpec | Number of int
        | Bool of bool
    type Combatant = {
        name: string
        team: int
        stats: Stats.Attributes
        scratchPad: Map<string, RuntimeValue> // a grab bag of things like current HP or whether it's currently stunned; not permanent parts of the character sheet
        }
    let IsOK (c: Combatant) =
        let checkWhether flagName =
           match c.scratchPad |> Map.tryFind flagName with
            | Some (Bool b) -> b
            | _ -> false
        not (checkWhether "IsDead" || checkWhether "IsUnconscious")
    let CurrentHP (c:Combatant) =
        let hp = HP c.stats |> Eval.sum
        let damageTaken =
            match c.scratchPad |> Map.tryFind "damageTaken" with
            | Some (Number n) -> n
            | _ -> 0
        hp - damageTaken
    // "object" is the game design sense, something within the system, not in the OO sense or even the "not a creature" sense
    type Object = Combatant of Combatant | Thing of string
    type FightMsg =
        | TakeDamage of name:string * int
        | SetIntention of name:string * string
        | SetResultMsg of name:string * string
    type FightModel = Map<Name, Object>
    let update msg (model: Map<Name, Object>) =
        let changeObject name propertyName fChange =
            model |> Map.change name (function
                | Some (Combatant c) ->
                    let change bag =
                        bag |> Map.change propertyName fChange
                    Some <| Combatant { c with scratchPad = c.scratchPad |> change }
                | otherwise -> notImpl())
        match msg with
        | TakeDamage(name, amount) ->
            let changeDamageTaken = function
                | Some (Number n) -> Some (Number (n + amount))
                | _ -> Some (Number amount)
            changeObject name "damageTaken" changeDamageTaken
        | SetIntention(name, intention) ->
            changeObject name "AdHocIntention" (fun _ -> Some (Text intention))
        | SetResultMsg(name, msg) ->
            changeObject name "AdHocLastRoundMsg" (fun _ -> Some (Text msg))
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
    open Domain.Random
    let goblin n =
        { name = sprintf "Goblin %d" n; team = 1; stats = Stats.freshFrom(11, 12, 9, 12); scratchPad = Map.empty }
    let goblinFight (me: Character) =
        let me: Combatant = {
            name =  me.header.name
            team = 0
            stats = me.stats
            scratchPad = Map.empty
            }
        let them = [goblin 1; goblin 2; goblin 3]
        let data = me::them |> List.map (fun c -> c.name, Combatant c) |> Map.ofList
        data
    let combatants (model: FightModel) = model |> Seq.choose (function KeyValue(_, Combatant c) -> Some c | _ -> None)
    let fightOneRound (model: FightModel) =
        let model = CQRS.CQRS.Create(model, update)
        let swingDamageOf = function
            | 10 -> RollSpec.create(1,6)
            | 11 -> RollSpec.create(1,6,+1)
            | 12 -> RollSpec.create(1,6,+2)
            | 13 -> RollSpec.create(2,6,-1)
            | 14 -> RollSpec.create(2,6)
            | 15 -> RollSpec.create(2,6,+1)
            | 16 -> RollSpec.create(2,6,+2)
            | 17 -> RollSpec.create(3,6,-1)
            | 18 -> RollSpec.create(3,6)
            | 19 -> RollSpec.create(3,6,+1)
            | 20 -> RollSpec.create(4,6,-1)
            | _ -> notImpl()
        let dmg name =
            match model.State |> Map.tryFind name with
            | Some (Combatant c) -> c.stats |> ST |> Eval.sum |> swingDamageOf // TODO: sw vs thr should be based on weapon, and weapon damage bonuses should be included
            | _ -> notImpl()
        let combatantsInOrder =
            combatants model.State |> Seq.sortBy (fun c -> Speed c.stats |> sum, ST c.stats |> sum, c.team)
            |> Seq.map (fun c -> c.name)
        let attack (attacker: Combatant) (defender: Combatant) =
            let attackTarget = attacker.stats |> DX |> Eval.sum // TODO: should use weapon skill instead
            let defendTarget = Dodge defender.stats |> Eval.sum // TODO: should use Parry or Block instead if appropriate
            let r3d6 = RollSpec.create(3,6)
            // TODO: tree-shaped logging here instead of just local SetResultMsg
            if r3d6.roll() <= attackTarget then // TODO: should account for crits
                if r3d6.roll() > defendTarget then
                    let damageRoll = dmg attacker.name
                    let damage = damageRoll.roll()
                    model.Execute (TakeDamage(defender.name, damage))
                    model.Execute (SetResultMsg(attacker.name, $"{attacker.name} hits {defender.name} for {damage} damage [{damageRoll}]"))
                else
                    model.Execute (SetResultMsg(attacker.name, $"{defender.name} dodges {attacker.name}'s attack"))
            else
                model.Execute (SetResultMsg(attacker.name, $"{attacker.name} misses {defender.name}"))
        for attackerName in combatantsInOrder do
            match model.State |> Map.tryFind attackerName with
            | Some (Combatant attacker) when IsOK attacker ->
                let defender = model.State.Values |> Seq.tryFind(function
                    | Combatant c when c.team <> attacker.team && IsOK c -> true
                    | _ -> false)
                match defender with
                | Some (Combatant defender) ->
                    attack attacker defender
                | _ ->
                    model.Execute (SetResultMsg(attacker.name, $"{attacker.name} can't find a target"))
            | _ ->
                model.Execute (SetResultMsg(attackerName, $"{attackerName} is not OK"))
        model.State
