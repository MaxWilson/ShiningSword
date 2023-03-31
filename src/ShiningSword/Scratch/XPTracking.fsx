#I __SOURCE_DIRECTORY__
#I ".."
#I "../Core"
#load "Optics.fs"
#load "Common.fs"
open Optics
open type Optics.Operations

type Name = Name of string
type XP = XP of int
type HP = HP of int

module Bestiary =
    type Definition = {
        xp: XP option
        hp: HP option
        }
        with static member fresh = { xp = None; hp = None }
    type d = Map<Name, Definition>
    let fresh = Map.empty
    let define name (bestiary: d) =
        bestiary |> Map.add name Definition.fresh
    let declareXP name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with xp = Some value })
    let declareHP name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with hp = Some value })

module Game =
    type Action = Action of string
    type WoundLog = { victims: Map<Name, int>; woundedBy: Map<Name, int> }
        with static member fresh = { victims = Map.empty; woundedBy = Map.empty }
    type Creature = { name: Name; templateType: Name option; actionDeclaration: Action option; xpEarned: int; HP: int; woundLog: WoundLog }
        with static member fresh name templateType = { name = name; templateType = templateType; actionDeclaration = None; xpEarned = 0; HP = 0; woundLog = WoundLog.fresh }
    type Command =
        | Define of Name
        | DeclareXP of Name * XP
        | DeclareHP of Name * HP
        | Add of Name
        | InflictDamage of src:Name * target:Name * hp:HP

    type d = {
        roster: Map<Name, Creature>
        bestiary: Bestiary.d
        }
    let fresh = { roster = Map.empty; bestiary = Bestiary.fresh }
    let update msg model =
        match msg with
        | Define name ->
            { model with bestiary = model.bestiary |> Bestiary.define name }
        | DeclareXP (name, (XP v as xp)) ->
            match model.roster |> Map.tryFind name with
            | Some creature ->
                { model with roster = model.roster |> Map.add name { creature with xpEarned = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareXP name xp }
        | DeclareHP (name, (HP v as hp)) ->
            match model.roster |> Map.tryFind name with
            | Some creature ->
                { model with roster = model.roster |> Map.add name { creature with HP = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareHP name hp }
        | Add (Name nameStr as name) ->
            match model.bestiary |> Map.tryFind name with
            | None ->
                { model with roster = model.roster |> Map.add name (Creature.fresh name None) }
            | Some def ->
                match def.hp with
                | None ->
                    failwith $"{nameStr} must declare HP first"
                | Some (HP hp) ->
                    let individualName = Seq.unfold (fun i -> Some(Name $"{nameStr} #{i}", i+1)) 1 |> Seq.find (fun name' -> model.roster.ContainsKey name' |> not)
                    { model with roster = model.roster |> Map.add individualName { Creature.fresh individualName (Some name) with HP = hp } }
        | InflictDamage(src, target, HP hpLoss) ->
            match model.roster |> Map.tryFind src, model.roster |> Map.tryFind target with
            | Some src, Some target ->
                let hp = target.HP
                let hp' = hp - hpLoss
                let isKill = hp > 0 && hp' <= 0
                // we don't let monster damage go negative, but we let hero damage go negative just in case their HP were never recorded
                let hpLoss = if isKill then hp elif (hp <= 0 && target.templateType.IsSome) then 0 else hpLoss
                let recordInteraction (name: Name) amount = Map.change name (function None -> Some amount | Some v -> Some (v+amount))
                let target' = { target with HP = target.HP - hpLoss; woundLog = { target.woundLog with woundedBy = target.woundLog.woundedBy |> recordInteraction src.name hpLoss } }
                let src' = { src with woundLog = { src.woundLog with victims = src.woundLog.victims |> recordInteraction target.name hpLoss } }
                let model' = { model with roster = model.roster |> Map.add src.name src' |> Map.add target.name target' }
                if isKill = false then
                    model'
                else
                    let woundLog = target'.woundLog
                    let awards =
                        [
                            let denominator = (woundLog.victims.Values |> Seq.sum) + (woundLog.woundedBy.Values |> Seq.sum)
                            let xpTotal =
                                match target'.templateType with
                                | Some monsterKind ->
                                    match model.bestiary[monsterKind].xp with Some (XP xp) -> xp | None -> 0
                                | None -> 0
                            for name in (woundLog.victims.Keys |> Seq.append woundLog.woundedBy.Keys |> Seq.distinct) do
                                let numerator =
                                    (woundLog.victims |> Map.tryFind name |> Option.defaultValue 0)
                                        + (woundLog.woundedBy |> Map.tryFind name |> Option.defaultValue 0)
                                let award = (numerator * xpTotal)/denominator
                                name, award
                        ]
                    let allocateXP (roster: Map<Name, Creature>) = function
                        | name, award ->
                            roster |> Map.change name (function None -> None | Some c -> Some { c with xpEarned = c.xpEarned + award })
                    { model' with roster = awards |> List.fold allocateXP model'.roster }
            | None, _ ->
                failwith $"{src} does not exist!"
            | _, None ->
                failwith $"{target} does not exist!"

    type FSX =
        // FSX-oriented script commands
        static member define name = update (Define (Name name))
        static member declareHP name hp = update (DeclareHP (Name name, HP hp))
        static member declareXP name xp = update (DeclareXP (Name name, XP xp))
        static member add name = update (Add (Name name))
        static member damage src target hp = update (InflictDamage (Name src, Name target, HP hp))
        static member getHP name (model:d) = model.roster[Name name].HP
        static member getXPEarned name (model:d) = model.roster[Name name].xpEarned

module UI =
    type d = { input: string; game: Game.d }
    let fresh = { input = ""; game = Game.fresh }

type Model = UI.d
type Msg =
    | ReviseInput of msg: string
    | SubmitInput

open Game
open type Game.FSX
let mutable x = Game.fresh
iter &x (define "Beholder")
iter &x (add "Beholder") // will throw because OrcHP aren't set yet and we don't support lazy data entry in this simple tool because ribbit already supports it
iter &x (declareXP "Beholder" 10000)
iter &x (declareHP "Beholder" 70) // we don't support rolls in this simple tool, only fixed values
iter &x (add "Beholder") // will not throw now
iter &x (add "Bob")
iter &x (add "Lara")
iter &x (add "Harry")
iter &x (declareHP "Harry" 50)
iter &x (damage "Bob" "Beholder #1" 40)
iter &x (damage "Beholder #1" "Lara" 20)
iter &x (damage "Beholder #1" "Harry" 10)
iter &x (damage "Lara" "Beholder #1" 30)
(getHP "Harry" x) = 40 // we declare Harry's HP but not Lara or Bob, so their HP goes into the negatives when they take damage
(getHP "Beholder #1" x) = 0 // it's dead, so XP will have been awarded when Lara killed it
(getHP "Lara" x) = -20 // she's taken 20 points of damage since the start of the fight. If she'd started with 50 she'd have 30 now.
(getHP "Bob" x) = 0 // still undamaged
(getXPEarned "Harry" x) = 1000
(getXPEarned "Bob" x) = 4000
(getXPEarned "Lara" x) = 5000
