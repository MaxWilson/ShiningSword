[<AutoOpen>]
module Dev.App.Tracker.Game

let helpText = """
    Example commands:
    define Beholder
    add Beholder
    Beholder hp 180, xp 10000
    add Bob, Lara, Harry
    Harry hits Beholder #1 for 80
    Beholder #1 hits Lara for 30
    Beholder #1 hits Bob for 60
    clear dead
    Lara declares Kill beholder
    roll init
    next init
    """

[<AutoOpen>]
module DataTypes =
    type Name = Name of string
    type XP = XP of int
    type HP = HP of int

module Bestiary =
    type Definition = {
        xp: XP option
        hp: HP option
        initiativeMod: int option
        }
        with static member fresh = { xp = None; hp = None; initiativeMod = None }
    type d = Map<Name, Definition>
    let fresh = Map.empty
    let define name (bestiary: d) =
        bestiary |> Map.add name Definition.fresh
    let declareXP name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with xp = Some value })
    let declareHP name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with hp = Some value })
    let declareInit name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with initiativeMod = Some value })

module Game =
    type Action = Action of string
    type WoundLog = { victims: Map<Name, int>; woundedBy: Map<Name, int> }
        with static member fresh = { victims = Map.empty; woundedBy = Map.empty }
    type Creature = { name: Name; templateType: Name option; actionDeclaration: Action option; initiativeMod: int option; xpEarned: int; HP: int; woundLog: WoundLog }
        with static member fresh name templateType = { name = name; templateType = templateType; actionDeclaration = None; initiativeMod = None; xpEarned = 0; HP = 0; woundLog = WoundLog.fresh }
    type Command =
        | Define of Name
        | DeclareAction of Name * Action
        | DeclareInitiativeMod of Name * int
        | DeclareXP of Name * XP
        | DeclareHP of Name * HP
        | Add of Name
        | InflictDamage of src:Name * target:Name * hp:HP
        | ClearDeadCreatures
        | Remove of Name list
        | Rename of Name * newName:Name

    type d = {
        roster: Name list
        stats: Map<Name, Creature>
        bestiary: Bestiary.d
        }
    let fresh = { roster = []; stats = Map.empty; bestiary = Bestiary.fresh }
    let update msg model =
        match msg with
        | Define name ->
            { model with bestiary = model.bestiary |> Bestiary.define name }
        | DeclareAction (name, action) ->
            let declare name model =
                { model with stats = model.stats |> Map.add name { model.stats[name] with actionDeclaration = Some action }}
            match model.stats |> Map.tryFind name with
            | Some creature -> declare name model
            | None ->
                let names = model.stats.Values |> Seq.choose (fun c -> if c.templateType = Some name then Some c.name else None)
                names |> Seq.fold (flip declare) model
        | DeclareInitiativeMod (name, initiativeMod) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with initiativeMod = Some initiativeMod }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareInit name initiativeMod }
        | DeclareXP (name, (XP v as xp)) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with xpEarned = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareXP name xp }
        | DeclareHP (name, (HP v as hp)) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with HP = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareHP name hp }
        | Add (Name nameStr as name) ->
            match model.bestiary |> Map.tryFind name with
            | None ->
                { model with
                    roster = model.roster@[name]
                    stats = model.stats |> Map.add name (Creature.fresh name None) }
            | Some def ->
                match def.hp with
                | None ->
                    failwith $"{nameStr} must declare HP first"
                | Some (HP hp) ->
                    let individualName = Seq.unfold (fun i -> Some(Name $"{nameStr} #{i}", i+1)) 1 |> Seq.find (fun name' -> model.stats.ContainsKey name' |> not)
                    { model with
                        roster = model.roster@[individualName]
                        stats = model.stats |> Map.add individualName { Creature.fresh individualName (Some name) with HP = hp } }
        | InflictDamage(src, target, HP hpLoss) ->
            match model.stats |> Map.tryFind src, model.stats |> Map.tryFind target with
            | Some src, Some target ->
                let hp = target.HP
                let hp' = hp - hpLoss
                let isKill = hp > 0 && hp' <= 0
                // we don't let monster damage go negative, but we let hero damage go negative just in case their HP were never recorded
                let hpLoss = if isKill then hp elif (hp <= 0 && target.templateType.IsSome) then 0 else hpLoss
                let recordInteraction (name: Name) amount = Map.change name (function None -> Some amount | Some v -> Some (v+amount))
                let target' = { target with HP = target.HP - hpLoss; woundLog = { target.woundLog with woundedBy = target.woundLog.woundedBy |> recordInteraction src.name hpLoss } }
                let src' = { src with woundLog = { src.woundLog with victims = src.woundLog.victims |> recordInteraction target.name hpLoss } }
                let model' = { model with stats = model.stats |> Map.add src.name src' |> Map.add target.name target' }
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
                    { model' with stats = awards |> List.fold allocateXP model'.stats }
            | None, _ ->
                failwith $"{src} does not exist!"
            | _, None ->
                failwith $"{target} does not exist!"
        | ClearDeadCreatures ->
            let roster' =
                model.roster
                |> List.filter (fun name ->
                    let creature = model.stats[name]
                    if creature.templateType.IsNone then
                        true // never clear PCs
                    else
                        creature.HP > 0 // clear dead monsters
                    )
            { model with roster = roster'; stats = model.stats |> Map.filter (fun name _ -> roster' |> List.contains name) }
        | Remove names ->
            let roster' = model.roster |> List.filter (not << flip List.contains names)
            { model with
                roster = roster'
                bestiary = model.bestiary |> Map.filter (fun name _ -> names |> List.contains name |> not)
                stats = model.stats |> Map.filter (fun name cr -> roster' |> List.contains name && (match cr.templateType with Some type1 -> names |> List.contains type1 |> not | None -> true)) }
        | Rename(name, newName) ->
            let roster' = model.roster |> List.map(function name' when name' = name -> newName | unchanged -> unchanged)
            { model with
                roster = roster'
                stats = model.stats |> Seq.map (function KeyValue(name', stats) when name' = name -> (newName, { stats with name = newName }) | KeyValue unchanged -> unchanged)
                    |> Map.ofSeq
                }

    type FSX =
        // FSX-oriented script commands
        static member define name = update (Define (Name name))
        static member declareHP name hp = update (DeclareHP (Name name, HP hp))
        static member declareXP name xp = update (DeclareXP (Name name, XP xp))
        static member add name = update (Add (Name name))
        static member damage src target hp = update (InflictDamage (Name src, Name target, HP hp))
        static member getHP name (model:d) = model.stats[Name name].HP
        static member getXPEarned name (model:d) = model.stats[Name name].xpEarned
