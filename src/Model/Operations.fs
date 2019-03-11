module Model.Operations
open Model.Types
open Model.Functions
open Common
open System

module Recognizer =
    open Packrat
    let (|Roster|_|) =
        ExternalContextOf<GameState> >> function
        | Some { battle1 = Some { combatants = c } } -> Some c
        | _ -> None
    let inRoster pred (roster: Battle1.Roster1) =
        roster |> Map.tryFindKey (fun id v -> pred v)
    let isAlive (c:Combatant) = true // todo: check for liveness
    let nameMatches name (c:Combatant) =
        c.stats.name = name && isAlive c
    let (|Name|_|) = function
        | Word (name, ctx) & Roster(roster)
            when inRoster (nameMatches name) roster |> Option.isSome ->
            Some((inRoster (nameMatches name) roster |> Option.get), ctx)
        | _ -> None
    let (|Intention|_|) = function
        | Str "move" (Int (x, OWS(Str "," (Int (y, ctx))))) -> Some(Move(x,y), ctx)
        | Str "attack" (Name(id, ctx)) -> Some(Attack(id), ctx)
        | _ -> None
    let (|Number|_|) = (|Int|_|)
    let (|Bool|_|) = function
        | Word(AnyCase("y" | "yes" | "true" | "t"), ctx) -> Some(true, ctx)
        | Word(AnyCase("n" | "no" | "false" | "f"), ctx) -> Some(false, ctx)
        | _ -> None
    let (|FreeformText|_|) = function
        | Any(words, ctx) -> Some(words, ctx)

module Query =
    open Packrat

    let tryParse recognizer arg =
        match ParseArgs.Init arg |> recognizer with
        | Some(v, End) -> Some v
        | _ -> None
    let text state txt =
        (Query.Freetext txt, state), Some
    let confirm state txt =
        (Query.Confirm txt, state), (tryParse Recognizer.``|Bool|_|``)
    let number state txt =
        (Query.Number txt, state), (tryParse Recognizer.``|Number|_|``)
    let choose state prompt choices =
        let tryChoose arg =
            choices |> Seq.tryFind (fun choice -> arg = choice.ToString())
        (Query.Select (prompt, choices |> Seq.map (fun v -> v.ToString()) |> Array.ofSeq), state), tryChoose
    let alert state txt =
        (Query.Alert txt, state), thunk (Some ())
    let character state char =
        (Query.Character char, state), thunk (Some ())

//module Creature =
//    let map f (c: Combatant) =
//        { c with current = f c.current }

module GameState =
    let mapLog f (g:GameState) =
        { g with log = f g.log }
    //let mapRoster f (g:GameState) =
    //    { g with roster = Option.map f g.roster }
    //let mapCreatureId id msg f (g:GameState) =
    //    match g.roster with
    //    | Some roster ->
    //        { g with
    //            roster = roster |> Map.add id (Creature.map f roster.[id]) |> Some;
    //            log = g.log |> Log.log msg }
    //    | None -> g
    let empty = { pcs = []; parEarned = 0; gateNumber = 1; towerNumber = 1; randomNumber = 1; timeElapsed = 0; gp = 0; log = Log.empty; battle1 = None; battle = None }

// executes action declarations in listed order
let execute (d: Declarations) (g:GameState) : GameState =
    let execute (g:GameState) decl =
        match g.battle1 with
        | None -> g
        | Some r ->
            failwith "Not implemented"
            //match decl with
            //| id, Move(x, y) ->
            //    let msg = sprintf "%s moves to %A" r.[id].current.name (x,y)
            //    { g with roster = r |> Map.add id { r.[id] with position = x, y } |> Some}
            //        |> GameState.mapLog (Log.log msg)
            //| id, Attack(targetId) ->
            //    let actor = r.[id]
            //    let target = r.[targetId]
            //    let roll = rand 20
            //    if roll + 4 > 15 then
            //        let dmg = rand 8 + 2
            //        let msg = (sprintf "%s rolls %d and hits %s for %d points of damage!" actor.current.name roll target.current.name dmg)
            //        g |> GameState.mapCreatureId targetId msg (fun (s: StatBlock) -> { s with hp = s.hp - dmg })
            //          |> GameState.mapLog (Log.log msg)
            //    else
            //        g |> GameState.mapLog (Log.log (sprintf "%s rolls %d and misses %s." actor.current.name roll target.current.name))

    d |> List.fold execute g

module CharSheet =
    open Model.Tables
    let computeLevel xp =
        (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
    let combatBonus stat = (stat/2) - 5 // based on 5E tables
    let computeHP con classList =
        let bonus = combatBonus con
        let dieSize characterClass = match characterClass with Champion -> 10 | Battlerager -> 12 | Elemonk -> 8 | PurpleDragonKnight -> 10 | Samurai -> 10
        classList |> Seq.mapi (fun l cl -> if l = 0 then (dieSize cl) + bonus else (dieSize cl)/2 + 1 + bonus) |> Seq.sum
    let computeMaxHP (char:CharSheet) =
        computeHP char.con char.classLevels
    let computeAC (char:CharSheet) =
        14 + combatBonus char.dex // todo: account for equipment properly
    let prioritize ((r1, r2, r3, r4, r5, r6) as rolls) ((str,dex,con,int,wis,cha) as priorities) =
        let rolls = Common.shuffleCopy [|r1;r2;r3;r4;r5;r6|] |> Array.sortDescending
        let priorityIndexes = [|str,0;dex,1;con,2;int,3;wis,4;cha,5|] |> Common.shuffleCopy |> Array.sortBy fst |> Array.map snd
        let statByIndex ix =
            let ix = priorityIndexes |> Array.findIndex ((=) ix)
            rolls.[ix]
        (statByIndex 0),(statByIndex 1),(statByIndex 2),(statByIndex 3),(statByIndex 4),(statByIndex 5)
    let create name sex rolls isNPC region (template:CharTemplate) =
        let xp = 0
        let defaultClass = Champion // fallback in the absence of other priorities
        let (str,dex,con,int,wis,cha) = prioritize rolls template.statPriorities
        let (stats : CharSheet) = {
            originalRolls = rolls
            CharSheet.name = name; sex = sex; xp = xp;
            str = str; dex = dex; con = con; int = int; wis = wis; cha = cha;
            features = []
            isNPC = isNPC
            race = Human
            classLevels = (template.advancementPriorities) @ List.init 20 (thunk defaultClass) |> List.take (computeLevel xp)
            equipment = []
            template = Some template
            description = template.description
            homeRegion = region
            }
        stats
    let name = Lens.lens (fun (c:CharInfo) -> c.src.name) (fun v c -> { c with src = { c.src with name = v }})
    let isAlive (charSheet:CharInfo) = charSheet.hp > 0
    let normalize (levels: CharClass list) : (CharClass * int) list =
        levels |> List.groupBy id |> List.map (fun (k, vs) -> k, vs.Length)
    let className = function
        | PurpleDragonKnight -> "Purple Dragon Knight"
        | Elemonk -> "Elemental Monk"
        | Samurai | Champion | Battlerager as c -> c.ToString()
    let summarize (levels: CharClass list) : string =
        normalize levels |> List.map (fun (c,l) -> sprintf "%s %d" (className c) l) |> String.join "/"
module CharInfo =
    open CharSheet
    open Roll
    let getCurrentHP char =
        match char.usages.TryGetValue("HP") with true, v -> v | _ -> char.hp
    let ofCharSheet (c:CharSheet) =
        { CharInfo.src = c; CharInfo.usages = Map.empty; CharInfo.hp = CharSheet.computeMaxHP c; CharInfo.thp = 0; sp = 0; CharInfo.status = Status([]) }
    let toStatBlock (char:CharInfo) =
        let c = char.src
        let profBonus = 1 + ((c.classLevels.Length - 3)/4)
        let toHit = profBonus + combatBonus (max c.str c.dex)
        {
        name = c.name
        typeName = c.template |> Option.map (fun t -> t.name)
        sex = c.sex
        str = c.str
        dex = c.dex
        con = c.con
        int = c.int
        wis = c.wis
        cha = c.cha
        hp = getCurrentHP char
        ac = computeAC c
        xp = c.xp / 10 // 10% XP reward for killing leveled PCs
        resistances = Set.empty
        immunities = Set.empty
        damageResistance = Map.empty
        conditionExemptions = Set.empty
        attacks = [{Attack.tohit = toHit; Attack.damage = (Combine(Sum, Aggregate[Dice(1, 8); StaticValue (max c.str c.dex)]), DamageType.Weapon) }] // todo: compute attacks
        features = c.features
        }
