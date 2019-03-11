module Model.Gameplay
open Interaction
open Model.Types
open Model.Functions
open Model.Operations
open Model.Tables
open Common
open System

let calculate mtable (monsters: Name seq) =
    let costs = monsters |> Seq.map (fun m -> Math.Pow((mtable m |> snd |> float) / 100., (2./3.)))
    (Math.Pow(Seq.sum costs, 1.5) * 100. |> Math.Round |> int)

let normalize template =
    [|for (name, i) in template do
        for i in 1..i do
            yield name
        |]

let makeEncounter (mtable: Name -> float * int) templates (maxCR: int) (xpBudget: int) =
    let rec generate() =
        let template : (Name * int) list = templates maxCR
        let template = normalize template
        let rec addMonster accum =
            let precost = calculate mtable accum
            if precost >= xpBudget then
                accum
            else
                let monster = chooseRandom template
                let monsters' = monster::accum
                let postcost = calculate mtable monsters'
                if postcost <= xpBudget then
                    addMonster monsters'
                else // probabilistically add the final monster, or not
                    let overage = postcost - xpBudget
                    let overageRatio = (float overage) / (float (postcost - precost))
                    if random.NextDouble() < overageRatio then
                        accum
                    else
                        monsters'
        match addMonster [] with
        | [] ->
            generate() // this template was too tough to allow even one monster--choose a different template
        | candidate ->
            candidate
    let lst = generate()
    lst |> List.groupBy id |> List.map (fun (k, vs) -> k, List.length vs) |> List.sortByDescending snd

let monsters = [
    "Hobgoblin", 0.5
    "Orc", 0.5
    "Orog", 2.
    "Orc War Chief", 4.
    "Beholder", 13.
    "Frost Giant", 8.
    "Fire Giant", 9.
    "Skeleton", 0.25
    "Zombie", 0.5
    "Goblin", 0.25
    "Flameskull", 4.
    "Githyanki Warrior", 3.
    "Yeti", 3.
    "Young White Dragon", 6.
    "Young Red Dragon", 10.
    "Adult Red Dragon", 17.
    "Ancient White Dragon", 20.
    "Purple Worm", 15.
    "Nightwalker", 20.
    "Bodak", 6.
    "Tarrasque", 30.
    ]
let lookup monsters name = monsters |> List.find (fst >> (=) name) |> fun (_, cr) -> Model.Tables.monsterCR |> Array.pick (function { CR = cr'; XPReward = xp } when cr' = cr -> Some(cr, xp) | _ -> None)
let templates = [|
    ["Orc", 10; "Orc War Chief", 1]
    ["Beholder", 1; "Hobgoblin", 20]
    ["Fire Giant", 1; "Hobgoblin", 8; "Skeleton", 4]
    ["Orc", 10; "Orog", 1]
    ["Skeleton", 3; "Zombie", 2]
    ["Orc", 6; "Skeleton", 4]
    ["Githyanki Warrior", 6; "Yeti", 3]
    ["Young White Dragon", 1]
    ["Young Red Dragon", 1]
    ["Adult Red Dragon", 1]
    ["Frost Giant", 1; "Yeti", 2]
    ["Beholder", 1; "Purple Worm", 1; "Adult Red Dragon", 1; "Frost Giant", 3]
    ["Nightwalker",1;"Ancient White Dragon", 1; "Beholder", 1; "Purple Worm", 1; "Adult Red Dragon", 1; "Frost Giant", 3]
    ["Ancient White Dragon", 1; "Beholder", 1; "Purple Worm", 1; "Adult Red Dragon", 1; "Frost Giant", 3]
    ["Nightwalker", 1; "Bodak", 6]
    ["Tarrasque", 1]
    |]
let rec getTemplate monsters (templates: (string * int) list[]) maxCR =
    let t = chooseRandom templates
    if t |> List.exists (fun (name, _) -> (lookup monsters name |> fst) |> int > maxCR) then
        getTemplate monsters templates maxCR
    else
        t
let mixTemplate mixProbability getTemplate arg =
    if (random.NextDouble() < mixProbability) then
        (getTemplate arg) @ (getTemplate arg)
    else getTemplate arg

let queryInteraction = Interaction.InteractionBuilder<Query * GameState, string>()

let log msg state = { state with log = Log.log msg state.log }
let logAdvance state = { state with log = Log.advance state.log }

let getNameAndSex state firstPerson isFriend : Eventual<_,_,Name * Sex> = queryInteraction {
    if firstPerson then
        let! name = Query.text state "What's your name?"
        let! sex = Query.choose state "What's your sex?" [Male; Female]
        return (name, sex)
    elif isFriend then
        let! name = Query.text state "What's your friend's name?"
        let! sex = Query.choose state "What's their sex?" [Male; Female]
        return (name, sex)
    else
        let! sex = Query.choose state "Are you looking for males or females? (This affects which regions you can recruit from.)" ["Don't care";"Male"; "Female"]
        let sex = match sex with "Don't care" -> chooseRandom [|Male;Female|] | "Male" -> Male | _ -> Female
        let eligibleLists = Model.Names.names |> List.filter (fun ((n,t),_) -> t = (sex.ToString()))
        let captions = (eligibleLists |> List.map (fst >> fst))
        let! nameType = Query.choose state "What region do you want to recruit from?" ("Don't care" :: captions)
        let nameType = match nameType with "Don't care" -> chooseRandom (Array.ofList captions) | v -> v
        let chosenList = eligibleLists |> List.pick (fun ((n,_),l) -> if nameType = n then Some l else None)
        let firstname = chooseRandom chosenList
        let lastname =
            match sex, Model.Names.names |> List.tryFind (fun ((n,t),_) -> (t = ("Last" + sex.ToString()) || t = "Last") && n = nameType) with
            | _, Some (_, lst) -> chooseRandom lst
            | Male, None -> sprintf "%s%s" (chooseRandom [|" ben ";" son ";" dak ";" s'";" "|]) (chooseRandom chosenList)
            | Female, None -> (chooseRandom [|" bat " + (chooseRandom chosenList); (chooseRandom chosenList) + "dotter"; "d'"+(chooseRandom chosenList); chooseRandom chosenList|])
        match Model.Names.names |> List.tryFind (fun ((n,t),_) -> t = ("Cognomen" + sex.ToString()) && n = nameType) with
        | Some (_, cognomens) ->
            let name = sprintf "%s %s %s" firstname lastname (chooseRandom cognomens)
            return (name, sex)
        | None ->
            let name = sprintf "%s %s" firstname lastname
            return (name, sex)
    }

let rec getPCs (state: GameState) firstPerson isFriend : Eventual<_,_,_> = queryInteraction {
    let! pc = Chargen.Workflow.newPC state firstPerson isFriend
    let state = { state with pcs = state.pcs@[pc]; gp = if firstPerson || isFriend then state.gp else state.gp - 100 }
    let! more = Query.choose state (sprintf "Do you want to recruit%shelp? It costs 100 gold pieces up front plus salary (50 gold per day per level)." (if state.pcs.Length = 1 then " " else " more "))
                    ["I have a friend"; "Hire help"; "No, I'm ready"]
    match more with
    | "I have a friend" ->
        return! getPCs state false true // no cost
    | "Hire help" ->
        // charge 100 gp to recruit a companion
        return! getPCs state false false
    | _ ->
        return state
    }

let makeTower parXpEarned nTower =
    let N = 4 // number of ideal PCs
    let avg a b = (a + b)/2
    let isEpic = parXpEarned >= 400000
    let computeLevel xp =
        (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
    let level = (computeLevel parXpEarned)
    let budget =
        match nTower with
        // once you've been 20th level for a while, we take off the difficulty caps and scale to unlimited difficulty
        | 1 | 2 when isEpic ->
            N * (parXpEarned / 40)
        | 3 when isEpic ->
            N * (parXpEarned / 27)
        | _ when isEpic ->
            N * (parXpEarned / 20)
        // otherwise use the DMG tables. Note that encounters 1-4 should sum to somewhat less than a full day's XP budget,
        // because you will have random encounters while resting.
        | 1 ->
            N * (avg xpBudgets.[level-1].easy xpBudgets.[level-1].medium)
        | 2 ->
            N * (avg xpBudgets.[level-1].medium xpBudgets.[level-1].hard)
        | 3 ->
            N * (avg xpBudgets.[level-1].hard xpBudgets.[level-1].deadly)
        | _ ->
            N * ((float xpBudgets.[level-1].deadly) * 1.2 |> int)
    let e = makeEncounter (lookup monsters) (getTemplate monsters templates |> mixTemplate 0.30) (if isEpic then 30 else level) budget
    let cost = (calculate (lookup monsters) (normalize e))
    let xpEarned = e |> Seq.sumBy (fun (name, i) -> i * (lookup monsters name |> snd))
    let earned = xpEarned/N
    let gpEarned = rand (Math.Pow (float budget, 2./3.) |> int)
    e, earned, xpEarned, gpEarned

let makeRandom pcs parXpEarned nRandom =
    let N = pcs |> Seq.length // number of ideal PCs
    let avg a b = (a + b)/2
    let isEpic = parXpEarned >= 400000
    let computeLevel xp =
        (levelAdvancement |> Array.findBack (fun x -> xp >= x.XPReq)).level
    let level = (computeLevel parXpEarned)
    let minRandomEncounterBudget = if isEpic then N * (parXpEarned / 80) else N * (avg xpBudgets.[level-1].easy xpBudgets.[level-1].medium) / 2
    let budget = minRandomEncounterBudget * nRandom
    let e = makeEncounter (lookup monsters) (getTemplate monsters templates) (if isEpic then 30 else level) budget
    let c = (calculate (lookup monsters) (normalize e))
    let xpEarned = e |> Seq.sumBy (fun (name, i) -> i * (lookup monsters name |> snd))
    let earned = xpEarned / N
    let gpEarned = rand (sqrt(float budget) |> int) // make less money from random encounters
    e, c, earned, xpEarned, gpEarned

let battlecry (pcs: CharInfo list) monsters =
    let plural = match monsters with [_, 1] -> false | _ -> true
    let cries = [|
        sprintf (if plural then """"Give me blood!" you scream as %s attack.""" else """"Give me blood!" you scream as %s attacks.""")
        sprintf (if plural then """"Not again!" you groan, as %s attack.""" else """"Not again!" you groan, as %s attacks.""")
        sprintf """"Blood or death!" shout your companions at %s, as they draw their weapons."""
        sprintf """%s grins crazily and gestures behind you. You turn and see %s!""" ((chooseRandom (Array.ofList pcs)).src.name)
        sprintf "Glumly you prepare yourselves to meet %s in battle."
        |]
    let cry = chooseRandom cries
    let rec monsterDescription monsters =
        match monsters with
        | (name:string, qty)::rest when qty = 1 ->
            match Char.ToLowerInvariant(name.[0]) with
            | 'a' | 'e' | 'i' | 'o' | 'u' -> (sprintf "an %s" name)::(monsterDescription rest)
            | _ -> (sprintf "a %s" name)::(monsterDescription rest)
        | (name, qty)::rest ->
            (sprintf "%d %ss" qty name)::(monsterDescription rest)
        | [] -> []
    cry (monsterDescription monsters |> oxfordJoin)

let advance state =
    match state with
        | { towerNumber = 4 } as state -> { state with gateNumber = state.gateNumber + 1; towerNumber = 1; timeElapsed = state.timeElapsed + 600 }
        | { towerNumber = n } as state -> { state with towerNumber = n + 1; timeElapsed = state.timeElapsed + 600 }
    |> logAdvance

let timeSummary = function
    | n when n < 60 -> sprintf "%d seconds" n
    | n when n < 3600 -> sprintf "%d minutes" (n/60)
    | n when n < 3600*24 -> sprintf "%d hour(s)" (n/(3600))
    | n ->
        let hours = (n/(3600))
        sprintf "%d day(s) and %d hour(s)" (hours / 24) (hours % 24)
let alert state msg = queryInteraction {
    let state = state |> GameState.mapLog (Log.log msg)
    do! Query.alert state msg
    return state
    }
let retirementMessage state =
    let hirelings, pcs = state.pcs |> List.filter CharSheet.isAlive |> List.partition (fun cs -> cs.src.isNPC)
    let hirelingNames = hirelings |> List.map (Lens.get CharSheet.name) |> oxfordJoin
    if pcs.Length = 0 then
        sprintf "%s retire from adventuring." hirelingNames
    else
        let collectiveVocative =
            match pcs with
            | [me]  when state.gp >= 0 ->
                sprintf "%s, you" me.src.name
            | me::rest ->
                sprintf "%s, %s" me.src.name (oxfordJoin ("you"::(List.map (Lens.get CharSheet.name) rest)))
            | v -> // everybody still alive is an NPC, should not get here, already handled above
                matchfail v
        if state.gp > 0 then
            if pcs.Length > 1 then
                (sprintf "%s happily retire from adventuring and spend the rest of your life living off %d gold pieces that you found, %d gold pieces each!" collectiveVocative state.gp (state.gp/pcs.Length))
            else
                (sprintf "%s happily retire from adventuring and spend the rest of your life living off %d gold pieces that you found." collectiveVocative state.gp)
        elif state.gp = 0 then
            (sprintf "%s happily retire from adventuring, glad just to be alive." collectiveVocative)
        elif hirelings.Length > 0 then
            (sprintf "%s glumly retire from adventuring and spend the rest of your life doing menial labor, paying off the %d gold pieces that you owe to %s." collectiveVocative -state.gp hirelingNames)
        else
            (sprintf "%s glumly retire from adventuring and spend the rest of your life doing menial labor, paying off the %d gold pieces that you owe." collectiveVocative -state.gp)

let fight (state: GameState) =
    let mutable hpMap =
        state.battle1.Value.combatants
        |> Seq.map (function
            KeyValue(id, c) ->
                id, match c.usages.TryGetValue("HP") with
                    | true, v -> v
                    | _ -> c.stats.hp)
        |> Map.ofSeq
    let combatants = state.battle1.Value.combatants
    let guys = state.battle1.Value.combatants |> Map.toArray |> Array.map snd

    let mutable log = state.log
    let logMsg msg =
        log <- Log.log msg log
    let targetName target = match target.stats.typeName with Some tn when not (String.equalsIgnoreCase tn target.stats.name) -> sprintf "%s the %s" target.stats.name tn | _ -> target.stats.name
    let inflict isCrit me (target:Combatant) dmg =
        // deduct dmg from hp
        let hp' = (hpMap.[target.id] - dmg)
        hpMap <- hpMap |> Map.add target.id hp'
        // now do logging
        let verb = if isCrit then "crits" else "hits"
        let targetName = targetName target
        if hp' > 0 then
            (sprintf "%s %s %s for %d points of damage! (%d HP remaining)" me verb targetName dmg hp') |> logMsg
        else
            (sprintf "%s %s %s for %d points of damage! %s dies!" me verb targetName dmg targetName) |> logMsg
    let randomTarget (actor:Combatant) =
        let liveEnemies = guys |> Array.filter (fun c -> c.team <> actor.team) |> Array.filter (fun c -> hpMap.[c.id] > 0)
        if liveEnemies.Length > 0 then
            Some <| chooseRandom liveEnemies
        else None
    let numberOfTeams () =
        guys |> Seq.filter (fun c -> hpMap.[c.id] > 0) |> Seq.groupBy (fun c -> c.team) |> Seq.length
    while numberOfTeams () > 1 do
        let liveIds = hpMap |> Map.filter (fun _ hp -> hp > 0) |> Map.toArray |> Array.map fst |> shuffleCopy
        let resolve = Model.Dice.Roll.eval >> (fun v -> v.value)
        for id in liveIds do
            if hpMap.[id] > 0 then // if still alive
                let c = combatants.[id]
                match randomTarget c with
                | Some target ->
                    for att in c.stats.attacks do
                        let toHit = target.stats.ac - att.tohit
                        match rand 20 with
                        | 20 ->
                            let dmg = (resolve (fst att.damage |> Model.Dice.Roll.multiplyResultDice 2))
                            inflict true c.stats.name target dmg
                        | n when n >= toHit ->
                            let dmg = (resolve (fst att.damage))
                            inflict false c.stats.name target dmg
                        | _ ->
                            logMsg (sprintf "%s misses %s" c.stats.name (targetName target))
                | None ->
                    ()
    let updateHp pcs =
        pcs |> List.map (fun (pc:CharInfo) ->
            let c = guys |> Array.find (fun c -> c.team = Blue && c.stats.name = pc.src.name)
            { pc with CharInfo.usages = pc.usages |> Map.add "HP" hpMap.[c.id] })
    { state with log = log; pcs = updateHp state.pcs }

let healAndAdvance (pc:CharInfo) =
    let heal pc =
        { pc with CharInfo.usages = pc.usages |> Map.remove "HP" }
    if pc.src.classLevels.Length >= CharSheet.computeLevel pc.src.xp then
        // just heal
        heal pc
    else
        // advance to new level
        let goals = match pc.src.template with Some t -> t.advancementPriorities | _ -> List.init 20 (thunk Champion)
        let pc = { pc with src = { pc.src with classLevels = goals |> List.take (CharSheet.computeLevel pc.src.xp) }}
        { heal pc with hp = CharSheet.computeMaxHP pc.src }

let rest state =
    let nextTick = state.timeElapsed + 3600 * 8;
    let newDay = state.timeElapsed / (3600 * 24) <> nextTick / (3600 * 24)
    let state =
        { state with timeElapsed = nextTick; pcs = state.pcs |> List.map (fun pc -> if (CharInfo.getCurrentHP pc) > 0 then healAndAdvance pc else pc) }
        |> GameState.mapLog (Log.log "The party rests for 8 hours and heals")
    let paydayMessage() = state.pcs |> List.filter (fun pc -> pc.src.isNPC) |> List.map (fun pc -> sprintf "%s has earned %d gold pieces for %s faithful service." pc.src.name (pc.src.classLevels.Length * 50) (match pc.src.sex with Male -> "his" | Female -> "her"))
    if newDay then
        let payday = state.pcs |> List.sumBy(fun pc -> if pc.src.isNPC then pc.src.classLevels.Length * 100 else 0)
        { state with gp = state.gp - payday }
        |> GameState.mapLog (Log.logMany (paydayMessage()))
    else state

let retire state : Eventual<_,_,_> = queryInteraction {
    return! alert state (retirementMessage state)
    }

let startBattle (state:GameState) =
    let monsters, parEarned, totalXpEarned, gp = makeTower state.parEarned state.towerNumber
    let battle = { Battle1.create with stakes = Some <| Battle1.Stakes(parEarned, totalXpEarned, gp) }
    let enemies = monsters |> List.collect (fun (name, n) ->
        let ctor = MonsterManual.lookup name;
        List.init n (thunk ctor))
    let battle = state.pcs |> List.fold (flip (Battle1.addExistingCharacter TeamId.Blue)) battle
    let battle = enemies |> List.fold (fun b e -> b |> Battle1.addFreshCombatant TeamId.Red e) battle
    let state = { state with battle1 = Some battle }
    monsters, state

let enterTower (state: GameState) : Eventual<_,_,_> = queryInteraction {
    let monsters, state = startBattle state
    let! state =
        alert state <|
            (sprintf "You have reached the %s tower of Gate #%d. %s" (match state.towerNumber with | 1 -> "first" | 2 -> "second" | 3 -> "inner" | _ -> "final") state.gateNumber
            (battlecry state.pcs monsters))
    return state
    }

let finishTower (state:GameState) : Eventual<_,_,_> = queryInteraction {
    let (parEarned, totalXpEarned, gp) =
        match state.battle1 with
        | Some { stakes = Some (Battle1.Stakes(parEarned, totalXpEarned, gp)) } -> (parEarned, totalXpEarned, gp)
        | _ -> failwith "Should never finish a battle that wasn't started with some stakes"
    if state.pcs |> List.exists (fun pc -> (CharInfo.getCurrentHP pc) > 0) |> not then
        // everyone is dead
        return! alert state "You have all died!"
    else
        let livePCs = state.pcs |> List.sumBy (fun pc -> if pc.hp > 0 then 1 else 0)
        let xp = totalXpEarned / livePCs
        let state = { state with gp = state.gp + gp; pcs = state.pcs |> List.map (fun pc -> if pc.hp <= 0 then pc else { pc with src = { pc.src with xp = pc.src.xp + xp } }) |> List.sortByDescending (fun pc -> pc.hp > 0); parEarned = state.parEarned + parEarned }
        let msg = (sprintf "You have found %d gold pieces and earned %d experience points." gp xp)
        do! Query.alert state msg
        return state |> GameState.mapLog (Log.log msg)
    }

let showPCDetails game pc = queryInteraction {
    do! Query.character game pc
    return game
    }

let campaignMode() : Eventual<_,_,_> = queryInteraction {
    let state = GameState.empty
    let! state = getPCs state true false
    do! Query.alert state "Before you lies the Wild Country, the Gate of Doom. Prepare yourselves for death and glory!"
    return! enterTower state
    }

