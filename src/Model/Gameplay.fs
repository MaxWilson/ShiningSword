module Model.Gameplay
open Interaction
open Model.Types
open Model.Operations
open Model.Tables
open Common
open System
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
            match sex, Model.Names.names |> List.tryFind (fun ((n,t),_) -> t = "Last" && n = nameType) with
            | _, Some (_, lst) -> chooseRandom lst
            | Male, None -> sprintf "%s%s" (chooseRandom [|" ben ";" son ";" dak ";" s'";" "|]) (chooseRandom chosenList)
            | Female, None -> (chooseRandom [|" bat " + (chooseRandom chosenList); (chooseRandom chosenList) + "dotter"; "d'"+(chooseRandom chosenList); chooseRandom chosenList|])
        let name = sprintf "%s %s" firstname lastname
        return (name, sex)
    }

let rec getPCs (state: GameState) firstPerson isFriend : Eventual<_,_,_> = queryInteraction {
    let! name, sex = getNameAndSex state firstPerson isFriend
    let pc = PC.create name sex

    let state = { state with pcs = state.pcs@[pc]; gp = if firstPerson || isFriend then state.gp else state.gp - 100 }
    let! more = Query.choose state (sprintf "Do you want to recruit%shelp? It costs 100 gold pieces up front plus salary." (if state.pcs.Length = 1 then " " else " more "))
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

let makeTower pcs parXpEarned nTower =
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
    let gpEarned = rand (Math.Log (float budget) |> int)
    e, cost, earned, xpEarned, gpEarned

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
    let gpEarned = rand (Math.Log10 (float budget) |> int) // make less money from random encounters
    e, c, earned, xpEarned, gpEarned

let battlecry (pcs: StatBlock list) monsters =
    let plural = match monsters with [_, 1] -> false | _ -> true
    let cries = [|
        sprintf (if plural then """"Give me blood!" you scream as %s attack.""" else """"Give me blood!" you scream as %s attacks.""")
        sprintf (if plural then """"Not again!" you groan, as %s attack.""" else """"Not again!" you groan, as %s attacks.""")
        sprintf """"Blood or death!" shout your companions at %s, as they draw their weapons."""
        sprintf """%s grins crazily and gestures behind you. You turn and see %s!""" ((chooseRandom (Array.ofList pcs)).name)
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
    if state.gp >= 0 then
        (sprintf "%s happily retire from adventuring and spend the rest of your life living off %d gold pieces that you found." (state.pcs |> List.map (fun pc -> pc.name) |> oxfordJoin |> sprintf "%s, you") state.gp)
    else
        (sprintf "%s glumly retire from adventuring and spend the rest of your life paying off the %d gold pieces that you owe." (state.pcs |> List.map (fun pc -> pc.name) |> oxfordJoin |> sprintf "%s, you") -state.gp)

// super-simple fight resolver currently
let fight encounter state =
    let goodguys = state.pcs |> Seq.map (fun pc -> pc.name, ref <| pc.hp) |> Array.ofSeq
    let badguys = normalize encounter |> Seq.map (fun name -> name, ref <| rand 10) |> Array.ofSeq // random number of HP
    let mutable log = state.log
    let inflict (me, _) (targetName, hp) dmg =
        hp := !hp - dmg
        if !hp > 0 then
            log <- Log.log (sprintf "%s hits %s for %d points of damage! (%d HP remaining)" me targetName dmg !hp) log
        else
            log <- Log.log (sprintf "%s hits %s for %d points of damage! %s dies!" me targetName dmg targetName) log
    let randomTarget targets =
        let liveTargets = targets |> Array.filter (fun (name, hp) -> !hp > 0)
        if liveTargets.Length > 0 then
            Some <| chooseRandom liveTargets
        else None
    let alive guys =
        guys |> Array.filter (fun (name, hp) -> !hp > 0)
    while goodguys |> alive |> Array.exists (thunk true)
            && badguys |> alive |> Array.exists (thunk true) do
        for g in goodguys |> alive |> shuffleCopy do
            match badguys |> randomTarget with
            | Some target ->
                let dmg = rand 10
                inflict g target dmg
            | None -> ()
        for b in badguys |> alive |> shuffleCopy do
            match goodguys |> randomTarget with
            | Some target ->
                let dmg = rand 10
                inflict b target dmg
            | None -> ()
    let updateHp pcs =
        pcs |> List.mapi (fun i pc -> { pc with hp = !(snd goodguys.[i]) })
    { state with log = log; pcs = updateHp state.pcs }

let rec doRest (state: GameState) : Eventual<_,_,_> = queryInteraction {
    let state =
        { state with timeElapsed = state.timeElapsed + 3600 * 8; pcs = state.pcs |> List.map (fun pc -> if pc.hp > 0 then { pc with hp = List.init (PC.computeLevel pc.xp) (thunk PC.Fighter) |> PC.computeHP pc.con } else pc) }
        |> GameState.mapLog (Log.log "The party rests for 8 hours and heals")
    match! Query.choose state (sprintf "You have earned %d XP and %d gold pieces, and you've been adventuring for %s. What do you wish to do next?" state.pcs.[0].xp state.gp (timeSummary state.timeElapsed)) ["Advance"; "Rest"; "Return to town"] with
        | "Advance" -> return! doTower (advance state)
        | "Rest" -> return! doRest (advance state)
        | "Return to town" ->
            return! alert state (retirementMessage state)
        | _ -> return state
        }
and doTower (state: GameState) : Eventual<_,_,_> = queryInteraction {
    let e, c, parEarned, totalXpEarned, gp = makeTower state.pcs state.parEarned state.towerNumber
    let! state = alert state <|
        (sprintf "You have reached the %s tower of Gate #%d. %s" (match state.towerNumber with | 1 -> "first" | 2 -> "second" | 3 -> "inner" | _ -> "final") state.gateNumber
            (battlecry state.pcs e))
    let state = fight e state
    if state.pcs |> List.exists (fun pc -> pc.hp > 0) |> not then
        // everyone is dead
        return! alert state "You have died!"
    else
        let livePCs = state.pcs |> List.sumBy (fun pc -> if pc.hp > 0 then 1 else 0)
        let xp = totalXpEarned / livePCs
        let state = { state with gp = state.gp + gp; pcs = state.pcs |> List.map (fun pc -> if pc.hp <= 0 then pc else { pc with xp = pc.xp + xp }) |> List.sortByDescending (fun pc -> pc.hp > 0); parEarned = state.parEarned + parEarned }
        let! state = alert state (sprintf "You have found %d gold pieces and earned %d experience points." gp xp)
        match! Query.choose state (sprintf "You have earned %d XP and %d gold pieces, and you've been adventuring for %s. What do you wish to do next?" state.pcs.[0].xp state.gp (timeSummary state.timeElapsed)) ["Advance"; "Rest"; "Return to town"] with
        | "Advance" -> return! doTower (advance state)
        | "Rest" -> return! doRest (advance state)
        | "Return to town" ->
            return! alert state (retirementMessage state)
        | _ -> return state
    }
let doGate state : Eventual<_,_,_> = queryInteraction {
    return! (doTower state)
    }

let game() : Eventual<_,_,_> = queryInteraction {
    let state = GameState.empty
    let! state = getPCs state true false
    let! state = alert state "Before you lies the Wild Country, the Gate of Doom. Prepare yourselves for death and glory!"
    return! doGate state
    }
