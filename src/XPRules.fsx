// scripts for tinkering with XP and level advancement rules

#I __SOURCE_DIRECTORY__
#load @"Common.fs"
#load @"Model\Types.fs"
#load @"Model\Tables.fs"

open Common
open Model.Tables

// I invented some alternate XP rules (inspired by reading about cybernetics) that are designed to stabilize (N)PCs at
// a level where they are competent to handle what they are already fighting. The scripts here are designed to help me
// explore the effects of my rules so I can tweak them, and also so I can e.g. calculate fixpoints of what level an
// NPC who is competent at fighting bears would max out at. (If you fight an infinite number of CR 1 grizzly bears,
// you'll max out at level 7: maxLevel 1 [1.].)

// Design logic: I base it off of actual XP earned instead of difficulty because I want to incentivize fighting new and
// exciting challenges. I don't WANT fighting six werewolves at a time (~10K difficulty) to max out at the same point as
// fighting beholders. I based it off XP earned-so-far mainly because the XP-for-next-level curve is too flat--there's
// not enough differentiation between 8th level and 20th level. The fact that XP earned-so-far is easier to calculate is
// a nice bonus, but the motivation was curve-fitting. The limit is generous enough that under normal scenarios you
// should always gain XP (the minimal range is usually deep in the Easy zone except for large hordes of goblins and such)
// so the main impact will probably be on heterogenous levels: a 12th level PC shepherding a group of level 1 PCs will
// not gain XP when they do unless the scenario is difficult enough for it to be somewhat hard to keep the level 1 PCs
// alive.

// Also, there is some incentive to avoid being shepherded, because XP is divided proportionate to levels/CR, not just total
// number of PCs. This also acts as a bit of a disincentive to bring along summons/animated undead, because they consume a
// share of XP.

let calculateXPReward pcLevels monsterLevels =
    let earnedXP = monsterLevels |> List.sumBy (fun i -> (monsterCR |> Array.find (fun cr -> cr.CR >= i)).XPReward)
    let pcTotal = pcLevels |> List.sum
    pcLevels |> List.map (fun myLevel -> match earnedXP * myLevel/pcTotal with | reward when reward >= levelAdvancement.[myLevel-1].XPReq/100 -> reward | _ -> 0)
// E.g. 3 8th level PCs and a 12th level PC vs. 6 werewolves
calculateXPReward [8;8;8;12] (List.init 6 (thunk 3.))

// find the minimum number of enemies that would give XP
let minimalChallenge all pcLevels enemyCR =
    let rec loop numberOfEnemies =
        let reward =
            calculateXPReward pcLevels (List.init numberOfEnemies (thunk enemyCR))
        if reward |> (if all then List.every else List.exists) (flip (>) 0) then numberOfEnemies, reward
        else loop (numberOfEnemies + 1)
    loop 1
let minimalChallengeOne = minimalChallenge false // challenge at least one PC
let minimalChallengeAll = minimalChallenge true // must challenge all PCs
minimalChallengeOne [8;8;8;12] 3. // how many werewolves needed?
minimalChallengeOne [1;1;12] 3. // how many werewolves needed for the first-level guys to advance?
minimalChallengeAll [1;1;12] 3. // how many werewolves needed for everyone to advance?
minimalChallengeOne [1;1;12] 0.25 // how many goblins needed for first-level guys to advance when shepherded by a 20th-level guy?
minimalChallengeAll [1;1;12] 0.25 // how many goblins needed for everyone to advance?
minimalChallengeOne [8;20;13] 5.
minimalChallengeAll [8;20;13] 5. // how many fire elementals for the 20th-level guy to advance?
minimalChallengeOne [12;12;12] 5.
minimalChallengeOne [12;12;12] 0.25
minimalChallengeOne [20;20;20;20] 5.
minimalChallengeOne [20;20;20;20] 13.
minimalChallengeOne [20;20;20;20] 17.
minimalChallengeOne [8;20;13] 0.25
minimalChallengeOne [8;20;13] 0.5
minimalChallengeOne [20;20;20;20] 0.25
minimalChallengeOne [20;20;20;20] 0.5
minimalChallengeOne [20;20;20;20] 3.

// find the highest PC level that can be reached from a given monster group: the first level at which you stop gaining XP
let maxLevelWithAllies allies numberOfPCs monsterLevels =
    let rec loop level =
        let reward =
            calculateXPReward (List.init numberOfPCs (thunk level)@allies) monsterLevels
        if level >= 20 || reward |> List.exists (flip (=) 0) then level
        else loop (level + 1)
    loop 1
let maxLevel = maxLevelWithAllies []
maxLevel 4 (List.init 6 (thunk 3.)) // how high can you grind on groups of six werewolves?
maxLevel 1 (List.init 6 (thunk 3.)) // how high can you grind solo on groups of six werewolves?
maxLevel 1 (List.init 3 (thunk 3.)) // how high can you grind solo on groups of three werewolves?
maxLevel 1 (List.init 3 (thunk 0.5)) // how high can you grind solo on groups of three orcs?
maxLevel 4 (List.init 6 (thunk 3.) @ [13.]) // what about six werewolves and a beholder
maxLevel 4 (List.init 6 (thunk 3.) @ [9.]) // what about six githyanki and an abominable yeti?
maxLevel 1 (List.init 6 (thunk 3.) @ [9.]) // what about six githyanki and an abominable yeti, solo?
maxLevel 4 (List.init 30 (thunk 0.25)) // thirty goblins?
maxLevel 4 (List.init 20 (thunk 0.5)) // twenty hobgoblins?
maxLevel 1 [1.] // how high can you grind solo on one grizzly bear at a time
maxLevel 4 [1.] // how high can you grind on one grizzly bear at a time with a group of four friends
maxLevelWithAllies (List.init 8 (thunk 1)) 1 (List.init 6 (thunk 3.)) // how high can a Necromancer with eight skeletons get on groups of six githyanki?
maxLevelWithAllies (List.init 8 (thunk 1)) 1 (List.init 6 (thunk 0.5)) // how high can a Necromancer with eight skeletons get on groups of six orcs?
maxLevelWithAllies (List.init 24 (thunk 1)@[5]) 1 (List.init 6 (thunk 3.)) // can 24 skeletons and a Planar Bound fire elemental get you to 20th level on groups of six githyanki at a time? Nope, it's too easy.
