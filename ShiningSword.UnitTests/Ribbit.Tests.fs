module Ribbit.Tests

open Common
open Expecto
open Expecto.Flip
open FsCheck
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Swensen.Unquote
open Domain.Ribbit

let define (rules: string) (ribbit:RibbitData) = notImpl ribbit
let execute (commands: string) (ribbit:RibbitData) = notImpl ribbit
let withRolls rollPlugin (ribbit:RibbitData) = notImpl ribbit
let addCreatures team creatureList src (ribbit:RibbitData) = notImpl ribbit
type Unknown() =
    do
        notImpl()

let trimFront (input: string) =
    let newLine = "\n" // can't use System.Environment.Newline for F# compile-time strings, which are just \n apparently
    let lines = input.Split(newLine) |> Array.skipWhile System.String.IsNullOrWhiteSpace
    if lines.Length = 0 then System.String.Empty
    else
        let countWhitespace (line:string) =
            let rec loop i =
                if i >= line.Length || line.[i] |> System.Char.IsWhiteSpace |> not then i
                else loop (i+1)
            loop 0
        let skipWhitespace n (line:string) =
            let rec loop i =
                if i >= line.Length then System.String.Empty
                elif line.[i] |> System.Char.IsWhiteSpace |> not || i >= n then line.Substring(i).TrimEnd()
                else loop (i+1)
            loop 0
        let prefixN = lines.[0] |> countWhitespace
        lines |> Array.map (skipWhitespace prefixN) |> fun x -> System.String.Join(newLine, x).TrimEnd()

let basicAttack = testCase "Basic attack definition can be parsed" <| fun _ ->
    let ribbitDef = """
        define attack as
            costs 1 attack
            set attackRoll = roll 1d20
    """
    ()

module Expect =
    let fail msg = Expect.equal msg true false

[<Tests>]
let tests = testList "Ribbit.scenario" [

    testCase "Lemma 1" <| fun _ ->
        let x = """
            abc
            def
               hij
               k
            ab
             a
            """
        test <@ "abc\ndef\n   hij\n   k\nab\n a" = trimFront x @>
        Expect.equal "Should trim first line and prefixes evenly" "abc\ndef\n   hij\n   k\nab\n a" (trimFront x)
    testCase "Basic ribbit commands (no stat manipulation): adding, defining, renaming" <| fun _ ->
        let mutable r = Ribbit.Fresh
        let exec txt =
            match Packrat.ParseArgs.Init(txt, r) with
            | Domain.Ribbit.Commands.Commands (cmds, Packrat.End) ->
                r <- cmds |> List.fold (flip Domain.Ribbit.Commands.executeCommand) r
            | _ -> Expect.fail $"Could not parse '{txt}' as a Ribbit command"
        let execs txts = txts |> List.iter exec
        exec "add Bob"
        Expect.equal "We just added Bob to a fresh Ribbit" "Bob" (r.data.roster |> Map.keys |> Seq.head)

        execs ["define Beholder"; "define Grue"; "add Grue, Grue, Grue, Grue, Beholder"]
        Expect.equal
            "We just added five monsters"
            "Bob, Grue #1, Grue #2, Grue #3, Grue #4, Beholder #1"
            (r.data.roster |> Seq.map(function KeyValue(name, id) -> name, id) |> List.ofSeq |> List.sortBy snd |> List.map fst |> String.join ", ")

        execs ["rename Grue #1 Ned"; "rename Grue #2 Pete"; "remove Grue #3"]
        Expect.equal
            "We just renamed two Grues and deleted #3"
            "Bob, Ned, Pete, Grue #4, Beholder #1"
            (r.data.roster |> Seq.map(function KeyValue(name, id) -> name, id) |> List.ofSeq |> List.sortBy snd |> List.map fst |> String.join ", ")

    testCase "Logs should be capable of adding entries" <| fun _ ->
        let r =
            Ribbit.Fresh
            |> Commands.executeCommand (Commands.AddLogEntry([], "The world awakens"))
        Expect.equal "We just awakened the world" "The world awakens" (getLogMsg 0 r)

    testCase "Time travel: log entries should be tagged with ids that support time travel" <| fun _ ->
        let mutable r = Ribbit.Fresh
        let exec txt =
            match Packrat.ParseArgs.Init(txt, r) with
            | Domain.Ribbit.Commands.Commands (cmds, Packrat.End) ->
                r <- cmds |> List.fold (flip Domain.Ribbit.Commands.executeCommand) r
            | _ -> Expect.fail $"Could not parse '{txt}' as a Ribbit command"
        let execs txts = txts |> List.iter exec
        execs ["add Bob"; "define Giant"; "add Giant"]
        Expect.equal "There should be Bob and a Giant now" 2 (r.data.roster.Count)
        r <- r |> Commands.executeCommand (Commands.AddLogEntry([], "Harry and Lara are coming!"))
        Expect.equal "There should be one log entry now" 1 (r.data.eventRoots.Length)
        execs ["add Lara"; "add Harry"]
        Expect.equal "There should be four people now" 4 (r.data.roster.Count)
        Expect.equal "History should still reflect only two people back when message about Harry and Lara was logged"
            2 (r.data.events[r.data.eventRoots[0]].timeTravelIndex |> r.rewindTo).data.roster.Count

    ptestCase "Simple attacks" <| fun _ ->
        let rules = """
        action is a resource
        round is a resource:
            1 round gives 1 action, 1 bonus action, 1 reaction, N movement for N = self.movement
        attacks is a resource:
            1 attacks for 1 action
            2 attacks for 1 action if you have Extra Attack
            3 attacks for 1 action if you have Extra Attack 2
            4 attacks for 1 action if you have Extra Attack 3
            if you have Crossbow Expert and have attacked this round, 1 attack with hand crossbow for 1 bonus action

        [I/me] takeDamage amount event:
            me.damageTaken <- me.damageTaken + amount
            log "{I} takes {inflicted} HP of damage!"
            return amount

        before [I/me] takeDamage amount if me.Raging:
            amount <- amount / 2

        critDamage dmg event:
            return dmg + noMods dmg

        after [I/me] critDamage dmg if me.BrutalCritical:
            // triple crit damage if BrutalCritical
            return returnValue + (noMods dmg)

        [I/me] crit [enemy] [with weapon] event:
            let dmg = roll (weapon.damage for me) for "damage"
            let dmg = trigger critDamage dmg
            let inflicted = trigger enemy takeDamage dmg
            log "{I} crits {enemy} for {inflicted} HP!"

        [I/me] hit [enemy] [with weapon] event:
            let dmg = roll (weapon.damage for me) for "damage"
            let inflicted = trigger enemy takeDamage dmg
            log "{I} hits {enemy} for {inflicted} HP!"

        [I/me] miss [enemy] [with weapon] event:
            log "{I} misses {enemy}!"

        [I/me] attack [enemy] [with weapon] affordance:
            costs me 1 attacks
            let toHit = (weapon.toHit for me)
            let attackRoll = roll d20 with bonus toHit for "attack enemy"
            if attackRoll >= (me.critRange | 20) then
                trigger crit
            elif attackRoll + toHit >= enemy.AC then
                trigger hit
            else
                trigger miss

        opportunity attack event:
            costs 1 reaction when enemy is about to move out of reach
            attack enemy, costs 1 reaction instead of 1 attack

        attack behavior:
            find nearest living target unless already have one
            attack target until dead

        monster Orc: AC 13, HP 2d8+6, attack with greataxe at +5 for d12+3 slashing, attack with javelin at +5 for d6+3 piercing from 30/120, bonus action Dash towards hostile
        monster Efreeti: plural Efreet, Large Elemental, AC 17, HP 16d10+112, Speed 40/fly 60,
            Attack with scimitar at +10 for 2d6+6 slashing and 2d6 fire,
            Attack with Hurl Flame at +7 for 5d6 fire damage from 120,
            Multiattack: 2x scimitar or 2x Hurl Flame.
        """
        let maxRolls = notImpl()
        let getIdByName = notImpl()
        let hpRemaining: NumberProperty = notImpl()
        let isOkay: BoolProperty = notImpl()
        let expectedHP: int = notImpl()
        let mockRoster = notImpl()
        let ribbit =
            RibbitData.fresh |> define rules
            |> withRolls maxRolls
            |> addCreatures 2 ["Orc", 5] None
            |> addCreatures 1 ["Elemonk", 1] (Some mockRoster)
            |> execute "run round"
        let monkId = ribbit |> getIdByName "Elemonk"
        //test<@ (hpRemaining.Get monkId ribbit) = expectedHP @>
        //test<@ isOkay.Get monkId ribbit @>
        //for i, remainingHP in [1,0;2,3;3,15;4,15;5,15] do
        //    let orcId = ribbit |> getIdByName $"Orc #{i}"
        //    test<@ (hpRemaining.Get orcId ribbit) = remainingHP @>
        //    test<@ isOkay.Get orcId ribbit = (remainingHP > 0) @>
        ()
    ]

