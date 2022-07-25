module Ribbit.Tests

open Common
open Expecto
open FsCheck
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Swensen.Unquote
open Domain.Ribbit

let define (rules: string) (ribbit:Ribbit) = notImpl ribbit
let execute (commands: string) (ribbit:Ribbit) = notImpl ribbit
let withRolls rollPlugin (ribbit:Ribbit) = notImpl ribbit

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

[<Tests>]
let tests = testList "ribbit.scenario" [

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
        ("abc\ndef\n   hij\n   k\nab\n a", "Should trim first line and prefixes evenly")
        ||> Expect.equal (trimFront x)
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

        """
        let maxRolls = notImpl()
        let withCreatures = notImpl()
        let fiveOrcsVsElemonk9 = notImpl()
        let getFriendly = notImpl()
        let hpRemaining: NumberProperty = notImpl()
        let isOkay: BoolProperty = notImpl()
        let expectedHP: int = notImpl()
        let ribbit =
            Ribbit.fresh |> define rules
            |> withRolls maxRolls
            |> withCreatures fiveOrcsVsElemonk9
            |> execute "run round"
        let monkId = ribbit |> getFriendly "Elemonk"
        test<@ (hpRemaining.Get monkId ribbit) = expectedHP @>
        test<@ isOkay.Get monkId ribbit @>
        for i, remainingHP in [1,0;2,3;3,15;4,15;5,15] do
            let orcId = ribbit |> getFriendly $"Orc #{i}"
            test<@ (hpRemaining.Get orcId ribbit) = remainingHP @>
            test<@ isOkay.Get orcId ribbit = (remainingHP > 0) @>
    ]

