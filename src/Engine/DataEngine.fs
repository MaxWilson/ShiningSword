(* The DataEngine consists of a domain-specific language (DSL) called Ribbit for RPG data manipulation, a parser
for turning text written in Ribbit into executable programs, and an interpreter for executing those programs, together with
F# interfaces for implementing the language host facilities like saving and loading data asynchronously.

Ribbit is inspired by Haskell and Prolog. You can think of it as performing a series of substitutions on the original
program until the result converges. Whitespace is significant. Ribbit is intended to be used in interactive mode within
a REPL loop, referencing chunks of pre-written code embodying e.g. the 5E ruleset with a given DM's houserules.

A simple Hello World program in Ribbit:

```
declare number hp, dmg default 0
define status = when dmg >= hp then dead when dmg > 0 then wounded else alive
hp = 10
status
```

This should evaluate to 'alive'. A slightly more complex program including actions might look like this:

```
module 5E =
  declare number hp, dmg default 0, ac, tohit
  declare dice damage, extraCritDice
  declare flag damageType, resistances, immunities, vulnerabilities
  define status = when dmg >= hp then dead when dmg > 0 then wounded else alive
  whenever attack target [with weapon]:
    let weapon = weapon or me.defaultAttack
    let attackRoll = roll d20
    when attackRoll = 20 then hit (crit)
    when attackRoll + weapon.toHit >= target.AC then hit
    else miss
  whenever hit:
    let dmg = roll weapon.damage
    log [me.name] hits [target.name] for [dmg] points of damage!
    damage target dmg weapon.damageType
  whenever hit is crit:
    log Critical hit!
    change dmg add (roll (diceOnly me.weaponDamage))
  whenever hit is crit and me has extraCritDice:
    change dmg add (roll me.extraCritDice)
  whenever miss:
    log [me.name] misses [target.name].
  whenever damage target damage:
    change target.dmg add damage
    when target is concentrating:
        target checkConcentration damage
  whenever damage type and target.resistances has type:
    change damage / 2 round down
  whenever damage type and target.immunities has type:
    change damage 0
  whenever damage type and target.vulnerabilities has type:
    change damage * 2
  whenever checkConcentration damage:
    let targetNumber = max(damage / 2 round down, 10)
    let roll = roll d20
    when roll + me.savingThrow constitution < targetNumber then
        log [me.name] has lost concentration on [me.concentrating]!
        change me.concentrating nothing

import 5E

add bob, john
bob.hp = 10, ac = 12, tohit = 4, weaponDamage = 2d8+4
john.hp = 10

bob attack john
```

At the end of this program, if rolls are [d20 => 18, 2d8+4 => 12], john.status should evaluate to "dead".

*)

module DataEngine

type Callback<'T> = unit -> 'T
type Label = string

type IDataStorage =
    abstract member Save: 'T -> Label -> Callback<unit>
    abstract member Load: Label -> Callback<'T>

type Model = string list
type Cmd = Quit | Log of string | Roll of Model.Types.Roll
open Packrat
open Model
open Common
open Interaction

let (|Cmd|) = function
    | Battle.Parse.Roll(r, End) -> Roll r
    | Str "q" End -> Quit
    | Str "quit" End -> Quit
    | Any(msg, End) -> Log(msg)
    | _ -> failwith "Should never get here--Any() should match everything"

module Roll =
    let eval (r:Model.Types.Roll) = r.bonus + List.sum (List.init r.n (thunk1 rand r.die))
    let render (r:Model.Types.Roll) =
        if r.bonus = 0 then sprintf "%dd%d" r.n r.die
        elif r.bonus < 0 then sprintf "%dd%d%d" r.n r.die r.bonus
        else sprintf "%dd%d+%d" r.n r.die r.bonus

module DataModel =
    type State = string list
    type Input = string
    type Response = string option * State
    let log state msg = List.append state [msg]
    type GameLoop = Eventual<Input, Response, State>

open DataModel
let gameLoop (storage: IDataStorage) : GameLoop =
    let rec consume (state: DataModel.State) input =
        match Packrat.ParseArgs.Init input with
        | Cmd c ->
            match c with
            | Quit -> Eventual.Final state
            | Log msg -> Eventual.Intermediate((None,state), (consume (log state msg))) // todo: append efficiently
            | Roll r ->
                let result = (Roll.eval r)
                let logEntry = (sprintf "%s: %d" (Roll.render r) result)
                Eventual.Intermediate((Some (sprintf "%d" result), state), (consume (log state logEntry)))
    Interaction.Intermediate((None,[]), consume [])

let consoleExecute (gameLoop: GameLoop) =
    let rec loop = function
        | Intermediate((responseText,logs), answer) ->
            match responseText with
            | Some msg -> printfn "%s" msg
            | None -> ()
            let cmd = System.Console.ReadLine()
            loop (answer cmd)
        | Final logs ->
            ()
    loop gameLoop

type LocalStorage() =
    interface IDataStorage with
        member this.Save label d = Common.notImpl()
        member this.Load label = Common.notImpl()
gameLoop (LocalStorage()) |> consoleExecute
