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

open Packrat
open Model
open Common
open Interaction
open Model
open Model.Types.Battle2
open Model.Functions.Battle2

let (|Cmd|_|) = function
    | Battle.Parse.Roll(r, End) -> Some (Roll r)
    | Word("show", End) -> Some (ShowLog <| Some 3)
    | Word("show", (Int(n, End))) -> Some (ShowLog <| Some n)
    | Word("show", Word("all", End)) -> Some (ShowLog None)
    | Str "q" End -> Some Quit
    | Str "quit" End -> Some Quit
    | OWS(End) -> None
    | Any(msg, End) -> Some (Log(msg))
    | _ -> failwith "Should never get here--Any() should match everything"

let execute combineLines (storage: IDataStorage) (state:State) input: State =
    match Packrat.ParseArgs.Init input with
    | Cmd c ->
        let state =
            { state with
                view = { state.view with lastCommand = Some c; lastInput = Some input }
                }
        match c with
        | Quit -> state |> Lens.over lview (fun s -> { s with finished = true })
        | Log msg -> state |> log msg
        | ShowLog n ->
            let log = state.data.log |> Functions.Log.extract
            let lines = log |> List.collect id
            let txt = (match n with Some n when n < lines.Length -> lines.[(lines.Length - n)..] | _ -> lines) |> String.join "\n"
            { state with
                view = { state.view with lastOutput = Some txt }
                }
        | Roll r ->
            let result = (Dice.Roll.eval r)
            let resultTxt = Model.Dice.Roll.renderExplanation result
            let logEntry = (sprintf "%s: %s" input resultTxt) // todo: is newline + spaces the right separator for roll outputs?
            { state with
                view = { state.view with lastOutput = Some resultTxt }
                } |> log logEntry
    | _ ->
        // invalid command (probably pure whitespace)
        { state with
            view = { state.view with lastInput = Some input }
            }
