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

type Callback<'T> = 'T -> unit
type Label = string

type IDataStorage =
    abstract member Save: Label -> 'T -> Callback<Result<unit, string>> -> unit
    abstract member Load: Label -> Callback<Result<'t, string>> -> unit

open Packrat
open Model
open Common
open Interaction
open Model
open Model.Types.Battle2
open Model.Functions.Battle2

#nowarn "40"
let (|LogWithEmbeddedRolls|_|) =
    let openBracket = Set.ofList ['[']
    let questionMark = Set.ofList ['?']
    let rec (|Chunkify|_|) = pack <| function
        | CharsExcept openBracket (prefix, Str "[" (Dice.Parse.Roll(roll, (OWS(Str "]" (Chunkify(chunks, ctx))))))) -> Some([LogChunk.Text prefix; LogChunk.Roll roll] @ chunks, ctx)
        | CharsExcept openBracket (prefix, Str "[" (Dice.Parse.CommaSeparatedRolls(rolls, (OWS(Str "]" (Chunkify(chunks, ctx))))))) -> Some(LogChunk.Text prefix::((rolls |> List.map LogChunk.Roll) |> List.join (LogChunk.Text ", ")) @ chunks, ctx)
        | Str "[" (Dice.Parse.Roll(roll, (OWS(Str "]" (Chunkify(chunks, ctx)))))) -> Some([LogChunk.Roll roll] @ chunks, ctx)
        | Str "[" (Dice.Parse.CommaSeparatedRolls(rolls, (OWS(Str "]" (Chunkify(chunks, ctx)))))) ->
            Some((rolls |> List.map LogChunk.Roll |> List.join (LogChunk.Text ",")) @ chunks, ctx)
        | CharsExcept questionMark (prefix, Str "?" (Dice.Parse.Roll(roll, (End as ctx)))) -> Some([LogChunk.Text (prefix + "? "); LogChunk.Roll roll], ctx)
        | Any(msg, ctx) -> Some ((if System.String.IsNullOrWhiteSpace msg then [] else [LogChunk.Text msg]), ctx)
        | v -> matchfail v
    function
    | Chunkify(chunks, ctx) -> Some(Log chunks, ctx)
    | _ -> None

let (|Cmd|_|) = pack <| function
    | Dice.Parse.Roll(r, (End as ctx)) -> Some (Roll r, ctx)
    | Word("show", (End as ctx)) -> Some (ShowLog <| Some 3, ctx)
    | Word("show", (Int(n, (End as ctx)))) -> Some (ShowLog <| Some n, ctx)
    | Word("show", Word("all", (End as ctx))) -> Some (ShowLog None, ctx)
    | Word(AnyCase("q" | "quit"), (End as ctx)) -> Some (Quit, ctx)
    | Word("save", Words(label, (End as ctx))) -> Some (Save label, ctx)
    | Word("load", Words(label, (End as ctx))) -> Some (Load label, ctx)
    | Word("clear", (End as ctx)) -> Some(Clear, ctx)
    | OWS(End) -> None
    | LogWithEmbeddedRolls(cmd, (End as ctx)) -> Some(cmd, ctx)
    | v -> matchfail v

let execute combineLines (explanationToString: Model.Types.Roll.Explanation -> string) (storage: IDataStorage) (state:State) (input: string) (return': Callback<State>): unit =
    let cmdPrefix txt = "* " + txt // Visually distinguish commands from log outputs in the output log.
                                    // May eventually lift this distinction to higher levels so
                                    // web view can italizicize or something instead.
    let rec exec state c return' =
        let state =
            { state with
                view = { state.view with lastCommand = Some c; lastInput = Some input }
                }
        match c with
        | Quit -> return' (state |> Lens.over lview (fun s -> { s with finished = true }), None)
        | Log chunks ->
            let eval = function
                | LogChunk.Text msg -> msg, []
                | LogChunk.Roll r ->
                    let result = Dice.Roll.eval r
                    let getSummary (Model.Types.Roll.Explanation(_, summary, _)) = summary
                    result.value.ToString(), [(result |> Dice.Roll.renderExplanation |> explanationToString).Trim()]
            let chunks' = chunks |> List.map eval
            let mainText = (String.join emptyString (chunks' |> List.map fst))
            let explanations = chunks' |> List.collect snd
            let resultText = combineLines (explanations @ [mainText])
            match chunks with
            | [LogChunk.Text _] ->
                // if they just logged text, don't echo the log entry to output, and don't double log the text
                return' (state |> log resultText, None)
            | _ ->
                let logEntry = combineLines [cmdPrefix input; resultText] // log the substituted values
                return' (state |> log logEntry, Some resultText)
        | ShowLog n ->
            let log = state.data.log |> Functions.Log.extract
            let lines = log |> List.collect id
            let txt = (match n with Some n when n < lines.Length -> lines.[(lines.Length - n)..] | _ -> lines) |> combineLines
            return' (state, Some txt)
        | Roll r ->
            let result = Dice.Roll.eval r
            let resultTxt = Dice.Roll.renderExplanation result |> explanationToString
            let logEntry = (cmdPrefix (sprintf "%s: %s" input resultTxt)) // todo: is newline + spaces the right separator for roll outputs?
            return' ((state |> log logEntry), Some resultTxt)
        | Save label ->
            storage.Save label state.data (function Ok _ -> return' (state, sprintf "Saved '%s'" label |> Some) | Error err -> return' (state, sprintf "Could not save '%s': '%s'" label err |> Some))
        | Load label ->
            storage.Load<Data> label (function Ok data -> exec { state with data = data; view = emptyView } (ShowLog None) return' | Error err -> return' (state, sprintf "Could not save '%s': '%s'" label err |> Some))
        | Clear ->
            return' (Model.Functions.Battle2.init(), None)
    match Packrat.ParseArgs.Init input with
    | Cmd(cmd, End) ->
        let callback (state, output) =
            return' { state with
                        view = { state.view with lastOutput = output }
                        }
        exec state cmd callback
    | _ ->
        // invalid command (probably pure whitespace)
        return'
            { state with
                view = { state.view with lastInput = Some input; lastOutput = None }
                }
