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
    abstract member Save: Label -> Model.Types.Battle2.Data -> Callback<Result<unit, string>> -> unit
    abstract member Load: Label -> Callback<Result<Model.Types.Battle2.Data, string>> -> unit

open Packrat
open Model
open Common
open Interaction
open Model
open Model.Types.Battle2
open Model.Functions.Battle2

#nowarn "40"
module Parse =
    let (|Roster|_|) = Packrat.ExternalContextOf<Model.Types.Battle2.Roster>
    let (|Keyword|_|) (word:string) =
        function
        | Word(w, ctx) when String.equalsIgnoreCase word w -> Some(ctx)
        | _ -> None
    let (|AnyCaseWord|_|) = function
        | Word(AnyCase ws, ctx) -> Some(ws, ctx)
        | _ -> None
    let (|ValidName|_|) =
        // in this case we need to do something a little tricky: detect names by prefix
        pack <| function
        | Roster(roster) as ctx ->
            let isValidName name = roster |> Roster.tryName name |> Option.isSome
            match ctx with
            | LongestSubstringWhere isValidName 18 (name, ctx) ->
                roster |> Roster.tryName name |> Option.map(fun id -> id, ctx)
            | _ -> None
        | _ -> None
    let (|CombatantProperty|_|) = pack <| function
        | ValidName(id, Str "'s" (Word(propertyName, ctx))) -> Some((id, propertyName), ctx)
        | ValidName(id, Str "." (Word(propertyName, ctx))) -> Some((id, propertyName), ctx)
        | _ -> None
    let (|Expression|_|) = pack <| function
        | CombatantProperty(p, ctx) -> Some(GetValue p, ctx)
        | Dice.Parse.Roll(r, ctx) -> Some(Expression.Roll r, ctx)
        | AnyCaseWord(("avg" | "average"), Dice.Parse.Roll(r, ctx)) -> Some(Expression.Average r, ctx)
        | _ -> None
    let rec (|CommaSeparatedExpressions|_|) = pack <| function
        | CommaSeparatedExpressions(exprs, OWS(Str "," (OWS (Expression(e, rest))))) -> Some(exprs @ [e], rest)
        | Expression(e, rest) -> Some([e], rest)
        | _ -> None
    let (|Statement|_|) = pack <| function
        | Keyword "set" (CombatantProperty((id,property), Keyword "to" ((Expression(e, ctx))))) -> Some(SetValue(id, property, e), ctx)
        | CombatantProperty((id,property), OWS(Str "=" (OWS(Expression(e, ctx))))) -> Some(SetValue(id, property, e), ctx)
        | ValidName(id, Keyword "has" (Dice.Parse.Roll(r, Word(propertyName, Optional "." (Optional "!" ctx))))) -> Some(SetValue(id, propertyName, Expression.Roll r), ctx)
        | ValidName(id, Keyword "gains" (Dice.Parse.Roll(r, Word(propertyName, Optional "." (Optional "!" ctx))))) -> Some(AddToValue(id, propertyName, Expression.Roll r), ctx)
        | ValidName(id, AnyCaseWord(("loses"|"spends"), (Dice.Parse.Roll(r, Word(propertyName, Optional "." (Optional "!" ctx)))))) -> Some(AddToValue(id, propertyName, Expression.Roll (Dice.Roll.invert r)), ctx)
        | Expression(e, ctx) -> Some(Expression e, ctx)
        | _ -> None

    // dev string: Packrat.parserWithExternalContext (|Statement|_|) (Roster.empty |> Roster.add "Larry" |> Common.Result.OkOnly) "Larry gains 3d8 HP"

    let (|LogWithEmbeddedExpressions|_|) =
        let openBracket = Set.ofList ['[']
        let questionMark = Set.ofList ['?']
        let rec (|Chunkify|_|) = pack <| function
            | CharsExcept openBracket (prefix, Str "[" (Expression(e, (OWS(Str "]" (Chunkify(chunks, ctx))))))) -> Some([Expression.text prefix; e] @ chunks, ctx)
            | CharsExcept openBracket (prefix, Str "[" (CommaSeparatedExpressions(exprs, (OWS(Str "]" (Chunkify(chunks, ctx))))))) -> Some(Expression.text prefix::(exprs |> List.join (Expression.text ", ")) @ chunks, ctx)
            | Str "[" (Expression(e, (OWS(Str "]" (Chunkify(chunks, ctx)))))) -> Some([e] @ chunks, ctx)
            | Str "[" (CommaSeparatedExpressions(exprs, (OWS(Str "]" (Chunkify(chunks, ctx)))))) ->
                Some((exprs |> List.join (Expression.text ",")) @ chunks, ctx)
            | CharsExcept questionMark (prefix, Str "?" (Expression(e, (End as ctx)))) -> Some([Expression.text (prefix + "? "); e], ctx)
            | Any(msg, ctx) -> Some ((if System.String.IsNullOrWhiteSpace msg then [] else [Expression.text msg]), ctx)
            | v -> matchfail v
        function
        | Chunkify(chunks, ctx) -> Some(Log chunks, ctx)
        | _ -> None

    let (|Cmd|_|) =
        let (|DetailLevel|) = function | Int(n, ctx) -> Some n, ctx | ctx -> (Some 99), ctx // default to full detail (99 is high enough)
        pack <| function
        | Word(AnyCase("q" | "quit"), (End as ctx)) -> Some (Quit, ctx)
        | Word("show", (End as ctx)) -> Some (ShowLog (Some 3, None), ctx)
        | Word("show", (Int(n, (End as ctx)))) -> Some (ShowLog (Some n, None), ctx)
        | Word("show", Word("all", (End as ctx))) -> Some (ShowLog (None, None), ctx)
        | Word("detail", DetailLevel(level, (End as ctx))) -> Some (ShowLog (Some 1, level), ctx)
        | Word("detail", (Int(n, DetailLevel(level, (End as ctx))))) -> Some (ShowLog (Some n, level), ctx)
        | Word("detail", Word("all", DetailLevel(level, (End as ctx)))) -> Some (ShowLog (None, level), ctx)
        | Word("set", Word("detail", Int(n, (End as ctx)))) -> Some(SetOutputDetail (Some n), ctx)
        | Word("clear", Word("detail", Int(n, (End as ctx)))) -> Some(SetOutputDetail None, ctx)
        | Word("set", Word("log", Word("detail", Int(n, (End as ctx))))) -> Some(SetLogDetail n, ctx)
        | Word("save", Words(label, (End as ctx))) -> Some (Save label, ctx)
        | Word("load", Words(label, (End as ctx))) -> Some (Load label, ctx)
        | Word("clear", (End as ctx)) -> Some(Clear, ctx)
        | OWS(End) -> None
        | Keyword "add" (Words(name, ctx)) -> Some(AddCombatant name, ctx)
        | Statement(s, (End as ctx)) -> Some (Statement s, ctx)
        | LogWithEmbeddedExpressions(cmd, (End as ctx)) -> Some(cmd, ctx)
        | v -> matchfail v

open Common.Hierarchy
open Model.Types
open Model.Functions
let execute (storage: IDataStorage) (state:State) (input: string) (return': Callback<State>): unit =
    let rec exec (state: State) c return' =
        let eval e: Value * Log.Chunk option =
            // returns expression value as string + optional detailed explanation
            match e with
            | Expression.Value(v) -> v, None
            | Expression.Roll r ->
                let result = Dice.Roll.eval r
                Value.Number(result.value), (result |> Dice.Roll.renderExplanation |> Dice.Roll.toLogChunk) |> Some
            | Expression.Average r ->
                let result = Dice.Roll.mean r
                Value.Text(sprintf "%.2f" result), None
            | Expression.GetValue(id, property) ->
                match state.data |> Property.get id property with
                | Some v -> v, None
                | None -> Common.notImpl() // todo: implement lazy data entry
        let state =
            { state with
                view = { state.view with lastCommand = Some c; lastInput = Some input }
                }
        let log logChunk state = state |> log (c, logChunk)
        match c with
        | Quit -> return' (state |> Lens.over lview (fun s -> { s with finished = true }), [])
        | Log chunks ->
            let valuesAndExplanations = chunks |> List.map eval
            let logChunk: Log.Chunk =
                match valuesAndExplanations with
                | [Value.Text txt, None] ->
                    Leaf(txt) // collapse simple text into a leaf entry
                | _ ->
                    // otherwise, log the whole thing
                    let mainText = String.join emptyString (valuesAndExplanations |> List.map (fst >> Value.toString))
                    let explanations = valuesAndExplanations |> List.collect (snd >> List.ofOption)
                    Nested(mainText, Leaf input::explanations)
            return' (state |> log logChunk, [c, logChunk])
        | ShowLog(n, _detailLevel) ->
            let log = state.data.log |> Log.extractEntries
            let lines = log |> List.collect id
            return' (state, (match n with Some n when n < lines.Length -> lines.[(lines.Length - n)..] | _ -> lines))
        | SetOutputDetail level ->
            return' (state |> Lens.over lview (fun v -> { v with outputDetailLevel = level } ), state.view.lastOutput) // repeat last output with new detail level
        | SetLogDetail n ->
            return' (state |> Lens.over lview (fun v -> { v with logDetailLevel = n } ), state.view.lastOutput) // repeat last output with new detail level
        | AddCombatant name ->
            return' <|
                match state.data.roster |> Roster.add name with
                | Ok(roster) -> state |> Lens.set (ldata << lroster) roster, [c, Leaf (sprintf "Added %s" name)]
                | Error(e) -> state, [c, Leaf e]
        | Statement(Expression e) ->
            let result, explanation = eval e
            let logChunk = Nested(sprintf "%s: %s" input (match explanation with None -> result |> Value.toString | Some v -> v |> Log.getText), match explanation with Some (Nested(_, children)) -> children | Some(leaf) -> [leaf] | None -> []) // don't repeat details unnecessarily
            return' (state |> log logChunk, [c, logChunk])
        | Statement(SetValue(id, propertyName, expr)) ->
            let result, explanation = eval expr
            let name = Roster.tryId id state.data.roster |> Option.get
            let logChunk = Nested(sprintf "%s's new %s: %s" name propertyName (Value.toString result), explanation |> List.ofOption)
            let state = state |> Lens.over ldata (Property.set id propertyName result)
            return' ((state |> log logChunk), [c, logChunk])
        | Statement(AddToValue(id, propertyName, expr)) ->
            let currentValue = state.data |> Property.get id propertyName |> Option.defaultValue (Value.Number 0)
            let result, explanation = eval expr
            let newValue =
                match currentValue, result with
                | Value.Number lhs, Value.Number rhs -> Value.Number(lhs + rhs)
                | _ -> Common.notImpl() // todo: implement strong typing or something
            let name = Roster.tryId id state.data.roster |> Option.get
            let logChunk = Nested(sprintf "%s's %s changed from %s to %s" name propertyName (Value.toString currentValue) (Value.toString newValue), explanation |> List.ofOption)
            let state = state |> Lens.over ldata (Property.set id propertyName newValue)
            return' ((state |> log logChunk), [c, logChunk])
        | Save label ->
            storage.Save label state.data (function Ok _ -> return' (state, [c, sprintf "Saved '%s'" label |> Leaf]) | Error err -> return' (state, [c, sprintf "Could not save '%s': '%s'" label err |> Leaf]))
        | Load label ->
            storage.Load label (function Ok data -> exec { state with data = data; view = emptyView } (ShowLog (None, None)) return' | Error err -> return' (state, [c, sprintf "Could not load '%s': '%s'" label err |> Leaf]))
        | Clear ->
            return' (Model.Functions.Battle2.init(), [])
    match Packrat.ParseArgs.Init(input, state.data.roster) with
    | Parse.Cmd(cmd, End) ->
        let callback (state: State, output: Log.Entry<_> list) =
            return' { state with
                        view = { state.view with lastOutput = output }
                        }
        exec state cmd callback
    | _ ->
        // invalid command (probably pure whitespace)
        return'
            { state with
                view = { state.view with lastInput = Some input; lastOutput = [] }
                }
