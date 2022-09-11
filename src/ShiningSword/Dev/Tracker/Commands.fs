module Dev.Tracker.Commands

open Packrat
open Domain.Random
open Domain.Ribbit.Commands

let helpText = Domain.Ribbit.Commands.helpText.Trim() + """
Beholder hp 180, xp 10000
add Bob, Lara, Harry
Harry hits Beholder #1 for 80
Beholder #1 hits Bob for 60, Harry for 30, Lara for 30
Lara: charmed by beholder
Harry:: angry at beholder
Harry:: furious at beholder
clear dead
Lara declares Kill beholder
roll init
next init
10d6
3d+1
d20-1
"""

#nowarn "40" // we're not going anything crazy with recursion like calling a pass-in method as part of a ctor. Just regular pattenr-matching.

open Dev.Tracker.Game
open Dev.Tracker.Game.Game
open Domain.Ribbit

let (|IntMod|_|) = pack <| function
    | OWS(Chars numericWithPlusOrMinus (v, OWS(rest))) ->
        match System.Int32.TryParse(v) with
        | true, v -> Some(v, rest)
        | _ -> None
    | _ -> None

let (|GameContext|_|) = ExternalContextOf<Game.d>
let isPotentialNamePrefix (names: obj) (substring: string) =
    match names |> unbox<obj option> with
    | Some externalContext ->
        let ribbit = externalContext |> unbox<Game.d>
        ribbit.data.roster |> Map.keys |> Seq.append (ribbit.data.kindsOfMonsters |> Map.keys) |> Seq.exists(fun name -> name.StartsWith substring)
    | _ -> false
let (|Name|_|) = pack <| function
    | OWS(GameContext(ribbit) & (args, ix)) ->
        let substring = args.input.Substring(ix)
        let startsWith s = substring.StartsWith(s, System.StringComparison.InvariantCultureIgnoreCase)
        let candidates = ribbit.data.roster |> Map.keys |> Seq.append (ribbit.data.kindsOfMonsters |> Map.keys) |> Seq.distinct |> Seq.sortByDescending (fun n -> n.Length)
        // allow leaving off # sign
        match candidates |> Seq.tryFind(startsWith) with
        | Some (name) ->
            let l = name.Length
            Some(name, (args, ix+l))
        | None -> None
    | _ -> None
// Individual Name = NOT a monsterkind. E.g. doesn't make sense for all dragons to hit all giants.
let (|IndividualName|_|) = pack <| function
    | OWS(GameContext(ribbit) & (args, ix)) ->
        let substring = args.input.Substring(ix)
        let startsWith s = substring.StartsWith(s, System.StringComparison.InvariantCultureIgnoreCase)
        let candidates = ribbit.data.roster |> Map.keys |> Seq.distinct |> Seq.sortByDescending (fun n -> n.Length)
        // allow leaving off # sign
        match candidates |> Seq.tryFind(startsWith) |> Option.orElseWith(fun () -> candidates |> Seq.tryFind(fun name -> startsWith (name.Replace("#", "")))) with
        | Some (name) ->
            let l = if substring.StartsWith (name.Replace("#", "")) then (name.Replace("#", "").Length) else name.Length
            Some(name, (args, ix+l))
        | None -> None
    | _ -> None
let (|IndividualNickName|_|) = function
    | OWS(GameContext(ribbit) & (args, ix)) ->
        let substring1 = args.input.Substring(ix)
        let startsWith1 s = substring1.StartsWith(s, System.StringComparison.InvariantCultureIgnoreCase)
        let candidates1 = ribbit.data.roster |> Map.keys |> Seq.distinct |> Seq.sortByDescending (fun n -> n.Length)
        // allow leaving off # sign
        match candidates1 |> Seq.tryFind(startsWith1) |> Option.orElseWith(fun () -> candidates1 |> Seq.tryFind(fun name -> startsWith1 (name.Replace("#", "")))) with
        | Some (name) ->
            let l = if substring1.StartsWith (name.Replace("#", "")) then (name.Replace("#", "").Length) else name.Length
            Some(name, (args, ix+l))
        | None -> None
    | _ -> None
let rec (|IndividualNames|_|) = pack <| function
    | IndividualName(name, Str "," (IndividualNames(rest, ctx))) -> Some(name::rest, ctx)
    | IndividualName(name, ctx) -> Some([name], ctx)
    | _ -> None
let rec (|Names|_|) = pack <| function
    | Name(name, Str "," (Names(rest, ctx))) -> Some(name::rest, ctx)
    | Name(name, ctx) -> Some([name], ctx)
    | _ -> None
let declare (prop: Property<_, Ribbit>) value =
    (fun name -> Game.RibbitCommand (Domain.Ribbit.Commands.Command.Declare(name, fun id -> prop.Set(id, value))))
let (|Declaration|_|) = function
    | Int (amt, OWSStr "xp" (ctx)) -> Some(declare xpValueP amt, ctx)
    | Int (amt, OWSStr "hp" (ctx)) ->
        Some((fun name -> Game.DeclareRemainingHP(name, amt)), ctx)
    | Int (amt, OWSStr "maxhp" (ctx)) ->
        Some((fun name -> Game.DeclareMaxHP(name, amt)), ctx)
    | OWSStr "xp" (Int (amt, ctx)) -> Some(declare xpValueP amt, ctx)
    | OWSStr "hp" (Int (amt, ctx)) ->
        Some((fun name -> Game.DeclareRemainingHP(name, amt)), ctx)
    | OWSStr "maxhp" (Int (amt, ctx)) ->
        Some((fun name -> Game.DeclareMaxHP(name, amt)), ctx)
    | OWSStr "init" (IntMod (amt, ctx)) -> Some(declare initiativeModifierP amt, ctx)
    | OWSStr "initiative" (IntMod (amt, ctx)) -> Some(declare currentInitiativeP amt, ctx)
    | OWSStr "will" (OWS(Any (action, ctx))) ->
        Some((fun name -> Game.DeclareAction(name, action)), ctx)
    | _ -> None
let rec (|Declarations|_|) = pack <| function
    | Declaration(f, OWSStr "," (Declarations(rest, ctx))) -> Some(f::rest, ctx)
    | Declaration(f, ctx) -> Some([f], ctx)
    | _ -> None
let rec (|TakeDamage|_|) = pack <| function
    | (IndividualName(target, OWSStr "for" (Int(amt, ctx)))) ->
        Some((fun src -> Game.InflictDamage(src, target, amt)), ctx)
    | _ -> None
let rec (|TakeDamages|_|) = pack <| function
    | TakeDamage(f, OWSStr "," (TakeDamages(rest, ctx))) -> Some(f::rest, ctx)
    | TakeDamage(f, ctx) -> Some([f], ctx)
    | _ -> None
let rec (|Roll|_|) = pack <| function
    | Int(n, Str "d" (IntMod(bonus, ctx))) -> Some(RollSpec.create(n, 6, bonus), ctx)
    | Int(n, Str "d" ctx) -> Some(RollSpec.create(n, 6), ctx)
    | Str "d" (Int(d, IntMod(bonus, ctx))) -> Some(RollSpec.create(1, d, bonus), ctx)
    | Str "d" (Int(d, ctx)) -> Some(RollSpec.create(1, d), ctx)
    | Int(n, Str "d" (Int(d, IntMod(bonus, ctx)))) -> Some(RollSpec.create(n, d, bonus), ctx)
    | Int(n, Str "d" (Int(d, ctx))) -> Some(RollSpec.create(n, d), ctx)
    | _ -> None
let (|Command|_|) = function
    | Str "clear dead" ctx ->
        Some(Game.ClearDeadCreatures, ctx)
    | Name(name, Str "::" (OWS(AnyTrimmed (note, ctx)))) ->
        Some(Game.SetNotes(name, if note.Length > 0 then [note] else []), ctx)
    | Name(name, Str ":" (OWS(AnyTrimmed (note, ctx)))) ->
        Some(Game.AddNotes(name, [note]), ctx)
    | Name(name, Declaration (f, ctx)) ->
        Some(f name, ctx)
    | Name(src, (OWSStr "hits" (IndividualName(target, OWSStr "for" (Int(amt, ctx)))))) ->
        Some(Game.InflictDamage(src, target, amt), ctx)
    | _ -> None
let (|Commands|_|) = pack <| function
    | Domain.Ribbit.Commands.Commands(cmds, ctx) -> Some(cmds |> List.map Game.RibbitCommand, ctx)
    | Name(name, Declarations (fs, (End as ctx))) ->
        Some(fs |> List.map(fun f -> f name), ctx)
    | IndividualNickName(name, Declarations (fs, ctx)) ->
        Some(fs |> List.map(fun f -> f name), ctx)
    | IndividualName(name, OWSStr "hits" (TakeDamages (fs, ctx))) ->
        Some(fs |> List.map(fun f -> f name), ctx)
    | Command(cmd, ctx) -> Some([cmd], ctx)
    | _ -> None
// commands that have their own logging built in. Might be able to refactor this back into commands.
let (|LoggingCommands|_|) = pack <| function
    | Roll(r, ctx) -> Some([Eval r], ctx)
    | Char('/', AnyTrimmed(txt, ctx)) -> Some([Print txt], ctx)
    | _ -> None
#if INTERACTIVE
let testbed() =
    let exec str game =
        match ParseArgs.Init(str, game) with
        | Commands(cmds, End) -> cmds |> List.fold (flip Game.update) game
    let mutable g = Game.fresh
    match ParseArgs.Init("Beholder maxhp 180, xp 10000", g) with
    | Commands(cmds, End) -> cmds
    match ParseArgs.Init("Beholder 180 maxhp, xp 10000", g) with
    | Commands(cmds, End) -> cmds
    match ParseArgs.Init("Beholder 180 hp, xp 10000", g) with
    | Commands(cmds, End) -> cmds
    iter &g (exec "define Beholder, Ogre")
    iter &g (exec "Beholder hp 180, xp 10000")
    iter &g (exec "define Giant")
    iter &g (exec "Giant 80 hp")
    iter &g (exec "Giant hp 80")
    iter &g (exec "Giant xp 2900")
    iter &g (exec "add Giant")
    iter &g (exec "add Bob")
    iter &g (exec "Bob hp 50")
    iter &g (exec "Bob hits Giant 1 for 30")
    g.stats[DataTypes.Name "Giant #1"].HP
    iter &g (exec "Bob hits Giant 1 for 30")
    g.stats[DataTypes.Name "Giant #1"].HP
    iter &g (exec "Bob hits Giant 1 for 30")
    g.stats[DataTypes.Name "Giant #1"].HP
    iter &g (exec "clear dead")
#endif
