module Dev.App

open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Packrat
open UI.Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core

importSideEffects "../sass/main.sass"

module DataTypes =
    type Name = Name of string
    type XP = XP of int
    type HP = HP of int
open DataTypes

module Bestiary =
    type Definition = {
        xp: XP option
        hp: HP option
        }
        with static member fresh = { xp = None; hp = None }
    type d = Map<Name, Definition>
    let fresh = Map.empty
    let define name (bestiary: d) =
        bestiary |> Map.add name Definition.fresh
    let declareXP name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with xp = Some value })
    let declareHP name value (bestiary: d) =
        bestiary |> Map.change name (fun e -> Some { (match e with Some d -> d | None -> Definition.fresh) with hp = Some value })

module Game =
    type Action = Action of string
    type WoundLog = { victims: Map<Name, int>; woundedBy: Map<Name, int> }
        with static member fresh = { victims = Map.empty; woundedBy = Map.empty }
    type Creature = { name: Name; templateType: Name option; actionDeclaration: Action option; xpEarned: int; HP: int; woundLog: WoundLog }
        with static member fresh name templateType = { name = name; templateType = templateType; actionDeclaration = None; xpEarned = 0; HP = 0; woundLog = WoundLog.fresh }
    type Command =
        | Define of Name
        | DeclareXP of Name * XP
        | DeclareHP of Name * HP
        | Add of Name
        | InflictDamage of src:Name * target:Name * hp:HP
        | ClearDeadCreatures

    type d = {
        roster: Name list
        stats: Map<Name, Creature>
        bestiary: Bestiary.d
        }
    let fresh = { roster = []; stats = Map.empty; bestiary = Bestiary.fresh }
    let update msg model =
        match msg with
        | Define name ->
            { model with bestiary = model.bestiary |> Bestiary.define name }
        | DeclareXP (name, (XP v as xp)) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with xpEarned = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareXP name xp }
        | DeclareHP (name, (HP v as hp)) ->
            match model.stats |> Map.tryFind name with
            | Some creature ->
                { model with stats = model.stats |> Map.add name { creature with HP = v }}
            | None ->
                { model with bestiary = model.bestiary |> Bestiary.declareHP name hp }
        | Add (Name nameStr as name) ->
            match model.bestiary |> Map.tryFind name with
            | None ->
                { model with
                    roster = model.roster@[name]
                    stats = model.stats |> Map.add name (Creature.fresh name None) }
            | Some def ->
                match def.hp with
                | None ->
                    failwith $"{nameStr} must declare HP first"
                | Some (HP hp) ->
                    let individualName = Seq.unfold (fun i -> Some(Name $"{nameStr} #{i}", i+1)) 1 |> Seq.find (fun name' -> model.stats.ContainsKey name' |> not)
                    { model with
                        roster = model.roster@[individualName]
                        stats = model.stats |> Map.add individualName { Creature.fresh individualName (Some name) with HP = hp } }
        | InflictDamage(src, target, HP hpLoss) ->
            match model.stats |> Map.tryFind src, model.stats |> Map.tryFind target with
            | Some src, Some target ->
                let hp = target.HP
                let hp' = hp - hpLoss
                let isKill = hp > 0 && hp' <= 0
                // we don't let monster damage go negative, but we let hero damage go negative just in case their HP were never recorded
                let hpLoss = if isKill then hp elif (hp <= 0 && target.templateType.IsSome) then 0 else hpLoss
                let recordInteraction (name: Name) amount = Map.change name (function None -> Some amount | Some v -> Some (v+amount))
                let target' = { target with HP = target.HP - hpLoss; woundLog = { target.woundLog with woundedBy = target.woundLog.woundedBy |> recordInteraction src.name hpLoss } }
                let src' = { src with woundLog = { src.woundLog with victims = src.woundLog.victims |> recordInteraction target.name hpLoss } }
                let model' = { model with stats = model.stats |> Map.add src.name src' |> Map.add target.name target' }
                if isKill = false then
                    model'
                else
                    let woundLog = target'.woundLog
                    let awards =
                        [
                            let denominator = (woundLog.victims.Values |> Seq.sum) + (woundLog.woundedBy.Values |> Seq.sum)
                            let xpTotal =
                                match target'.templateType with
                                | Some monsterKind ->
                                    match model.bestiary[monsterKind].xp with Some (XP xp) -> xp | None -> 0
                                | None -> 0
                            for name in (woundLog.victims.Keys |> Seq.append woundLog.woundedBy.Keys |> Seq.distinct) do
                                let numerator =
                                    (woundLog.victims |> Map.tryFind name |> Option.defaultValue 0)
                                        + (woundLog.woundedBy |> Map.tryFind name |> Option.defaultValue 0)
                                let award = (numerator * xpTotal)/denominator
                                name, award
                        ]
                    let allocateXP (roster: Map<Name, Creature>) = function
                        | name, award ->
                            roster |> Map.change name (function None -> None | Some c -> Some { c with xpEarned = c.xpEarned + award })
                    { model' with stats = awards |> List.fold allocateXP model'.stats }
            | None, _ ->
                failwith $"{src} does not exist!"
            | _, None ->
                failwith $"{target} does not exist!"
        | ClearDeadCreatures ->
            let roster' =
                model.roster
                |> List.filter (fun name ->
                    let creature = model.stats[name]
                    if creature.templateType.IsNone then
                        true // never clear PCs
                    else
                        creature.HP > 0 // clear dead monsters
                    )
            { model with roster = roster'; stats = model.stats |> Map.filter (fun name _ -> roster' |> List.contains name) }

    type FSX =
        // FSX-oriented script commands
        static member define name = update (Define (Name name))
        static member declareHP name hp = update (DeclareHP (Name name, HP hp))
        static member declareXP name xp = update (DeclareXP (Name name, XP xp))
        static member add name = update (Add (Name name))
        static member damage src target hp = update (InflictDamage (Name src, Name target, HP hp))
        static member getHP name (model:d) = model.stats[Name name].HP
        static member getXPEarned name (model:d) = model.stats[Name name].xpEarned

#nowarn "40" // we're not going anything crazy with recursion like calling a pass-in method as part of a ctor. Just regular pattenr-matching.

module UI =
    type d = { input: string; game: Game.d; errors: string list }
    let fresh = { input = ""; game = Game.fresh; errors = [] }
    let nameChars = alphanumeric + whitespace + Set ['#']
    let (|NewName|_|) = function
        | OWS(Chars nameChars (name, ctx)) -> Some(DataTypes.Name (name.Trim()), ctx)
        | _ -> None
    let rec (|NewNames|_|) = pack <| function
        | NewName(name, OWSStr "," (NewNames(rest, ctx))) -> Some(name::rest, ctx)
        | NewName(name, ctx) -> Some([name], ctx)
        | _ -> None
    let (|GameContext|_|) = ExternalContextOf<Game.d>
    let isPotentialNamePrefix (names: obj) (substring: string) =
        match names |> unbox<obj option> with
        | Some externalContext ->
            let game = externalContext |> unbox<Game.d>
            game.roster |> Seq.append game.bestiary.Keys |> Seq.exists(fun (DataTypes.Name name) -> name.StartsWith substring)
        | _ -> false
    let (|Name|_|) = function
        | OWS(GameContext(game) & (args, ix)) ->
            let substring = args.input.Substring(ix)
            let candidates = game.bestiary.Keys |> Seq.append game.roster |> Seq.distinct |> Seq.sortByDescending (fun (DataTypes.Name n) -> n.Length)
            // allow leaving off # sign
            match candidates |> Seq.tryFind(fun (DataTypes.Name name) -> substring.StartsWith name || substring.StartsWith (name.Replace("#", ""))) with
            | Some (DataTypes.Name n as name) ->
                let l = if substring.StartsWith (n.Replace("#", "")) then (n.Replace("#", "").Length) else n.Length
                Some(name, (args, ix+l))
            | None -> None
        | _ -> None
    let (|Declaration|_|) = function
        | OWSStr "xp" (Int (amt, ctx)) ->
            Some((fun name -> Game.DeclareXP(name, XP amt)), ctx)
        | OWSStr "hp" (Int (amt, ctx)) ->
            Some((fun name -> Game.DeclareHP(name, HP amt)), ctx)
        | _ -> None
    let rec (|Declarations|_|) = pack <| function
        | Declaration(f, OWSStr "," (Declarations(rest, ctx))) -> Some(f::rest, ctx)
        | Declaration(f, ctx) -> Some([f], ctx)
        | _ -> None
    let (|Command|_|) = function
        | Str "clear dead" ctx ->
            Some(Game.ClearDeadCreatures, ctx)
        | Str "add" (NewName(name, ctx)) ->
            Some(Game.Add(name), ctx)
        | Str "define" (NewName(name, ctx)) ->
            Some(Game.Define(name), ctx)
        | Name(name, Declaration (f, ctx)) ->
            Some(f name, ctx)
        | Name(src, (OWSStr "hits" (Name(target, OWSStr "for" (Int(amt, ctx)))))) ->
            Some(Game.InflictDamage(src, target, HP amt), ctx)
        | _ -> None
    let (|Commands|_|) = pack <| function
        | Str "add" (NewNames(names, ctx)) ->
            Some(names |> List.map Game.Add, ctx)
        | Str "define" (NewNames(names, ctx)) ->
            Some(names |> List.map Game.Define, ctx)
        | Name(name, Declarations (fs, ctx)) ->
            Some(fs |> List.map(fun f -> f name), ctx)
        | Command(cmd, ctx) -> Some([cmd], ctx)
        | _ -> None
    let executeIfPossible ui cmd =
        try
            { ui with input = ""; game = ui.game |> Game.update cmd; errors = [] }
        with err ->
            { ui with input = ""; errors = (err.ToString())::ui.errors }
    let executeInputIfPossible (ui: d) =
        match ParseArgs.Init(ui.input, ui.game) with
        | Commands (cmds, End) ->
            cmds |> List.fold executeIfPossible ui
        | Command (cmd, End) ->
            executeIfPossible ui cmd
        | _ -> ui
    let testbed() =
        let exec str game =
            match ParseArgs.Init(str, game) with
            | Commands(cmds, End) -> cmds |> List.fold (flip Game.update) game
        let mutable g = Game.fresh
        match ParseArgs.Init("Beholder hp 180, xp 10000", g) with
        | Commands(cmds, End) -> cmds
        iter &g (exec "define Beholder, Ogre")
        iter &g (exec "Beholder hp 180, xp 10000")
        iter &g (exec "define Giant")
        iter &g (exec "Giant hp 80")
        iter &g (exec "Giant xp 2900")
        iter &g (exec "add Giant")
        iter &g (exec "add Bob")
        iter &g (exec "Bob hp 50")
        iter &g (exec "Bob hits Giant 1 for 30")
        g.stats[Name "Giant #1"].HP
        iter &g (exec "Bob hits Giant 1 for 30")
        g.stats[Name "Giant #1"].HP
        iter &g (exec "Bob hits Giant 1 for 30")
        g.stats[Name "Giant #1"].HP
        iter &g (exec "clear dead")

module App =
    open Game
    open type Game.FSX

    type Model = UI.d
    type Msg =
        | ReviseInput of msg: string
        | SubmitInput
        | ExecuteCommand of Game.Command

    let init initialCmd = UI.fresh

    let update msg (model: Model) =
        match msg with
        | ReviseInput input -> { model with input = input }
        | SubmitInput -> model |> UI.executeInputIfPossible
        | ExecuteCommand cmd -> UI.executeIfPossible model cmd

    open Feliz.Router
    let view (model: Model) dispatch =
        let class' (className: string) ctor (children: ReactElement list) =
            ctor [prop.className className; prop.children children]
        class' "dev" Html.div [
            Html.table [
                Html.thead [
                    Html.tr [Html.th [prop.text "Name"]; Html.th [prop.text "Declaration"]; Html.th [prop.text "Initiative"]; Html.th [prop.text "Notes"]; Html.th [prop.text "XP earned"]; Html.th [prop.text "HP"]]
                    ]
                Html.tbody [
                    for name in model.game.roster do
                        Html.tr [
                            let creature = model.game.stats[name]
                            Html.td [prop.text (match name with Name name -> $"{name}")]
                            Html.td [prop.text "Declaration TODO"]
                            Html.td [prop.text "Initiative TODO"]
                            Html.td [prop.text "Notes TODO"]
                            Html.td [prop.text creature.xpEarned]
                            Html.td [prop.text creature.HP]
                            ]
                        ]
                    ]
            class' "inputPanel" Html.div [
                Html.input [
                    prop.autoFocus true
                    prop.valueOrDefault model.input;
                    prop.onKeyPress (fun e ->
                        if e.key = "Enter" then
                            e.preventDefault()
                            dispatch SubmitInput
                        );
                    prop.onChange (fun (e: string) ->
                        ReviseInput e |> dispatch)
                    ]
                Html.button [prop.text "OK"; prop.onClick (fun _ -> dispatch SubmitInput)]
                ]
            Html.div [
                for err in model.errors do
                    Html.div err
                ]
            class' "bestiary" Html.table [
                Html.thead [
                    Html.tr [Html.th [prop.text "Type"]; Html.th [prop.text "HP"]; Html.th [prop.text "XP reward"]]
                    ]
                Html.tbody [
                    for KeyValue(name, type1) in model.game.bestiary do
                        Html.tr [
                            Html.td [prop.text (match name with Name name -> name)]
                            Html.td [
                                Html.input [prop.valueOrDefault (match type1.hp with Some (HP v) -> v.ToString() | None -> ""); prop.onChange (fun (txt:string) -> match System.Int32.TryParse(txt) with true, hp -> dispatch (Game.DeclareHP(name, HP hp) |> ExecuteCommand))]
                                ]
                            Html.td [
                                Html.input [prop.valueOrDefault (match type1.xp with Some (XP v) -> v.ToString() | None -> ""); prop.onChange (fun (txt:string) -> match System.Int32.TryParse(txt) with true, xp -> dispatch (Game.DeclareXP(name, XP xp) |> ExecuteCommand))]
                                ]
                            ]
                        ]
                    ]
            ]

open App
open Elmish
open Elmish.Navigation

Program.mkSimple init update view
|> Program.withReactBatched "feliz-dev"
|> Program.run
