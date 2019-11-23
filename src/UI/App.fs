module App
open UI.Ribbit
open Elmish

open Elmish.React
//open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
importAll "../../sass/main.sass"

module Url =
    open Model.Types
    open UI.Types
    type ParseResult = Campaign of (GameState * ViewModel) | Battle | Neither
    module Parse =
        open Model.Operations
        open Browser.Types
        open Packrat
        let locationParser (rootActivePattern: ParseInput -> (_ * ParseInput) option) (loc: Location) =
            let (|Root|_|) = rootActivePattern
            match ParseArgs.Init loc.hash with
            | Str "#" (Root(v, End)) -> v
            | _ -> Neither

        let (|Page|_|) = function
            | Str "battleDebug" ctx ->
                let pcs = [
                    CharSheet.create "Max the Mighty" Male (18,12,14,15,9,11) false None Model.Chargen.Templates.charTemplates.["Brute"]
                        |> CharInfo.ofCharSheet
                    ]
                Some(Campaign ({ GameState.empty with pcs = pcs } |> Model.Gameplay.startBattle |> snd, ViewModel.Battle), ctx)
            | Str "battle" ctx ->
                Some(Campaign ({ GameState.empty with battle = Some (DataEngine.init())}, ViewModel.Battle), ctx)
            | Str "ribbit" ctx ->
                Some(Battle, ctx)
            | Str "mapGen" ctx ->
                Some(Campaign ({ GameState.empty with mapGen = Some (Model.MapGen.init 10 10) }, ViewModel.MapGen), ctx)
            | Str "campaignDebug" ctx ->
                let template = Model.Chargen.Templates.charTemplates.["Brute"]
                let pc = Model.Operations.CharSheet.create "Spartacus" Male (14, 16, 9, 13, 11, 13) false None template
                Some(Campaign ({ GameState.empty with pcs = [CharInfo.ofCharSheet pc] }, ViewModel.Campaign), ctx)
            | _ -> None

        let page = locationParser (|Page|_|)
    let parse = Parse.page

module App =
    open Fable.React
    type Model = {
        campaign: UI.Types.Model
        battle: UI.Ribbit.Model option
        }
    type Msg = Campaign of UI.Types.Msg | Battle of UI.Ribbit.Cmd
    let update msg model =
        match msg with
        | Campaign msg ->
            let m, cmd = UI.State.update msg model.campaign
            { model with campaign = m }, cmd
        | Battle msg when model.battle.IsSome ->
            let m, cmd = UI.Ribbit.update msg model.battle.Value
            { model with battle = Some m }, cmd
        | Battle _ -> model, Cmd.Empty
    let init = function
        | Url.ParseResult.Campaign args ->
            let c, cmds = UI.State.init (Some args)
            { campaign = c; battle = None }, cmds
        | Url.ParseResult.Battle ->
            let c, cmds = UI.State.init None
            let b, cmds2 = UI.Ribbit.init ()
            { campaign = c; battle = Some b }, cmds@cmds2
        | Url.ParseResult.Neither ->
            let c, cmds = UI.State.init None
            { campaign = c; battle = None }, cmds
    let view (model: Model) dispatch =
        match model with
        | { battle = Some b } -> UI.Ribbit.view b (Msg.Battle >> dispatch)
        | _ -> UI.View.root model.campaign (Msg.Campaign >> dispatch)
    let urlUpdate (parseResult: Url.ParseResult) (model: Model) =
        match parseResult with
        | Url.ParseResult.Campaign args ->
            let c, cmds = UI.State.init (Some args)
            { model with campaign = c }, cmds
        | Url.ParseResult.Battle ->
            let b, cmds = UI.Ribbit.init ()
            { model with battle = Some b }, cmds
        | Url.ParseResult.Neither -> model, Cmd.Empty

open App
Program.mkProgram init update view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun d ->
    Browser.Dom.window.onerror <-
    fun msg _src _lineNo _colNo err ->
        if msg.ToString().Contains "SocketProtocolError" = false then
            d (App.Msg.Battle <| UI.Ribbit.Error (sprintf "Error: %A" msg))
        ))
#if DEBUG
|> Program.toNavigable Url.parse App.urlUpdate
//|> Program.withDebugger
#endif
|> Program.withReactBatched "elmish-app"
|> Program.run
