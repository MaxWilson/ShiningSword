module UI.State

open Elmish
open Elmish.Browser.Navigation
open Types
open Model.Types
open Model.Operations
open Common
open Fable.Import.Browser
open UI.View

open Elmish.React
open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif

let urlUpdate (parseResult: (GameState * ViewModel) option) model =
    match parseResult with
    | Some (state, vm) -> { model with game = state; mode = [vm]}, Cmd.Empty
    | None -> model, Cmd.Empty

let init parseResult =
    { modalDialogs = []; game = GameState.empty; undo = None; logSkip = None; mode = []; busy = NotBusy } |> urlUpdate parseResult

let update msg model =
    match msg with
    | Busy msg -> { model with busy = msg }, Cmd.Empty
    | NewMode(vm) -> { model with mode = vm::model.mode }, Cmd.Empty
    | EndMode(vm) ->
        match model.mode with
        | vm'::rest when vm = vm' -> { model with mode = rest }, Cmd.Empty
        | _ -> { model with mode = (Error (sprintf "Unable to pop viewModel: expected %A, found %A" vm model.mode))::model.mode }, Cmd.Empty
    | NewModal(op,gameState) ->
        { model with
            modalDialogs = op :: model.modalDialogs
            game = gameState
            undo = Some(model.game, model.modalDialogs)
            logSkip = None }, Cmd.Empty
    | UpdateModalOperation(op, gameState) ->
        let m =
            match model.modalDialogs with
            | _::rest -> op::rest
            | _ -> []
        { model with
            modalDialogs = m
            game = gameState
            undo = Some(model.game, model.modalDialogs)
            logSkip = None }, Cmd.Empty
    | UpdateCurrentViewModel vm ->
        let vm =
            match model.mode with
            | _::rest -> vm::rest
            | _ -> []
        { model with mode = vm }, Cmd.Empty
    | CloseModal ->
        let pop = function [] -> [] | _::t -> t
        { model with modalDialogs = model.modalDialogs |> pop }, Cmd.Empty
    | UndoModal ->
        // support up to one level of undo
        match model.undo with
        | Some(game, stack) ->
            { model with undo = None; game = game; modalDialogs = stack }, Cmd.Empty
        | None -> model, Cmd.Empty
    | LogSkip n -> { model with logSkip = Some n }, Cmd.Empty
    | UpdateGameState state -> { model with game = state }, Cmd.Empty
    | BattleUpdate msg -> Battle.update model msg, Cmd.Empty
    | Battle1Update (Battle1.Msg.Ongoing state) -> { model with game = state }, Cmd.Empty
    | Battle1Update (Battle1.Msg.Finish state) -> { model with game = state; mode = (match model.mode with _::rest -> rest | _ -> []) }, Cmd.Empty
    | MapGen msg -> { model with game = { model.game with mapGen = model.game.mapGen |> Core.Option.map (flip MapGen.update msg) } }, Cmd.Empty


module Parse =
    open Packrat
    open Global
    let locationParser (rootActivePattern: ParseInput -> ('result * ParseInput) option) (loc: Location) =
        let (|Root|_|) = rootActivePattern
        match ParseArgs.Init loc.hash with
        | Str "#" (Root(v, End)) -> Some v
        | _ -> None

    let (|Page|_|) = function
        | Str "battleDebug" ctx ->
            let pcs = [
                CharSheet.create "Max the Mighty" Male (18,12,14,15,9,11) false None Model.Chargen.Templates.charTemplates.["Brute"]
                    |> CharInfo.ofCharSheet
                ]
            Some(({ GameState.empty with pcs = pcs } |> Model.Gameplay.startBattle |> snd, ViewModel.Battle), ctx)
        | Str "battle" ctx ->
            Some(({ GameState.empty with battle = Some (DataEngine.init()) }, ViewModel.Battle), ctx)
        | Str "mapGen" ctx ->
            Some(({ GameState.empty with mapGen = Some (Model.MapGen.init 10 10) }, ViewModel.MapGen), ctx)
        | Str "campaignDebug" ctx ->
            let template = Model.Chargen.Templates.charTemplates.["Brute"]
            let pc = Model.Operations.CharSheet.create "Spartacus" Male (14, 16, 9, 13, 11, 13) false None template
            Some(({ GameState.empty with pcs = [CharInfo.ofCharSheet pc] }, ViewModel.Campaign), ctx)
        | _ -> None

    let page = locationParser (|Page|_|)
// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
window.onerror <- fun msg _src _lineNo _colNo err -> window.alert (sprintf "Bug alert! Unhandled exception. Email Max and tell him what happened. Error details: %A %A" msg err)

// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
