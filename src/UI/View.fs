module UI.View

open Common
open Interaction
open Model.Types
open Model.Functions
open Model.Operations
open UI.Global
open Types
open UI.State
open Elmish
open Elmish.Browser.Navigation
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser

importAll "../../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif
open Fulma
open Fulma.Color

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
            Some(({ GameState.empty with pcs = pcs } |> Model.Gameplay.startBattle |> snd, ViewModel.Battle) , ctx)
        | Str "campaignDebug" ctx ->
            let template = Model.Chargen.Templates.charTemplates.["Brute"]
            let pc = Model.Operations.CharSheet.create "Spartacus" Male (14, 16, 9, 13, 11, 13) false None template
            Some(({ GameState.empty with pcs = [CharInfo.ofCharSheet pc] }, ViewModel.Campaign) , ctx)
        | _ -> None

    let page = locationParser (|Page|_|)

let modalOperation dispatch onSuccess e =
    e |> Eventual.toOperation (fun (q, gameState) continue' -> dispatch (NewModal(Operation(q, continue'), gameState))) (fun () -> dispatch CloseModal) (fun v -> onSuccess v)

let progress dispatch (Operation(_:Model.Types.Query, provideAnswer)) answer =
    match provideAnswer answer with
    | Final _ -> ()
    | Intermediate((q,gameState), answer) -> dispatch (UpdateModalOperation (Operation(q, answer), gameState))

module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow =  40.
    let escape = 27.

let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev)

let mutable onKeypress : (KeyboardEvent -> bool) option = None
document.addEventListener_keydown(fun ev ->
    if onKeypress.IsSome && onKeypress.Value(ev) then
        ev.preventDefault()
    obj())
let mutable undoModal : unit -> unit = ignore
document.addEventListener_keyup((fun ev ->
    if ev.keyCode = KeyCode.escape then
        undoModal()
    obj()), true)

let textbox placeholder answer =
    let cell = ref ""
    input [
        ClassName "input"
        Type "number"
        Placeholder placeholder
        AutoFocus true
        OnChange (fun ev -> cell := !!ev.target?value)
        onKeyDown KeyCode.enter (fun _ -> answer !cell)
        ]

let confirmQuery txt answer =
    [
        str txt
        Button.button [Button.OnClick (answer "yes")] [str "Yes"]
        Button.button [Button.OnClick (answer "no")] [str "No"]
        ]

/// Helper method: an input which stores state locally in React.state and then calls onEnter when Enter is pressed
let statefulInput onEnter (props: IHTMLProp list) =
    Components.stateful "" (=) <| fun txt update ->
        let props : IHTMLProp list = [
                yield Value txt
                yield OnChange (fun ev ->
                    let v = !!ev.target?value
                    update (thunk v)
                    )
                yield onKeyDown KeyCode.enter (thunk1 onEnter txt)
                yield! props
                ]
        input props

let freeTextQuery prompt answer =
    [
        str prompt
        statefulInput answer [
            ClassName "input"
            Type "text"
            AutoFocus true
            ]
        ]

let numberQuery prompt answer =
    [   str prompt
        statefulInput answer [
            ClassName "input"
            Type "number"
            AutoFocus true
            ]
        ]

let buttonsWithHotkeys (labelsAndActions: (string * (_ -> unit)) list) =
    onKeypress <- Some(fun ev ->
        match System.Int32.TryParse ev.key with
        // use one-based choosing for UI purposes: 1 is the first choice, 2 is the second
        | true, n when n-1 < labelsAndActions.Length ->
            (snd (labelsAndActions |> List.item (n-1)))()
            true
        | _ ->
            match labelsAndActions |> List.tryFind (fun (label, action) -> label.StartsWith(ev.key, System.StringComparison.InvariantCultureIgnoreCase)) with
            | Some(_,action) ->
                action()
                true
            | _ -> false
        )
    labelsAndActions |> List.map (fun (label, action) ->
        Button.button [Button.OnClick (fun _ -> action()); Button.Color Fulma.Color.IsBlack] [str label])

let selectQuery prompt choices answer =
    [
        yield str prompt
        yield br[]
        yield! (choices |> List.map (fun choice -> choice, answer choice) |> buttonsWithHotkeys)
        ]

let alertQuery prompt answer =
    [
        yield str prompt
        yield br[]
        yield Button.button [Button.OnClick <| answer "OK" ; Button.Props [AutoFocus true]] [str "OK"]
        ]

let partySummary =
    lazyView2 <| fun ((game: GameState), isViewingChars: bool) dispatch ->
        let line msg = p [] [str msg]
        let children =
            if game.pcs.IsEmpty then []
            else
                let showStatus pc _ =
                    if isViewingChars then
                        dispatch CloseModal
                    Model.Gameplay.showPCDetails game pc
                    |> modalOperation dispatch ignore
                [
                    yield line <| "The party consists of " + (game.pcs |> List.map (fun pc -> pc.src.name) |> oxfordJoin)
                    yield line <| sprintf "You have %d gold" game.gp
                    yield table [ClassName "table"] [
                        thead [] [
                            tr [] [
                                th [] [str "Pos"]
                                th [] [str "Name"]
                                th [] [str "Type"]
                                th [] [str "Status"]
                                th [] [str "Level"]
                                th [] [str "Current HP"]
                                th [] [str "Max HP"]
                                th [] [str "XP"]
                                ]
                            ]
                        tbody [] (game.pcs |> List.mapi (fun i pc ->
                                tr [OnClick (showStatus pc)] ([
                                    str <| (i+1).ToString()
                                    str pc.src.name
                                    str <| (match pc.src.template with Some (v) -> v.name | None -> emptyString)
                                    str <| describeStatus (CharInfo.getCurrentHP pc) pc.hp
                                    str (pc.src.classLevels.Length.ToString())
                                    str ((CharInfo.getCurrentHP pc).ToString())
                                    str (pc.hp.ToString())
                                    str (pc.src.xp.ToString())
                                    ] |> List.map (fun v -> td [] [v]))
                            ))
                        ]
                    ]
        div [ClassName "partySummary"] children

let logOutput =
    lazyView2 <| fun (log: Log.Data, logSkip) dispatch ->
        if log = Log.empty then div[ClassName "logDisplay"][]
        else
            let logLength = (snd log).Length
            let logSkip = (defaultArg logSkip (logLength-1)) // default to most recent
            let logSkip = max (min (logLength - 1) logSkip) 0 // trim bounds just in case
            let log = Log.extract log
            let current = log |> List.skip logSkip |> List.head

            let inline logMove n label =
                Button.button
                    [Button.OnClick (fun _ -> dispatch (LogSkip (logSkip + n)));
                        Button.Disabled (logLength <= 1)]
                    [str label]
            div[ClassName "logDisplay"] [
                logMove -logSkip "<<"
                logMove -1 "<"
                logMove +1 ">"
                logMove (logLength - 1 - logSkip) ">>"
                div [ClassName "logDisplay"](current |> List.map (fun line -> p[][str line]))
                ]

let inline notImpl _ = Browser.window.alert "Sorry, not implemented yet. Send email to Max and tell him you want this."

let root model dispatch =
    undoModal <- thunk1 dispatch UndoModal
    let inline answer v = match model with { modalDialogs = op::_ } -> progress dispatch op v | _ -> ()
    let children =
        match model with
        | { modalDialogs = (Operation(q,_) as op)::_ } ->
            let ongoingInteraction =
                onKeypress <- None
                div [ClassName "queryDialog"] <|
                    match q with
                    | Query.Confirm(q) -> confirmQuery q (thunk1 answer)
                    | Query.Freetext(q) ->
                        freeTextQuery q answer
                    | Query.Number(q) ->
                        numberQuery q answer
                    | Query.Select(prompt, choices) ->
                        selectQuery prompt (choices |> List.ofArray) (thunk1 answer)
                    | Query.Alert txt ->
                        onKeypress <- Some(fun ev ->
                            if ev.keyCode = KeyCode.enter then answer ""; true
                            else false)
                        alertQuery txt (thunk1 answer)
                    | Query.Character pc ->
                        let ok = Button.button [Button.OnClick <| thunk1 answer "OK" ; Button.Props [AutoFocus true]] [str "OK"]
                        [
                            p[][str pc.src.name]
                            p[][str (sprintf "%A %A [%s]%s" pc.src.sex pc.src.template.Value.name (Model.Operations.CharSheet.summarize pc.src.classLevels) (match pc.src.homeRegion with Some v -> " from " + v | _ -> ""))]
                            p[][str (sprintf "Str: %d Dex: %d Con: %d Int: %d Wis: %d Cha: %d AC: %s HP: %d/%d" pc.src.str pc.src.dex pc.src.con pc.src.int pc.src.wis pc.src.cha (describeAC true (CharSheet.computeAC pc.src)) (CharInfo.getCurrentHP pc) pc.hp)]
                            div [](pc.src.description.Split('\n') |> Array.map (fun line -> p [][str line]))
                            br[]
                            ok
                            ]
            [ongoingInteraction; partySummary (model.game, (match model.modalDialogs with (Operation(Query.Character _, _))::_ -> true | _ -> false)) dispatch; logOutput (model.game.log, model.logSkip) dispatch]
        | { mode = Battle::_; game = { battle = Some battle } } ->
            Battle.view dispatch modalOperation buttonsWithHotkeys (logOutput (model.game.log, model.logSkip) dispatch) model.game battle
            | _ -> failwith "Should never be in Battle mode without a battle"
        | { mode = Campaign::_ } ->
            let state = model.game
            let msg = (sprintf "You have earned %d XP and %d gold pieces, and you've been adventuring for %s. What do you wish to do next?" state.pcs.[0].src.xp state.gp (Model.Gameplay.timeSummary state.timeElapsed))
            let latest =
                match model.game.log |> Log.extract |> List.tryLast with
                | Some entries ->
                    div [] [
                        for entry in entries do
                            yield (str entry)
                            yield br[]
                        ]
                | _ -> div[][]
            onKeypress <- Some(fun ev ->
                if ev.keyCode = KeyCode.enter then answer ""; true
                else false)
            let rest _ =
                model.game |> Model.Gameplay.rest |> UpdateGameState |> dispatch
            let advance _ =
                model.game |> Model.Gameplay.advance |> Model.Gameplay.enterTower
                |> modalOperation dispatch (fun state ->
                    dispatch (UpdateGameState state)
                    dispatch (NewMode Battle)
                    )
            let returnToTown _ =
                model.game |> Model.Gameplay.retire
                |> modalOperation dispatch (fun state ->
                    dispatch (UpdateGameState state)
                    dispatch (EndMode Campaign)
                    )
            [   div [ClassName "interaction"] [
                    yield latest
                    yield str msg
                    yield br[]
                    yield! buttonsWithHotkeys [
                        "Advance", advance
                        "Rest", rest
                        "Return to town", returnToTown
                        ]
                    ]
                partySummary (model.game, true) dispatch
                logOutput (model.game.log, model.logSkip) dispatch
                ]
            //| "Advance" -> return! doTower (advance state)
            //| "Rest" -> return! doRest state
            //| "Return to town" ->
            //    return! alert state (retirementMessage state)
            //| _ -> return state

        | { mode = [] } ->
            let startGame _ =
                Model.Gameplay.campaignMode() |> modalOperation dispatch (fun state -> dispatch (UpdateGameState state); dispatch (NewMode Campaign); dispatch (NewMode Battle))
            let startBattles = notImpl
            let loadCampaign = notImpl
            let saveCampaign = notImpl
            [Hero.hero [] [
                h1 [ClassName "is-size-3"; Style [TextAlign "center"]] [str "Shining Sword: Citadel of the Hundred Gates"]
                ul [ClassName "menu"; Style [TextAlign "center"]] ([
                    "Start new campaign", startGame
                    "Load campaign", loadCampaign
                    "Save campaign", saveCampaign
                    "Run standalone battles", startBattles
                    ]
                    |> buttonsWithHotkeys
                    |> List.map (fun x -> li [ClassName "menu-list"] [x]))
                ]]
        | { mode = Error msg::_ } ->
            [   str "Something went wrong:  Please file a bug report (email Max and describe what happened)."
                br []
                str <| "Details: " + msg]
    let gameHasStarted = List.exists (fun x -> x = Campaign || x = Battle) model.mode
    div [ClassName <| if gameHasStarted then "mainPage" else "startPage"] children

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
