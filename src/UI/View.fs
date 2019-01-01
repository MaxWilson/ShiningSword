module App.View

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Core
open Fable.Core.JsInterop
open Fable.Import
open Fable.Import.Browser
open Types
open App.State

importAll "../../sass/main.sass"

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Elmish.React
open Elmish.Debug
#if DEBUG
open Elmish.HMR
#endif
open Interaction
open Common
open Fulma
open Fulma.Color
open Model.Types
open Model.Operations

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
            Some(GameState.empty, ctx)
        | _ -> None

    let page = locationParser (|Page|_|)

let modalOperation dispatch viewModel onSuccess e =
    e |> Eventual.toOperation (fun (q, gameState) continue' -> dispatch (NewModal(Operation(q, continue'), gameState, viewModel))) (fun () -> dispatch CloseModal) (fun v -> onSuccess v)

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
        onKeyDown KeyCode.enter (fun _ -> answer !cell ())
        ]

let confirmQuery txt answer =
    [
        str txt
        Button.button [Button.OnClick (answer "yes")] [str "Yes"]
        Button.button [Button.OnClick (answer "no")] [str "No"]
        ]

let freeTextQuery prompt state updateState answer =
    [
        str prompt
        input [
            ClassName "input"
            Type "text"
            Value state
            AutoFocus true
            OnChange (fun ev -> !!ev.target?value |> updateState)
            onKeyDown KeyCode.enter (answer state)
            ]
        ]

let numberQuery prompt state updateState answer =
    [
        str prompt
        input [
            ClassName "input"
            Type "number"
            Value state
            AutoFocus true
            OnChange (fun ev -> !!ev.target?value |> updateState)
            onKeyDown KeyCode.enter (answer state)
            ]
        ]

let selectQuery prompt choices answer =
    [
        yield str prompt
        yield br[]
        for choice in choices do
            yield Button.button [Button.OnClick <| answer choice; Button.Color Fulma.Color.IsBlack] [str choice]
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
                    |> modalOperation dispatch "" ignore
                [
                    yield line <| "The party consists of " + (game.pcs |> List.map (fun pc -> pc.src.name) |> oxfordJoin)
                    yield line <| sprintf "You have %d gold" game.gp
                    yield table [ClassName "table"] [
                        thead [] [
                            tr [] [
                                th [] [str "Pos"]
                                th [] [str "Name"]
                                th [] [str "Status"]
                                th [] [str "Level"]
                                th [] [str "Current HP"]
                                th [] [str "Max HP"]
                                th [] [str "XP"]
                                ]
                            ]
                        tbody [] (game.pcs |> List.mapi (fun i pc ->
                                let details =
                                    if pc.hp > 0 then [
                                        str (i.ToString())
                                        Button.button [Button.OnClick (showStatus pc); Button.IsLink] [str pc.src.name]
                                        str (sprintf " HP %d XP %d" pc.hp pc.src.xp)
                                        ]
                                    else
                                        [
                                            str "(Dead) "
                                            Button.button [Button.OnClick (showStatus pc); Button.IsLink] [str pc.src.name]
                                            str (sprintf " XP %d" pc.src.xp)
                                            ]
                                tr [OnClick (showStatus pc)] ([
                                    str <| (i+1).ToString()
                                    str pc.src.name
                                    str <| if pc.hp <= 0 then "(Dead)" elif pc.hp < (CharSheet.computeMaxHP pc.src) then "Wounded" else "OK"
                                    str (pc.src.classLevels.Length.ToString())
                                    str (pc.hp.ToString())
                                    str ((CharSheet.computeMaxHP pc.src).ToString())
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

let root model dispatch =
    undoModal <- thunk1 dispatch UndoModal
    let inline answer v _ = match model with { modalDialogs = (op, vm)::_ } -> progress dispatch op v | _ -> ()
    let ongoingInteraction =
        match model with
        | { modalDialogs = (Operation(q,_) as op, vm)::_ } ->
            match q with
            | Query.Alert _ ->
                onKeypress <- Some(fun ev ->
                    if ev.keyCode = KeyCode.enter then answer "" (); true
                    else false)
            | Query.Select(_, choices) ->
                onKeypress <- Some(fun ev ->
                    match System.Int32.TryParse ev.key with
                    // use one-based choosing for UI purposes: 1 is the first choice, 2 is the second
                    | true, n when n-1 < choices.Length ->
                        answer (choices.[n-1])()
                        true
                    | _ -> false)
            | _ -> onKeypress <- None
            div [ClassName "queryDialog"] <|
                match q with
                | Query.Confirm(q) -> confirmQuery q answer
                | Query.Freetext(q) ->
                    freeTextQuery q vm (dispatch << UpdateModalViewModel) answer
                | Query.Number(q) ->
                    numberQuery q vm (dispatch << UpdateModalViewModel) answer
                | Query.Select(prompt, choices) ->
                    selectQuery prompt choices answer
                | Query.Alert txt ->
                    alertQuery txt answer
                | Query.BattleQuery ->
                    let cmdEntry = textbox "Enter a text command" answer
                    [cmdEntry]
                | Query.Character pc ->
                    let ok = Button.button [Button.OnClick <| answer "OK" ; Button.Props [AutoFocus true]] [str "OK"]
                    [
                        p[][str pc.src.name]
                        p[][str (sprintf "%A %A [%s]%s" pc.src.sex pc.src.template.Value.name (Model.Operations.CharSheet.summarize pc.src.classLevels) (match pc.src.homeRegion with Some v -> " from " + v | _ -> ""))]
                        p[][str (sprintf "Str: %d Dex: %d Con: %d Int: %d Wis: %d Cha: %d HP: %d" pc.src.str pc.src.dex pc.src.con pc.src.int pc.src.wis pc.src.cha pc.hp)]
                        div [](pc.src.description.Split('\n') |> Array.map (fun line -> p [][str line]))
                        br[]
                        ok
                        ]
        | _ ->
            let startGame _ = Model.Gameplay.campaignMode() |> modalOperation dispatch "" ignore
            let startBattles _ = Browser.window.alert "Sorry, not implemented yet"
            let loadCampaign _ = Browser.window.alert "Sorry, not implemented yet"
            let saveCampaign _ = Browser.window.alert "Sorry, not implemented yet"
            Hero.hero [] [
                h1 [ClassName "is-size-3"; Style [TextAlign "center"]] [str "Shining Sword: Citadel of the Hundred Gates"]
                ul [ClassName "menu"; Style [TextAlign "center"]] ([
                    Button.button [Button.OnClick startGame; Button.Color Fulma.Color.IsBlack] [str "Start new campaign"]
                    Button.button [Button.OnClick loadCampaign; Button.Color Fulma.Color.IsBlack] [str "Load campaign"]
                    Button.button [Button.OnClick saveCampaign; Button.Color Fulma.Color.IsBlack] [str "Save campaign"]
                    Button.button [Button.OnClick startBattles; Button.Color Fulma.Color.IsBlack] [str "Run standalone battles"]
                    ] |> List.map (fun x -> li [ClassName "menu-list"] [x]))
                ]
    div [ClassName "mainPage"] [ongoingInteraction; partySummary (model.game, (match model.modalDialogs with (Operation(Query.Character _, _),_)::_ -> true | _ -> false)) dispatch; logOutput (model.game.log, model.logSkip) dispatch]


// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
