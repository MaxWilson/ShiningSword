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

module Parse =
    open Packrat
    open Global
    let locationParser (rootActivePattern: ParseInput -> ('result * ParseInput) option) (loc: Location) =
        let (|Root|_|) = rootActivePattern
        match ParseArgs.Init loc.hash with
        | Str "#" (Root(v, End)) -> Some v
        | _ -> None

    let (|Page|_|) = function
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
    lazyView <| fun (game: GameState) ->
        let line msg = p [] [str msg]
        let children =
            if game.pcs.IsEmpty then []
            else
                [
                    yield line <| "The party consists of " + (game.pcs |> List.map (fun pc -> pc.src.name) |> oxfordJoin)
                    for pc in game.pcs do
                        if pc.hp > 0 then
                            yield line (sprintf "%s: HP %d XP %d" pc.src.name pc.hp pc.src.xp)
                        else
                            yield line (sprintf "(Dead) %s: XP %d" pc.src.name pc.src.xp)
                    yield line <| sprintf "You have %d gold" game.gp
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

            let inline logMove n = Button.OnClick (fun _ -> dispatch (LogSkip (logSkip + n)))
            div[ClassName "logDisplay"] [
                Button.button [logMove -logSkip][str "<<"]
                Button.button [logMove -1][str "<"]
                Button.button [logMove +1][str ">"]
                Button.button [logMove (logLength - 1 - logSkip)][str ">>"]
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
        | _ ->
            let startGame _ = Model.Gameplay.game() |> modalOperation dispatch "" ignore

            div [Style [TextAlign "center"]] [
                yield h1 [Style [TextAlign "center"]] [str "Shining Sword: Citadel of the Hundred Gates"]
                yield Button.button [Button.OnClick startGame; Button.Color Fulma.Color.IsBlack] [str "Start new game"]
                ]
    div [] [ongoingInteraction; partySummary model.game; logOutput (model.game.log, model.logSkip) dispatch]


// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Program.run
