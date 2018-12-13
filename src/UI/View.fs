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
open Elmish.HMR
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

let confirmQuery txt answer =
    div [] [
        str txt
        Button.button [Button.OnClick (answer "yes")] [str "Yes"]
        Button.button [Button.OnClick (answer "no")] [str "No"]
        ]

module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow =  40.

let onKeyDown keyCode action =
      OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
          if ev.keyCode = keyCode then
              ev.preventDefault()
              action ev)

let freeTextQuery prompt state updateState answer =
    div [] [
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
    div [] [
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
    div [] [
        yield str prompt
        for choice in choices do
            yield Button.button [Button.OnClick <| answer choice; Button.Color Fulma.Color.IsBlack] [str choice]
        ]

let alertQuery prompt answer =
    div [] [
        yield str prompt
        yield br[]
        yield Button.button [Button.OnClick <| answer "OK" ; Button.Props [AutoFocus true]] [str "OK"]
        ]

let root model dispatch =
    let contents =
        match model with
        | { modalDialogs = (Operation(q,_) as op, vm)::_ } ->
            let inline answer v _ = progress dispatch op v
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
        | _ ->
            let startGame _ = Model.Gameplay.game() |> modalOperation dispatch "" ignore

            div [] [
                yield Button.button [Button.OnClick startGame; Button.Color Fulma.Color.IsBlack] [str "Start new game"]
                ]
    div [] [contents]


// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Elmish.HMR.Program.run
