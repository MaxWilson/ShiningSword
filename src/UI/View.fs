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

let modalOperation dispatch state onSuccess e =
    e |> Eventual.toOperation (fun op -> dispatch (NewModal(op, state))) (fun () -> dispatch CloseModal) (fun v -> onSuccess v)

let progress dispatch (Operation(_:Model.Types.Query, provideAnswer)) answer =
    match provideAnswer answer with
    | Final _ -> ()
    | Intermediate(q, answer) -> dispatch (UpdateModalOperation (Operation(q, answer)))

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
    input [
        ClassName "input"
        Type "text"
        Placeholder prompt
        Value state
        AutoFocus true
        OnChange (fun ev -> !!ev.target?value |> updateState)
        onKeyDown KeyCode.enter (answer state)
        ]

let root model dispatch =
    let chooseName() =
        let rec e() : Eventual<_,_,_> = queryInteraction {
            let! name = Model.Operations.Query.text "Please enter your name"
            return name
            }
        Button.button [Button.OnClick (fun _ -> e() |> modalOperation dispatch "" (fun x -> dispatch (SetName x)))][str "Choose name"]
    let contents =
        match model with
        | { modalDialogs = (Operation(q,_) as op, vm)::_ } ->
            let inline answer v _ = progress dispatch op v
            match q with
            | Model.Types.Query.Confirm(q) -> confirmQuery q answer
            | Model.Types.Query.Freetext(q) ->
                freeTextQuery q vm (dispatch << UpdateModalViewModel) answer
        | _ ->
            let rec game i : Interaction.Eventual<_,_,_> = queryInteraction {
                let! keepGoing = Model.Operations.Query.confirm (sprintf "You've played %d rounds%s. Want to keep going?" i (match model.name with Some name -> (", " + name) | None -> ""))
                if keepGoing then
                    let! rest = game (1+i)
                    return rest
                else
                    return i
                }

            let startGame _ = game 0 |> modalOperation dispatch "" (fun x -> dispatch (SetGameLength x))
            div [] [
                match model.gameLength with
                | Some n -> yield (str (sprintf "Last time, %s played %d rounds" (defaultArg model.name "you") n))
                | _ -> ()
                yield Button.button [Button.OnClick startGame; Button.Color Fulma.Color.IsBlack] [str "Start new game"]
                ]
    div [] [contents;chooseName()]


// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withReact "elmish-app"
|> Elmish.HMR.Program.run
