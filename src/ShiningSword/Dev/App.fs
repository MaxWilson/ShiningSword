module Dev.App

open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open UI.Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core

importSideEffects "../sass/main.sass"

module App =

    type Model = { counter: int }
    type Msg =
        | Increment

    let init initialCmd =
        { counter = 0 }

    let update msg model =
        match msg with
        | Increment -> { model with counter = model.counter + 1 }

    open Feliz.Router
    let view (model: Model) dispatch =
        Html.div [
            Html.button [prop.text "+"; prop.onClick (fun _ -> dispatch Increment)]
            Html.text (model.counter.ToString())
            Html.div [
                Html.button [prop.text "Panic"; prop.onClick (fun _ -> raise (System.Exception $"10/0={10/0}"))]
                ]
            ]

open App
open Elmish
open Elmish.Navigation

Program.mkSimple init update view
|> Program.withReactBatched "feliz-app"
|> Program.run
