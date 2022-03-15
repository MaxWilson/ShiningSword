module App

open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open UI.Components
open Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
importSideEffects "../sass/main.sass"

module App =
    type Model = { size: int; error: string option }
    type Msg =
        | Error of msg: string
        | Transform of (Model -> Model)
    let update msg model =
        match msg with
        | Error msg -> { model with error = Some msg }
        | Transform f -> { f model with error = None }
    let init _ =
        { size = 1; error = None }
    let view (model: Model) dispatch =
        let window = Browser.Dom.window;
        Html.div [
            Html.div [
                prop.children [
                    Html.text (match model.error with Some msg -> msg | None -> "Welcome to Shining Sword")
                    ]
                prop.style [style.marginBottom 10]
                ]

            stage [
                "width" ==> window.innerWidth - 100.
                "height" ==> window.innerHeight - 50.
                "children" ==>
                    layer [
                        "children" ==> [
                            circle [
                                Shape.key 2
                                Circle.radius 100.
                                Circle.fill Green
                                Circle.x 100
                                Circle.y 100
                                ]
                        ]
                    ]
                ]
            ]

open App
Program.mkSimple init update view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun dispatch ->
    Browser.Dom.window.onerror <-
    fun msg ->
        if msg.ToString().Contains "SocketProtocolError" = false then
            dispatch (sprintf "Error: %A" msg |> Error)
            Browser.Dom.window.alert ("Unhandled Exception: " + msg.ToString())
        ))
|> Program.withReactBatched "feliz-app"
|> Program.run
