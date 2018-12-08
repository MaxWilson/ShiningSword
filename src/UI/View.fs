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

let root model dispatch =
    let progress (Operation(_:Model.Types.Query, answer)) v =
        match answer v with
        | Final v -> dispatch CloseModal
        | Intermediate(q, answer) -> dispatch (UpdateModal (Operation(q, answer)))
    let contents =
        match model with
        | { modalDialogs = Operation(v,_) as op::_ } ->
            div [] [
                button [OnClick (fun _ -> progress op "yes")] [str (sprintf "%A" v)]
                button [OnClick (fun _ -> progress op "no")] [str "Done"]
                ]
        | _ ->
            let modalOperation onEnd e =
                e |> Eventual.toOperation (dispatch << NewModal) (fun v -> onEnd v; dispatch CloseModal)
            let startGame _ = game 0 |> modalOperation (fun x -> Browser.window.alert(sprintf "Result: %A" x))
            button [OnClick startGame] [str "Start new game"]
    div [] [contents]
        

// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
