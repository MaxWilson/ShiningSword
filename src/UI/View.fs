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
        //| Word(AnyCase "about", rest) -> Some ((), rest)
        //| Word(AnyCase "counter", rest) -> Some (Counter, rest)
        //| Word(AnyCase "home", rest) -> Some (Home, rest)
        | _ -> None

    let page = locationParser (|Page|_|)

let root model dispatch =
    let modalOperation e =
        e |> Eventual.toOperation (dispatch << NewModal) (thunk1 dispatch CloseModal)
    let contents =
        match model with
        | { modalDialog = Some(Operation(v, answer)) } ->
            div [] [
                button [OnClick (fun _ -> answer "yes" |> modalOperation)] [str v]
                button [OnClick (fun _ -> answer "no" |> modalOperation)] [str "Cancel"]
                ]
        | _ ->
            let startGame _ = game 0 |> modalOperation
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
