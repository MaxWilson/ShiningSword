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
    let modalOperation onEnd e =
        e |> Eventual.toOperation (dispatch << NewModal) (fun () -> dispatch CloseModal) (fun v -> onEnd v)
    let progress (Operation(_:Model.Types.Query, answer)) v =
        match answer v with
        | Final v -> ()
        | Intermediate(q, answer) -> dispatch (UpdateModal (Operation(q, answer)))
    let names = [|"Vanya"; "Ryan"; "Ted"; "Matt"|]
    let chooseName() =
        let rec e() : Eventual<_,_,_> = queryInteraction {
            let name = names.[random.Next(names.Length)]
            let! like = Model.Operations.Query.confirm (sprintf "Do you like the name '%s'?" name)
            if like then
                return name
            else
                let! name = e()
                return name
            }
        button [OnClick (fun _ -> e() |> modalOperation (fun x -> dispatch (SetName x)))][str "Choose name"]
    let contents =
        match model with
        | { modalDialogs = Operation(q,_) as op::_ } ->
            match q with
            | Model.Types.Query.Confirm(txt) ->
                div [] [
                    str txt
                    button [OnClick (fun _ -> progress op "yes")] [str "Yes"]
                    button [OnClick (fun _ -> progress op "no")] [str "No"]
                    ]
            | Model.Types.Query.Freetext(txt) ->
                div [] [
                    str txt
                    ]
        | _ ->
            let startGame _ = game 0 |> modalOperation (fun x -> dispatch (SetGameLength x))
            div [] [
                match model.gameLength with
                | Some n -> yield (str (sprintf "Last time, %s played %d rounds" (defaultArg model.name "you") n))
                | _ -> ()
                yield button [OnClick startGame] [str "Start new game"]
                ]
    div [] [contents;chooseName()]
        

// App
Program.mkProgram init update root
|> Program.toNavigable Parse.page urlUpdate
#if DEBUG
|> Program.withDebugger
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
|> Program.run
