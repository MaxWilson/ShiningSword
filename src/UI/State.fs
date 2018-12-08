module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open Model.Types

let urlUpdate (parseResult: Msg option) model =
    model, []

let init parseResult =
    { modalDialogs = [] } |> urlUpdate parseResult

let queryInteraction = Interaction.InteractionBuilder<Query, string>()
let rec game i : Interaction.Eventual<_,_,_> = queryInteraction {
    let! keepGoing = Model.Operations.Query.confirm "Want to keep going?"
    if keepGoing then
        let! rest = game (1+i)
        return rest
    else
        return (sprintf "You played %d rounds" i)
    }

let update msg model =
    match msg with
    | NewModal op ->
        { model with modalDialogs = op :: model.modalDialogs }, Cmd.Empty
    | UpdateModal op ->
        { model with modalDialogs = op :: model.modalDialogs.Tail }, Cmd.Empty
    | CloseModal ->
        let pop = function [] -> [] | _::t -> t
        { model with modalDialogs = model.modalDialogs |> pop }, Cmd.Empty

let oldUrlUpdate (result: Option<OldPage>) model =
  let toHash page =
    match page with
    | About -> "#about"
    | Counter -> "#counter"
    | Home -> "#home"
  match result with
  | None ->
    console.error("Error parsing url")
    model,Navigation.modifyUrl (toHash model.currentPage)
  | Some page ->
      { model with currentPage = page }, []

let oldInit result =
  let (counter, counterCmd) = Counter.State.init()
  let (home, homeCmd) = Home.State.init()
  let (model, cmd) =
    oldUrlUpdate result
      { currentPage = Home
        counter = counter
        home = home }
  model, Cmd.batch [ cmd
                     Cmd.map CounterMsg counterCmd
                     Cmd.map HomeMsg homeCmd ]

let oldUpdate msg model =
  match msg with
  | CounterMsg msg ->
      let (counter, counterCmd) = Counter.State.update msg model.counter
      { model with counter = counter }, Cmd.map CounterMsg counterCmd
  | HomeMsg msg ->
      let (home, homeCmd) = Home.State.update msg model.home
      { model with home = home }, Cmd.map HomeMsg homeCmd
