module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types

let urlUpdate (parseResult: Msg option) model =
    model, []

let init parseResult =
    { stack = []; modalDialog = None } |> urlUpdate parseResult

let rec game i : Interaction.Eventual<_,_,_> = Interaction.InteractionBuilder<string, string>() {
    let! keepGoing = "Want to keep going?", Some
    if keepGoing = "yes" then
        let! rest = game (1+i)
        return rest
    else
        return (sprintf "You played %d rounds" i)
    }

let update msg model =
    match msg with
    | NewModal op ->
        { model with modalDialog = Some(op) }, Cmd.Empty
    | CloseModal ->
        { model with modalDialog = None }, Cmd.Empty

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
