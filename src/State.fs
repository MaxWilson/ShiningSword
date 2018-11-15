module App.State

open Elmish
open Elmish.Browser.Navigation
open Elmish.Browser.UrlParser
open Fable.Import.Browser
open Global
open Types
open Wilson.Packrat



let pageParser (rootActivePattern: ParseInput -> ('result * ParseInput) option) (loc: Location) =
    let (|Root|_|) = rootActivePattern
    match ParseArgs.Init loc.hash with
    | Str "#" (Root(v, End)) -> Some v
    | _ -> None

let (|Page|_|) = function
    | Word(AnyCase "about", rest) -> Some (About, rest)
    | Word(AnyCase "counter", rest) -> Some (Counter, rest)
    | Word(AnyCase "home", rest) -> Some (Home, rest)
    | _ -> None

let urlUpdate (result: Option<Page>) model =
  match result with
  | None ->
    console.error("Error parsing url")
    model,Navigation.modifyUrl (toHash model.currentPage)
  | Some page ->
      { model with currentPage = page }, []

let init result =
  let (counter, counterCmd) = Counter.State.init()
  let (home, homeCmd) = Home.State.init()
  let (model, cmd) =
    urlUpdate result
      { currentPage = Home
        counter = counter
        home = home }
  model, Cmd.batch [ cmd
                     Cmd.map CounterMsg counterCmd
                     Cmd.map HomeMsg homeCmd ]

let update msg model =
  match msg with
  | CounterMsg msg ->
      let (counter, counterCmd) = Counter.State.update msg model.counter
      { model with counter = counter }, Cmd.map CounterMsg counterCmd
  | HomeMsg msg ->
      let (home, homeCmd) = Home.State.update msg model.home
      { model with home = home }, Cmd.map HomeMsg homeCmd
