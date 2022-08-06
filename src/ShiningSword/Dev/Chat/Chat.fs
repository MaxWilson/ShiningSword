module Chat


open Elmish
open Elmish.React

open Fable.React

open Elmish.Bridge
open Model

let init () =
  {
    Mode = User
    Connection = Disconnected
    ConnectedUsers = []
    Messages = []
  }, Cmd.none
let update (msg : ClientMsg) (model : Model)  =
  match msg with
  | ToggleMode ->
    {model with Mode = if model.Mode = Message then User else Message}, Cmd.none
  | SendUser(name, color)->
    match name with
    |"" -> model, Cmd.none
    |_ ->
      {model with Connection=Waiting}, Cmd.bridgeSend(SetUser {Name = name; Color = color})
  | ConnectionLost -> {model with Connection = Disconnected}, Cmd.none
  | RC msg ->
    printfn "Remote message received: %A" msg
    match msg with
    | GetUsers l -> {model with ConnectedUsers = l}, Cmd.none
    | NameChange(ou, nu) ->
      {model with
        ConnectedUsers = model.ConnectedUsers |> List.map (fun ({Name=n} as u)-> if n = ou then {u with Name = nu} else u)
        Messages = model.Messages |> List.map (function ClientMsg(c,m) when c = ou -> ClientMsg(nu,m)| m -> m)
        }, Cmd.none
    | QueryConnected ->
        match model.Connection with
        |Connected u -> Bridge.Send(SetUser u)
        |Waiting | Disconnected -> ()
        {model with ConnectedUsers = []}, Cmd.bridgeSend UsersConnected
    | NameStatus (Some u) ->
        {model with Connection = Connected u; Mode = Message}, Cmd.none
    | NameStatus None ->
        {model with Connection = Disconnected}, Cmd.none
    | AddUser u ->
      {model with ConnectedUsers = u::model.ConnectedUsers}, Cmd.none
    | RemoveUser u ->
      {model with
        ConnectedUsers =
          model.ConnectedUsers
          |> List.filter (fun {Name=n} -> n<>u)}, Cmd.none
    | AddMsg m ->
      {model with Messages = m::model.Messages}, Cmd.none
    | ColorChange (u,c) ->
      let newConnUsers = model.ConnectedUsers |> List.map (fun ({Name=n} as o) ->if n=u then {o with Color=c} else o )
      {model with ConnectedUsers = newConnUsers}, Cmd.none
    | AddMsgs m -> {model with Messages = m}, Cmd.none
  | SetColor c ->
      model,
      match model.Connection with
      |(Connected ({Color=o} as u)) when o<>c -> Cmd.bridgeSend(SetUser {u with Color = c})
      |_ -> Cmd.none

let view =
  let v = FunctionComponent.Lazy(View.view, div [][])
  fun m d -> v {| model=m;dispatch=d |}

let start() =
    Program.mkProgram init update view
    |> Program.withBridgeConfig
        (Bridge.endpoint "http://localhost:8085/socket"
            |> Bridge.withUrlMode Bridge.UrlMode.Raw
            |> Bridge.withMapping RC
            |> Bridge.withWhenDown ConnectionLost)
    |> Program.withReactSynchronous "feliz-dev"
    |> Program.run
