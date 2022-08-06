module Model

[<AutoOpen>]
module Shared =
    type Color =
        | Red
        | Green
        | Blue
        | Black

    type User = {Name : string; Color: Color}
    type Message = {Time : System.DateTime; Content: string}

    type Msgs =
      | ClientMsg of (string*Message)
      | SysMsg of Message

    type RemoteClientMsg =
        | QueryConnected
        | GetUsers of User list
        | NameStatus of User option
        | AddUser of User
        | RemoveUser of string
        | AddMsg of Msgs
        | AddMsgs of Msgs list
        | ColorChange of string * Color
        | NameChange of string * string

    type RemoteServerMsg = SetUser of User | SendMsg of string | UsersConnected


    module Remote =
        let socketPath = "/socket"

type ClientMsg =
    | RC of RemoteClientMsg
    | SendUser of string * Color
    | ConnectionLost
    | ToggleMode
    | SetColor of Color

type Connection =
  | Disconnected
  | Waiting
  | Connected of User

type FieldMode =
  | User
  | Message

type Model = {
    Mode : FieldMode
    Connection : Connection
    ConnectedUsers : User list
    Messages : Msgs list
}
