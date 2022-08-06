module View


open Fable.React
open Fable.React.Props
open Model
open Shared
open Fulma

open Fable.Core.JsInterop
importAll "../../../node_modules/bulma/bulma.sass"
open Elmish.Bridge
open Browser.Types

let ColorToColor = function
  | Red -> IsDanger
  | Green -> IsSuccess
  | Blue -> IsLink
  | Black -> IsDark
let formatUser {Name=n;Color=c} = Message.message [Message.Size IsSmall;Message.Color (ColorToColor c)] [Message.body [][str n]]
let colorSelector (dispatch:(Color -> unit)) =
  [Red;Green;Blue;Black]
  |> List.map (fun c ->
    Column.column [Column.Width (Screen.All,Column.IsOneQuarter)]
      [Button.a [
        Button.Color (ColorToColor c)
        Button.OnClick (fun _ -> dispatch c)][str " "]])
  |>
  Columns.columns [Columns.IsMobile;Columns.CustomClass "is-variable is-1";Columns.IsMultiline]
let formatMessage users = function
  | SysMsg {Time=t;Content=c}->
    Message.message [Message.Color IsWarning;Message.Size IsSmall][
      Message.body [][
        Columns.columns [Columns.IsMobile][
          Column.column[Column.Width (Screen.All,Column.Is11)][str c]
          Column.column[Column.Width (Screen.All,Column.Is1)][str (t.ToShortTimeString())]
        ]
      ]
    ]
  | ClientMsg (user,{Time=t;Content=c}) ->
    let color =
      users
      |> List.tryPick (fun {Name=n;Color = c} -> if n = user then Some (ColorToColor c) else None)
      |> Option.defaultValue IsWarning
    Message.message [Message.Color color;Message.Size IsSmall][
      Message.header[][str user]
      Message.body [][
        Columns.columns [Columns.IsMobile][
          Column.column[Column.Width (Screen.All,Column.Is11)][str c]
          Column.column[Column.Width (Screen.All,Column.Is1)][str (t.ToShortTimeString())]
        ]
      ]
    ]

let auto = Fable.React.Props.OverflowOptions.Auto
let none = Fable.React.Props.OverflowOptions.Clip
let view (p:{|model:Model; dispatch:ClientMsg->unit|}) =
  let props size : IHTMLProp list = [ Style [CSSProp.Height size;CSSProp.MaxHeight size;CSSProp.Overflow auto]]
  let userField = Hooks.useState ""
  let textField = Hooks.useState ""
  div [Style[CSSProp.Overflow auto]]  [
    Container.container [Container.Props [Style [CSSProp.Height "100vh";CSSProp.MaxHeight "100vh";CSSProp.Overflow none]]] [
        Columns.columns [Columns.IsMobile] [
          Column.column [Column.Width (Screen.All,Column.IsFourFifths);Column.Props (props "80vh")]
            (p.model.Messages |> List.map (formatMessage p.model.ConnectedUsers))
          Column.column [Column.Props (props "80vh")]
            (p.model.ConnectedUsers |> List.map formatUser)
        ]
        Container.container [Container.Props [Style [CSSProp.Height "10vh";CSSProp.MaxHeight "20vh";CSSProp.Position PositionOptions.Absolute;CSSProp.Bottom "0"]]]
          [
            let (c, pl, txf, act) =
              match p.model.Mode with
              | Message ->
                "Change user",
                "Message",
                textField,
                fun () -> Bridge.Send(SendMsg textField.current)
                          textField.update("")
              | User ->
                "Send message",
                "User name",
                userField,
                fun () -> p.dispatch (SendUser (userField.current,match p.model.Connection with Connected u -> u.Color | _ -> Color.Black))
            yield Media.media[][
                 Media.left [] [(colorSelector (SetColor>>p.dispatch))]
                 Media.content[][
                   Field.div[Field.HasAddons][
                     match p.model.Connection with
                     | Connected _ ->
                      yield Control.p [][
                        Button.a[Button.OnClick (fun _ -> p.dispatch ToggleMode)][str c]
                      ]
                     |_ -> ()
                     yield Control.p[Control.IsExpanded][
                     Input.text [
                      Input.Placeholder pl
                      Input.Value txf.current
                      Input.Disabled (p.model.Connection = Waiting)
                      Input.OnChange (fun e -> txf.update(unbox<string>(e.target?value)))
                      Input.Props [
                        OnKeyDown (fun (ev:KeyboardEvent) ->
                                  if ev.keyCode = 13. then
                                    ev.preventDefault()
                                    act())
                      ]]]
                    ]
                  ]
                 Media.right[][
                   Button.a [
                    Button.OnClick (fun _ -> act ())
                    Button.IsLoading (p.model.Connection = Waiting)
                    Button.Disabled (txf.current = "") ][str "Send"]
                 ]
            ]
          ]
      ]
  ]
