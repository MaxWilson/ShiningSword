module UI.Global

open Common
open Fable.Helpers
open Fable.Helpers.React.Props
open Fable.Import

type OldPage =
  | Home
  | Counter
  | About

let describeAC fullInfo ac =
    let descr =
        if ac <= 8 then "Target practice"
        elif ac <= 11 then "Unarmored"
        elif ac <= 13 then "Lightly armored"
        elif ac <= 15 then "Moderately armored"
        elif ac <= 17 then "Heavily armored"
        elif ac <= 20 then "Very heavily armored"
        else "Walking fortress"
    if fullInfo then sprintf "%d (%s)" ac descr else descr

let describeStatus hp maxHp =
    match hp with
    | hp when hp <= 0 -> "Dead"
    | hp when hp <= (maxHp / 4) -> "Badly wounded"
    | hp when hp <= (maxHp * 3 / 4) -> "Wounded"
    | hp when hp < maxHp -> "Barely wounded"
    | _ -> "OK"

module KeyCode =
    let enter = 13.
    let upArrow = 38.
    let downArrow =  40.
    let escape = 27.

let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev)

let mutable onKeypress : (Browser.KeyboardEvent -> bool) option = None
Browser.document.addEventListener_keydown(fun ev ->
    if onKeypress.IsSome && onKeypress.Value(ev) then
        ev.preventDefault()
    obj())
let mutable undoModal : unit -> unit = ignore
Browser.document.addEventListener_keyup((fun ev ->
    if ev.keyCode = KeyCode.escape then
        undoModal()
    obj()), true)

open Fable.Core.JsInterop
/// Helper method: an input which stores state locally in React.state and then calls onEnter when Enter is pressed
let statefulInput onEnter (props: IHTMLProp list) =
    Components.stateful "" (=) <| fun txt update ->
        let props : IHTMLProp list = [
                yield Value txt
                yield OnChange (fun ev ->
                    let v = !!ev.target?value
                    update (thunk v)
                    )
                yield onKeyDown KeyCode.enter (fun _ -> onEnter txt; update (thunk emptyString))
                yield AutoFocus true // Open question? Would we ever want this NOT to be true? For now it's hard-wired.
                yield! props
                ]
        React.input props
