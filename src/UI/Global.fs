module UI.Global

open Common
open Fable.React
open Fable.React.Props
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
    OnKeyDown (fun (ev:Browser.Types.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev)

let mutable onKeypress : (Browser.Types.KeyboardEvent -> bool) option = None
let mutable undoModal : unit -> unit = ignore
// Disabling these global listeners until I can rewrite them in the new model, because they interfere with typing in Ribbit
//Browser.document.addEventListener_keydown(fun ev ->
//    if onKeypress.IsSome && onKeypress.Value(ev) then
//        ev.preventDefault()
//    obj())
//Browser.document.addEventListener_keyup((fun ev ->
//    if ev.keyCode = KeyCode.escape then
//        undoModal()
//    obj()), true)

open Fable.Core.JsInterop
/// Helper method: an input which stores state locally in React.state and then calls onEnter when Enter is pressed
let statefulInput onEnter (extraProps: IHTMLProp list) =
    FunctionComponent.Of(fun _ ->
        let state = Hooks.useState ""
        let p : IHTMLProp list = [
                HTMLAttr.Value state.current
                DOMAttr.OnChange (fun ev ->
                    let v = !!ev.target?value
                    state.update(thunk v)
                    )
                onKeyDown KeyCode.enter (fun _ -> onEnter state.current; state.update emptyString)
                ]
        input (p@extraProps))[]

