module UI.Display
open Fable.Core.JsInterop
open Fable.React.ReactBindings

let pixi: obj = importAll "pixi.js"
let rpf: obj = importAll "react-pixi-fiber"

let bunny props =
    React.createElement(rpf?Text, createObj ["text" ==> props?text], [])

type AnimationTime = AnimationTime of float
    with
    member this.v = match this with AnimationTime(v) -> v
    static member (+)(AnimationTime(lhs), AnimationTime(rhs)) = AnimationTime(lhs + rhs)
type Color = Black | Red | Grey
    with
    member this.asPixiColor =
        match this with
        | Black -> "black"
        | Red -> "red"
        | Grey -> "grey"
type FadingText = {
    color: Color
    text: string
    endTime: AnimationTime
    duration: AnimationTime
    }
type FadingTexts = {
    currentTime: AnimationTime
    texts: FadingText list
    }

let fadingTexts =
    Fable.React.FunctionComponent.Of(fun (props: FadingTexts) ->
        React.createElement(rpf?Stage, createObj ["options" ==> (createObj ["backgroundColor" ==> 0x10bb99; "height" ==> 200; "width" ==> 800])], [
                for t in props.texts ->
                    let alpha = max 0. (t.endTime.v - props.currentTime.v)/t.duration.v
                    React.createElement(rpf?Text, createObj ["text" ==> t.text; "style" ==> createObj(["fill" ==> t.color.asPixiColor]); "alpha" ==> alpha], [])
            ])
        )

let render (texts: FadingTexts) =
    React.createElement(fadingTexts, texts, [])
    //fadingTexts texts
