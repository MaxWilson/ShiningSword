module UI.Adventure
open Feliz

type ControlMsg = Quit
type Model = {
    state: Domain.Adventure.AdventureState
    }

let init state =
    { state = state }
let class' element (className: string) (children: _ seq) = element [prop.className className; prop.children children]
let view model control dispatch =
    class' Html.div "adventure" [
        yield! Chargen.View.viewCharacter model.state.mainCharacter
        Html.span [prop.text "We're going on an adventure (Under Construction)!"; prop.className "header"]
        class' Html.div "finalize" [
            Html.button [prop.text "Quit Without Saving"; prop.onClick (thunk1 control Quit)]
            ]
        ]

