module UI.Components

open Fable.React

let stateful initState render =
    FunctionComponent.Of(fun () ->
        let state = Hooks.useState initState
        render state
        )

