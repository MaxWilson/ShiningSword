namespace App

open Feliz
open Feliz.Router
open Feliz.UseElmish
open Elmish
type private Model = { name: string; age: int; }
type private Msg = ChangeName of string | ChangeAge of int
type Components =
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

    [<ReactComponent>]
    static member EmployeeEditor (start: string) =

        let init() = { name = start; age = 43 }
        let update msg model =
            match msg with
            | ChangeName name -> { name = name; age = model.age + 1 }
            | ChangeAge age -> { model with age = age }
        let p () = Program.mkSimple init update (fun _ _ -> ())
        let state, dispatch = React.useElmish(p, [| |])
        Html.div [
            Html.h1 state.name
            Html.input [
                prop.value state.name
                prop.onChange (ChangeName >> dispatch)
            ]
            Html.text $"is at least {state.age} years old!"
        ]
    /// <summary>
    /// A stateful React component that maintains a counter
    /// </summary>
    [<ReactComponent>]
    static member Counter() =
        let (count, setCount) = React.useState(0)
        Html.div [
            Html.h1 count
            Html.button [
                prop.onClick (fun _ -> setCount(count + 1))
                prop.text "Increment"
            ]
            Components.EmployeeEditor "Tom"
        ]

    /// <summary>
    /// A React component that uses Feliz.Router
    /// to determine what to show based on the current URL
    /// </summary>
    [<ReactComponent>]
    static member Router() =
        let (currentUrl, updateUrl) = React.useState(Router.currentUrl())
        React.router [
            router.onUrlChanged updateUrl
            router.children [
                match currentUrl with
                | [ ] -> Html.h1 "Index"
                | [ "hello" ] -> Components.HelloWorld()
                | [ "counter" ] -> Components.Counter()
                | otherwise -> Html.h1 "Not found"
            ]
        ]
