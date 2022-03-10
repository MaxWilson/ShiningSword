namespace App

open Feliz
open Feliz.Router

type Components =

    static member rand = System.Random()
    /// <summary>
    /// The simplest possible React component.
    /// Shows a header with the text Hello World
    /// </summary>
    [<ReactComponent>]
    static member HelloWorld() = Html.h1 "Hello World"

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
                | [ ] -> Html.h1 $"Index: {currentUrl}"
                | [ "hello" ] -> Components.HelloWorld()
                | [ "counter" ] -> Components.Counter()
                | otherwise ->
                    let messages = ["Not found, y'all"; "sorry, couldn't find it"; "Nope, try again"]
                    Html.h1 (messages[Components.rand.Next(3)])
            ]
        ]

open Fable.Core
open Fable.Core.JsInterop
module Konva =

    [<Erase>]
    type IAppProperty =
        interface end

    [<Erase>]
    type IContentProperty =
        interface end

    module Interop =    
        let inline mkAppAttr (key: string) (value: obj) : IAppProperty = unbox (key, value)
        let inline mkContentAttr (key: string) (value: obj) : IContentProperty = unbox (key, value)
    let stage props = Interop.reactApi.createElement(import "Stage" "react-konva", createObj !!props)
    let layer props = Interop.reactApi.createElement(import "Layer" "react-konva", createObj !!props)
    let circle props = Interop.reactApi.createElement(import "Circle" "react-konva", createObj !!props)
    let text props = Interop.reactApi.createElement(import "Text" "react-konva", createObj !!props)
    
open Konva

[<Erase>]
type Konva =
    static member inline App (props: #IAppProperty list) =
        Interop.reactApi.createElement(import "App" "react-konva", createObj !!props)
    
    static member inline Content (props: #IAppProperty list) =
        Interop.reactApi.createElement(import "Content" "react-konva", createObj !!props)

    static member inline DemoShapes(x) =
        let window = Browser.Dom.window;
        stage [
            "width" ==> window.innerHeight
            "width" ==> window.innerWidth
            "height" ==> window.innerHeight
            "children" ==> [
                layer [
                    "children" ==> [
                        text [
                            "text" ==> "Some text here"
                            "fontSize" ==> "15"
                            ]
                        circle [
                            "x" ==> x
                            "y" ==> "100"
                            "radius" ==> "50"
                            "fill" ==> "green"
                            ]
                        ]
                    ]
                ]
            ]
            
module Maze =
    open Domain
    let render (maze: Maze) =
        let window = Browser.Dom.window;

        stage [
            "width" ==> window.innerWidth - 100.
            "height" ==> window.innerHeight - 100.
            "children" ==> [
                layer [
                    "children" ==> [
                        let zip a b = a,b
                        for x, row in maze.grid |> Array.mapi zip do
                            for y, cell in row |> Array.mapi zip do
                                if cell = Closed then
                                    circle [
                                        "x" ==> x * 20 + 10
                                        "y" ==> y * 20 + 10
                                        "radius" ==> 10
                                        "fill" ==> "grey"
                                        ]
                        ]
                    ]
                ]
            ]
