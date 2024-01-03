[<AutoOpen>]
module CommonUI

open Feliz
open Elmish
open Feliz.UseElmish
let class' (className: string) ctor (elements: ReactElement seq) = ctor [prop.className className; prop.children elements]
let classP' (className: string) ctor (elements: IReactProperty list) : ReactElement = ctor ((prop.className className)::elements)
let classTxt' (className: string) ctor (txt: string) = ctor [(prop.className className); prop.text txt]
let divWrap (className: string) element =
    Html.div [
        prop.className className
        prop.children [element]
        ]
exception UserFacingException of msg:string
let informUserOfError msg = UserFacingException msg |> raise
let srcLink =
    classP' "srcLink" Html.a [
        prop.href "https://github.com/MaxWilson/ShiningSword/"
        prop.children [Html.img [prop.ariaLabel "GitHub"; prop.src "img/GitHub_Logo.png"]]
        prop.target "_blank"
        ]

type React =
    static member inline useElmishSimple (init: _ -> 'model) (update: 'msg -> 'model -> 'model) =
        React.useElmish(fun _ -> Program.mkSimple init update (fun _ _ -> ()))
    static member inline useStateWithDependencies (getState: unit -> _) dependencies =
        // we want to reinitialize combat if and only if dependencies change, instead of never reinitializing it.
        let deps, setDeps = React.useState (thunk dependencies)
        let currentValue, setter = React.useState getState // for perf reasons we expect state to come in as a function instead of a concrete value
        if(deps <> dependencies) then // will happen when we run a new combat
            setDeps dependencies
            let state = getState()
            setter state
            state, setter
        else currentValue, setter
    static member ofJsx (jsx: Fable.Core.JSX.Element) = jsx |> unbox<ReactElement>
    static member toJsx (element: ReactElement) = element |> unbox<Fable.Core.JSX.Element>

// originally from https://github.com/fable-compiler/fable-react/blob/master/docs/react-error-boundaries.md, but updated to Fable 4
module ReactErrorBoundary =
    open Fable.Core
    open Fable.React
    open Fable.Core.JsInterop
    open Feliz
    open Feliz.Router
    open Feliz.Listeners
    open Browser.Dom
    open Fable
    open Fable.Core.JsInterop

    [<ReactComponent>]
    let WindowProtector setError =
        React.useWindowListener.onError(fun (ev: Browser.Types.UIEvent) -> setError (Some ("[window] " + (if ev?error then ev?error?msg else ev?message): string)))
        React.useWindowListener.onUnhandledRejection(fun (ev: Browser.Types.PromiseRejectionEvent) -> setError (Some ("[unhandled rejection] " + ev.reason.ToString())))
        React.fragment [
            ]

    type [<AllowNullLiteral>] InfoComponentObject =
        abstract componentStack: string with get

    type ErrorBoundaryProps =
        {   Inner : React.ReactElement
            ErrorComponent : string -> (string option -> unit) -> React.ReactElement
            OnError : exn * InfoComponentObject -> unit }

    type ErrorBoundaryState =
        { Error : string option }

    // See https://github.com/MangelMaxime/Fulma/blob/master/docs/src/Widgets/Showcase.fs
    // See https://reactjs.org/docs/error-boundaries.html
    type ErrorBoundary(props) =
        inherit React.Component<ErrorBoundaryProps, ErrorBoundaryState>(props)
        do base.setInitState({ Error = None })

        override this.componentDidCatch(error, info) =
            let info = info :?> InfoComponentObject
            this.props.OnError(error, info)
            let error = if error?msg then error?msg elif error?error && error?error?msg then error?error?msg else error?Message + error?StackTrace
            this.setState(fun _ _ -> { Error = Some error })

        override this.render() =
            let setErrorFromString v = this.setState(fun _ _ ->{ Error = v }) // won't have a stack trace if it doesn't come from an exception
            React.fragment [
                WindowProtector (fun exn -> async { setErrorFromString exn } |> Async.StartImmediate)
                match this.state.Error with
                | Some err -> this.props.ErrorComponent err setErrorFromString
                | None -> this.props.Inner
                ]
    let err (error: string) (setError: (string option -> unit)) =
        class' "error" Html.div [
            yield! ("There has been an error:" :: (error.Split("\n") |> List.ofArray)) |> List.map Html.div
            Html.div [
                Html.button [ prop.onClick (fun _ -> setError None); prop.children [Html.text "Dismiss"] ]
                ]
            ]

    let renderCatchSimple element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = err; OnError = fun _ -> () } [ ]

    let renderCatchFn onError errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = onError } [ ]
