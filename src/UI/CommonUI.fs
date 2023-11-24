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

    let err msg clearError =
        class' "error" Html.div [
            Html.text $"There has been an error: {msg}"
            Html.div [
                Html.button [ prop.onClick (fun _ -> clearError()); prop.children [Html.text "Dismiss"] ]
                ]
            ]

    [<ReactComponent>]
    let WindowProtector(error, setError, child) =
        React.useWindowListener.onError(fun (ev: Browser.Types.UIEvent) -> setError (Some (ev?message: string)))
        React.useWindowListener.onUnhandledRejection(fun (ev: Browser.Types.PromiseRejectionEvent) -> setError (Some (ev.reason.ToString())))
        let child = Html.div [prop.children [child]; prop.style [Feliz.style.margin (length.px 0); if Option.isSome error then Feliz.style.display.none]]
        React.fragment [
            child
            match error with
            | Some error ->
                class' "error" Html.div [
                    Html.text $"There has been an error: {error}"
                    Html.div [
                        Html.button [ prop.onClick (fun _ -> setError None); prop.children [Html.text "Dismiss"] ]
                        ]
                    ]
            | None -> ()
            ]

    type [<AllowNullLiteral>] InfoComponentObject =
        abstract componentStack: string with get

    type ErrorBoundaryProps =
        {   Inner : React.ReactElement
            ErrorComponent : string -> (unit -> unit) -> React.ReactElement
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
            this.setState(fun _ _ -> { Error = Some (error.ToString()) })

        override this.render() =
            let setError v = this.setState(fun _ _ -> { Error = v })
            let clearError = fun () -> setError None
            WindowProtector(this.state.Error, setError, this.props.Inner)

    let renderCatchSimple errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = fun _ -> () } [ ]

    let renderCatchFn onError errorElement element =
        ReactElementType.create ReactElementType.ofComponent<ErrorBoundary,_,_> { Inner = element; ErrorComponent = errorElement; OnError = onError } [ ]
