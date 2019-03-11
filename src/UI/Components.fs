module UI.Components

type StateHolder<'model> = StateHolder of 'model
type StatefulProps<'model> = {
    initial: 'model
    render: 'model -> (('model -> 'model) -> unit) -> Fable.Import.React.ReactElement
    equal: 'model -> 'model -> bool
}
type Stateful<'t>(props) as this =
    inherit Fable.Import.React.Component<StatefulProps<'t>, StateHolder<'t>>(props)
    do this.setInitState(StateHolder props.initial)
    let extract (StateHolder v) = v
    override this.shouldComponentUpdate(_nextProps, nextState) =
        not <| this.props.equal (extract this.state) (extract nextState)
    override this.render() =
        let doUpdate = (fun updater -> printfn "Update"; this.setState(fun (StateHolder st) _props -> StateHolder (updater st)))
        this.props.render (extract this.state) doUpdate

let stateful initState eq render =
    Fable.Helpers.React.ofType<Stateful<'t>, _, _>({initial = initState; equal = eq; render = render})[]

