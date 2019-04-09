module UI.Components

type StateHolder<'model> = StateHolder of {| model: 'model option; dirty: bool |}
type StatefulProps<'model> = {
    render: 'model -> (('model -> 'model) -> unit) -> Fable.Import.React.ReactElement
}
type Stateful<'t>(props) as this =
    inherit Fable.Import.React.Component<StatefulProps<'t>, StateHolder<'t>>(props)
    do this.setInitState(StateHolder {| model = None; dirty = false; |})
    let extract (StateHolder v) = v
    override this.shouldComponentUpdate(_nextProps, nextState) =
        match nextState with
        | StateHolder v when v.dirty -> true
        | _ -> false
    override this.render() =
        let update value =
            this.setState(fun _ _ -> StateHolder {| model = Some value; dirty = true |})
        let clear() =
            this.setState(fun _ _ -> StateHolder {| model = None; dirty = false |})
        let doUpdate = (fun updater ->
            this.setState(fun (StateHolder st) _props ->
                                
                                StateHolder (updater st.model)))
        this.props.render (extract this.state) doUpdate

let stateful initState eq render =
    Fable.Helpers.React.ofType<Stateful<'t>, _, _>({initial = initState; equal = eq; render = render})[]

