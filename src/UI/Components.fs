module UI.Components

type StateHolder<'model> = StateHolder of {| model: 'model option; dirty: bool |}
type StatefulProps<'model> = {
    render: 'model option -> ('model -> unit) -> (unit -> unit) -> Fable.Import.React.ReactElement
}
type Stateful<'t>(props) as this =
    inherit Fable.Import.React.Component<StatefulProps<'t>, StateHolder<'t>>(props)
    do this.setInitState(StateHolder {| model = None; dirty = false; |})
    let extract (StateHolder v) = v
    override this.shouldComponentUpdate(_nextProps, nextState) =
        match nextState with
        | StateHolder v when v.dirty ->
            this.setState(fun (StateHolder st) _ -> StateHolder {| st with dirty = false |}) // not dirty any more after component updates
            true
        | _ -> false
    override this.render() =
        let update value =
            this.setState(fun _ _ -> StateHolder {| model = Some value; dirty = true |})
        let clear() =
            this.setState(fun _ _ -> StateHolder {| model = None; dirty = true |})
        this.props.render (extract this.state).model update clear

let stateful render =
    Fable.Helpers.React.ofType<Stateful<'t>, _, _>({ render = render })[]

