module View.ViewComponents

open Fable.React
open Fable.React.Props

// an input-like component which stores state locally until blur
let localInput =
    let component' =
        FunctionComponent.Of(
            fun (value, props: seq<IHTMLProp>, onChange) ->
                let v = Hooks.useState value
                Hooks.useEffect(
                    fun () ->
                        v.update(value) |> ignore // when value changes externally, make sure we detect that!
                    , [|value|] )
                let lst : IHTMLProp list = [
                    yield upcast Value v.current
                    match props
                        |> Seq.choose(function :? DOMAttr as a -> Some a | _ -> None)
                        |> Seq.tryFind(function OnChange(_) -> true | _ -> false) with
                    | Some (OnChange event) ->
                        yield upcast OnChange(fun e -> if e <> null then v.update(e.Value); event e)
                    | _ ->
                        yield upcast OnChange(fun e -> if e <> null then v.update(e.Value))
                    yield upcast OnKeyDown(fun e -> if e.keyCode = 13. then
                                                        e.preventDefault()
                                                        onChange v.current
                                                    )
                    yield upcast OnBlur(fun _ -> onChange v.current)
                    yield! props
                    ]
                input lst
            , memoizeWith = (fun (v1, p1, _) (v2, p2, _) -> v1 = v2))
    (fun value (props: seq<IHTMLProp>) onChange -> component'(value, props, onChange))

// an input-like component which stores state locally until blur
let localForm =
    let component' =
        FunctionComponent.Of(
            fun (query, props: seq<IHTMLProp>, onChange) ->
                let st = Hooks.useState ""
                Hooks.useEffect(fun e ->
                        st.update ""
                    , [|query|])
                form(Seq.append [OnSubmit(fun e -> e.preventDefault(); onChange st.current)] props)[
                    h2[][str query]
                    localInput st.current [] (fun v -> st.update v; onChange v)
                    button[Type "submit"; Default true][str "OK"]
                    ]
            , memoizeWith = equalsButFunctions)
    (fun query (props: seq<IHTMLProp>) onChange -> component'(query, props, onChange))
