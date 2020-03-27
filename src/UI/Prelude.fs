module UI
open Feliz
open Fable.React

module Feliz =
    // in order to make Feliz openable as a static class, we derive from it

    [<AbstractClass; Sealed>]
    type Html =
        inherit Feliz.Html

    [<AbstractClass; Sealed>]
    type Prop =
        inherit Feliz.prop

    [<AbstractClass; Sealed>]
    type React =
        inherit Feliz.React

type Bulma = Zanaptak.TypedCssClasses.CssClasses<"""..\node_modules\bulma\css\bulma.min.css""">

[<AutoOpen>]
module Prelude =
    // an input-like component which stores state locally until blur
    let localInput =
        let component' =
            FunctionComponent.Of(
                fun (value: string, props: seq<IReactProperty>, onChange) ->
                    let v = Hooks.useState value
                    Hooks.useEffect(
                        fun () ->
                            v.update(value) |> ignore // when value changes externally, make sure we detect that!
                        , [|value|] )
                    let lst : IReactProperty list = [
                        prop.value v.current
                        prop.onChange(fun (txt: string) -> v.update(txt))
                        prop.onKeyDown(fun e -> if e.keyCode = 13. then
                                                            e.preventDefault()
                                                            onChange v.current
                                                        )
                        prop.onBlur(fun _ -> onChange v.current)
                        yield! props
                        ]
                    Html.input lst
                , memoizeWith = (fun (v1, p1, _) (v2, p2, _) -> v1 = v2))
        (fun value (props: seq<IReactProperty>) onChange -> component'(value, props, onChange))

    // an input-like component which stores state locally until blur
    let localForm =
        let component' =
            FunctionComponent.Of(
                fun (query: string, props: seq<IReactProperty>, validator, onSubmit) ->
                    let st = Hooks.useState ""
                    Hooks.useEffect(fun e ->
                            st.update ""
                        , [|query|])
                    Html.form [
                        prop.onSubmit <| fun v ->
                            let v = st.current
                            if validator v then
                                st.update ""
                                onSubmit v
                        prop.children [
                            Html.h2 query
                            Html.input [
                                prop.value st.current;
                                prop.onChange (fun (v: string) -> v |> st.update)]
                            Html.button [prop.type'.submit; prop.defaultValue true; prop.text "OK"]
                            ]
                        yield! props
                        ]
                , memoizeWith = equalsButFunctions)
        (fun query (props: seq<IReactProperty>) validator onSubmit -> component'(query, props, validator, onSubmit))
