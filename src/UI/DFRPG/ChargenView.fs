module UI.DFRPG.Chargen.View
open Feliz
open Menus
open UI.DFRPG.Chargen

module private Impl =
    let button (txt: string) onClick = Html.button [ prop.text txt; prop.onClick onClick ]
    let checkbox (txt: string) checked' (onChange: bool -> unit) =
        let id = System.Guid.NewGuid().ToString()
        Html.div [
            Html.input [ prop.type'.checkbox; prop.id id; prop.isChecked checked'; prop.onChange onChange ]
            Html.label [ prop.htmlFor id; prop.text txt ]
            ]
    let reactApi: ReactElement RenderApi = {
        checked' = fun (label, key, children) -> checkbox label true ignore
        unchecked = fun (label, key) -> checkbox label false ignore
        leveledLeaf = fun (label, level) -> React.fragment [ button "-" ignore; button "+" ignore; Html.text label ]
        unconditional = fun (label, children) -> React.fragment [ Html.text label; Html.div children ]
        combine = function [v] -> v | vs -> React.fragment vs
        }
    let eval selections (template: Trait ListOffer list) =
        let value, menus =
            [
            for offer in template do
                evaluate { OfferInput.fresh with selected = selections } offer
            ]
            |> List.unzip
        let react = render reactApi menus
        value |> List.collect id, react
open Impl

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let profession = swash
    let value, react = eval model.selections profession // value will be used later for things like enabling/disabling the Save button
    Html.div [
        srcLink
        react
        ]