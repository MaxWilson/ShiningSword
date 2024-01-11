module UI.DFRPG.Chargen.View
open Feliz
open Menus
open UI.DFRPG.Chargen

module private Impl =
    let button (txt: string) onClick = Html.button [ prop.text txt; prop.onClick onClick ]
    let combine = function [] -> React.fragment [] | [v] -> v | vs -> Html.ul [prop.style [style.listStyleType.none; style.listStylePosition.outside]; prop.children vs]
    let toggle dispatch (key: Key) (newValue: bool) =
        if newValue then dispatch (SetKey(key, Some Flag))
        else dispatch (SetKey(key, None))
    let changeLevel dispatch key (newLevel: int) (levelCount: int) =
        if newLevel < 0 then dispatch (SetKey(key, None))
        elif newLevel < levelCount then dispatch (SetKey(key, Some (Level newLevel)))
    let checkbox (txt: string) checked' (onChange: bool -> unit) children =
        let id = System.Guid.NewGuid().ToString()
        Html.li [
            Html.input [ prop.type'.checkbox; prop.id id; prop.isChecked checked'; prop.onChange onChange ]
            Html.label [ prop.htmlFor id; prop.text txt ]
            match children with
            | [] -> ()
            | children ->
                combine children
            ]

    let reactApi dispatch: ReactElement RenderApi =
        let toggle = toggle dispatch
        {
        checked' = fun (label, key, children) -> checkbox label true (toggle key) children
        unchecked = fun (label, key) -> checkbox label false (toggle key) []
        leveledLeaf = fun (label, key, level, levelCount) -> class' "" Html.li [ button "-" (thunk4 changeLevel dispatch key (level-1) levelCount); button "+" (thunk4 changeLevel dispatch key (level+1) levelCount); Html.text label ]
        unconditional = fun (label, children) -> class' "" Html.li [ Html.text label; combine children ]
        combine = combine
        }
    let eval dispatch selections (template: Trait ListOffer list) =
        let value, menus =
            [
            for offer in template do
                evaluate { OfferInput.fresh with selected = selections } offer
            ]
            |> List.unzip
        let react = render (reactApi dispatch) menus
        value |> List.collect id, react
open Impl

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    let profession = swash
    let value, react = eval dispatch model.selections profession // value will be used later for things like enabling/disabling the Save button
    Html.div [
        srcLink
        react
        ]