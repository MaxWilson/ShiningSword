module UI.DFRPG.Chargen.View
open Feliz
open Menus
open UI.DFRPG.Chargen

module private Impl =
    let button (txt: string) onClick = Html.button [ prop.text txt; prop.onClick onClick ]
    let bundle = function
        | [] -> React.fragment []
        | [props: IReactProperty list] -> Html.li props
        | proplists -> Html.ul [prop.style [style.listStyleType.none; style.listStylePosition.outside]; prop.children (proplists |> List.map (fun props -> Html.li props))]
    let toggle dispatch (key: Key) (newValue: bool) =
        if newValue then dispatch (SetKey(key, Some Flag))
        else dispatch (SetKey(key, None))
    let changeLevel dispatch key (newLevel: int) (levelCount: int) =
        if newLevel < 0 then dispatch (SetKey(key, None))
        elif newLevel < levelCount then dispatch (SetKey(key, Some (Level newLevel)))
    let checkbox (txt: string) checked' (onChange: bool -> unit) children =
        let id = System.Guid.NewGuid().ToString()
        [
            prop.children [
                Html.input [ prop.type'.checkbox; prop.id id; prop.isChecked checked'; prop.onChange onChange ]
                Html.label [ prop.htmlFor id; prop.text txt ]
                match children with
                | [] -> ()
                | children ->
                    bundle children
                ]
            ]

    let reactApi dispatch: IReactProperty list RenderApi =
        let toggle = toggle dispatch
        {
        checked' = fun (label, key, children) -> checkbox label true (toggle key) children
        unchecked = fun (label, key) -> checkbox label false (toggle key) []
        leveledLeaf = fun (label, key, level, levelCount) -> [ prop.children [ button "-" (thunk4 changeLevel dispatch key (level-1) levelCount); button "+" (thunk4 changeLevel dispatch key (level+1) levelCount); Html.text label ] ]
        unconditional = fun (label, children) -> [ prop.children [ Html.text label; bundle children ] ]
        combine = fun proplists -> [
            prop.children [
                for props in proplists do
                    Html.div props
                ]
            ]
        }
    let eval dispatch selections (template: _ ListOffer list) =
        let value, menus =
            [
            for offer in template do
                evaluate { OfferInput.fresh with selected = selections } offer
            ]
            |> List.unzip
        let reactProps: IReactProperty list = render (reactApi dispatch) menus
        value |> List.collect id, Html.div reactProps
open Impl

[<ReactComponent>]
let DomainView() =
    let model, dispatch = React.useElmishSimple init update
    let profession = Domain.DFRPG.Templates.swashbuckler
    let value, react = eval dispatch model.selections profession // value will be used later for things like enabling/disabling the Save button
    Html.div [
        srcLink
        react
        ]

[<ReactComponent>]
let StubView () =
    let model, dispatch = React.useElmishSimple init update
    let profession = swash
    let value, react = eval dispatch model.selections profession // value will be used later for things like enabling/disabling the Save button
    Html.div [
        srcLink
        react
        ]

[<ReactComponent>]
let View (args: string list) =
    match args with
    | ["domain"; "swashbuckler"] | ["domain"; "swash"] -> DomainView()
    | _ -> StubView()
