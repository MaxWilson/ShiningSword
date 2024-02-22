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
    module Wizard =
        type ConstraintStatus = Locked | Unlocked
        type 't Constraint = (ConstraintStatus * 't) option // locked * value
        type Nation = string
        type Sex = Male | Female | Neither
        type Race = string Constraint
        type RollMethod = Roll3d6InOrder | Fixed | Roll4d6DropLowest | AverageOf3d6 | PowerLawDistribution
        type RuleSystem = DFRPG | ADND
        type Constraints = {
            // should nation, sex, etc. be flags/fields instead of F# types?
            // also, don't they belong in Domain? They're domain logic, not UI-specific logic.
            //    Chargen should take in a set of constraints and spit out a character matching those
            //    constraints, and a new set of constraints on what things are still flexible, e.g.
            //    is changing race legal?
            sex: Sex Constraint
            race: Race Constraint
            nation: Nation Constraint
            profession: string Constraint
            ruleSystem: RuleSystem Constraint
            rollMethod: RollMethod Constraint
            }
        type Model = unit
        type Msg = unit
        let init _ = ()
        let update msg model = model
        [<ReactComponent>]
        let view() =
            let model, dispatch = React.useElmishSimple init update
            match model with
            | _ -> Html.div [ Html.text "Wizard" ]

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

let Wizard() = notImpl()


[<ReactComponent>]
let View (args: string list) =
    match args with
    | ["domain"; (Router.Route.Query [ "class", "swash" ] | Router.Route.Query [ "class", "swashbuckler" ] ) ] -> DomainView()
    | "domain"::_ -> DomainView()
    | _ -> Wizard.view()
