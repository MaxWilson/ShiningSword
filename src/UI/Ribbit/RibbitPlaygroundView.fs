module UI.Ribbit.PlaygroundView
open Ribbit
open Feliz
open Feliz.UseElmish

type private DataKey = string
type private Value = Text of string | Number of int
type private DataBank = Map<DataKey, Value>
type private TODO = Unit
type private Model = { data: DataBank; definitions: TODO }
    with
    static member fresh = { data = Map.empty; definitions = () }

type private Msg = TODO

// make this module private so it will interfere less with React hot-loading
module private Impl =
    let data =
        [
        "Rath", [ "HP", Number 14; "maxHP", Number 53; "AC", Number 3; "Status", Text "OK" ]
        "Delsenora", [ "HP", Number 23; "maxHP", Number 23; "SP", Number 22; "maxSP", Number 29; "AC", Number 10; "Status", Text "OK" ]
        "Wild Boar", [ "HP", Number 34; "maxHP", Number 49; "AC", Number 6; "Status", Text "OK" ]
        ]
        |> List.map (fun (name, stats) -> Map.ofList (("Name", Text name)::stats))
    let numberValue name row =
        match Map.tryFind name row with
        | Some (Number v) -> Some v
        | _ -> None
    let textValue name row =
        match Map.tryFind name row with
        | Some (Text v) -> Some v
        | _ -> None
    let text name = { header = name; cell = textValue name >> Option.defaultValue "" >> Html.text }
    let textWithDefault name defaultValue = { header = name; cell = textValue name >> Option.defaultValue defaultValue >> Html.text }
    let numeric name = { header = name; cell = fun row ->   match numberValue name row, numberValue ("max" + name) row with
                                                            | Some v, Some max ->
                                                                let className = if v <= max / 3 then "dangerouslyLow" elif v <= (max * 2 / 3) then "mediumLow" else "enough"
                                                                classP' className Html.div [prop.text $"{v}/{max}"]
                                                            | Some v, None -> Html.div (string v)
                                                            | _ -> Html.div []
                            }

    let init _ = Model.fresh

    let update msg model = model
open Impl

[<ReactComponent>]
let View() =
    let model dispatch = React.useElmishSimple init update
    class' "scrollParent" Html.div [
        srcLink
        table [
            text "Name"
            numeric "HP"
            numeric "SP"
            numeric "AC"
            textWithDefault "Status" "OK"
            ] data
        class' "scrollable" Html.div [
            Html.div "Rath's HP are now 14"
            Html.div "Delsenora casts Fireball!"
            Html.div "Wild Boar attacks Rath!"
            ]
        Html.input [prop.placeholder "Enter a command"]
        ]