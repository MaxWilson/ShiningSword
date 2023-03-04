module UI.Chargen.TraitView
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Fable.React
open Feliz
open Fable.Core
open UI

type TraitMsg =
    | Add of Trait
    | Remove of Trait
type ReactBuilder(char: Character, dispatch: TraitMsg -> unit) =
    interface OutputBuilder<Menus.Chosen, ReactElement> with
        member this.chooseOneFromHierarchy(arg1) = notImpl()
        member this.chooseWithStringInput(arg1, arg2) = notImpl()
        member this.grant(arg1) = notImpl()
        member this.grantOne(arg1) = notImpl()
        member this.grantWithStringInput(arg1, arg2) = notImpl()
        member this.choose2D(arg1) = notImpl()
        member this.chooseLevels(arg1) = notImpl()
        member this.chooseUpToBudget arg1 arg2 = notImpl()
        // labeled binary
        member _.binary(value, label) =
            match value with
            | Menus.Trait value ->
                let chkId = $"chk-{value}{label}"
                let isChecked = char.traits |> List.contains value
                let toggle newValue =
                    if newValue then dispatch (Add value)
                    else dispatch (Remove value)
                class' "binary" Html.section [
                    Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked isChecked; prop.onChange toggle]
                    Html.label [prop.text label; prop.htmlFor chkId]
                    ]
            | _ -> notImpl()
        member this.binary(value) = this.up.binary(value, (value.ToString() |> String.uncamel))
        member _.aggregate values = class' "gridContainer selection" Html.div values
    member private this.up = this :> OutputBuilder<_,_>
