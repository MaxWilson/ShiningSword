module UI.Chargen.TraitView
open Domain.Character.DungeonFantasy
open Domain.Character.DungeonFantasy.Templates
open Fable.React
open Feliz
open Fable.Core

type TraitMsg =
    | Add of Trait
    | Remove of Trait
type ReactBuilder(char: Character, dispatch: TraitMsg -> unit) =
    interface OutputBuilder<Trait, ReactElement> with
        // labeled binary
        member _.binaryL(value, label) =
            let chkId = $"chk-{value}{label}"
            let isChecked = char.traits |> List.contains value
            let toggle _ =
                if isChecked then dispatch (Remove value)
                else dispatch (Add value)
            Html.section [
                Html.input [prop.id chkId; prop.type'.checkbox; prop.isChecked isChecked; prop.onClick toggle]
                Html.label [prop.text label; prop.htmlFor chkId]
                ]
        member this.binary(value) = this.up.binaryL(value, (value.ToString()))
        member _.aggregate values = Html.div values
    member private this.up = this :> OutputBuilder<_,_>
