module UI.Components
open UI
open UI.Chargen
open UI.Chargen.View
open Elmish
open Domain
open Domain.Character
open Domain.Character.DungeonFantasy
open Domain.Character.Universal
open Feliz
open Fable.Core.JsInterop

[<ReactComponent>]
let AutoFocusInput props =
    let self = React.useRef None
    React.useEffectOnce(fun _ ->
        if self.current.IsSome then
            self.current?focus()
            self.current?select())
    Html.input (prop.ref self::props)

[<ReactComponent>]
let View (model: Model) (control: ParentMsg -> unit) dispatch =
    class' Html.div "charGen" [
        let header (child: ReactElement) =
            class' Html.div "header" [
                match model.ruleset with
                | Ruleset.TSR ->
                    Html.text "Create a character for Advanced Dungeons and Dragons!"
                | Ruleset.WotC ->
                    Html.text "Create a character for Fifth Edition Dungeons and Dragons!"
                | Ruleset.DungeonFantasy ->
                    Html.text "Create a character for Dungeon Fantasy RPG (powered by GURPS)!"
                    child
                ]
        match model.ruleset with
            | Ruleset.TSR -> ()
            | Ruleset.WotC -> ()
            | Ruleset.DungeonFantasy ->
                UI.Chargen.Components.DF.View header model.dfChar (FwdDungeonFantasy >> dispatch)
        ]
