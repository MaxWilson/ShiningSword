module UI.Roleplaying
open Domain
open Domain.Character.Core
open Domain.Character.Universal
open Feliz
open Feliz.UseElmish

type Model = RoleplayingData

let init _ =
    let sex = chooseRandom [Male; Female]
    let nationalOrigin, name = makeNameAnyNation sex
    {
        name = name
        nationalOrigin = nationalOrigin
        sex = sex
    }

type Msg =
    | SetName of string
    | SetSex of Sex
    | SetOrigin of string
    | RecomputeName

let update msg (model: RoleplayingData) =
    match msg with
    | SetName v -> { model with name = v }
    | SetSex v -> { model with sex = v }
    | SetOrigin v -> { model with nationalOrigin = v }
    | RecomputeName ->
        match makeName model.nationalOrigin model.sex with
        | Some name ->
            { model with name = name }
        | None ->
            let nationalOrigin, name = makeNameAnyNation model.sex
            { model with name = name; nationalOrigin = nationalOrigin }

open Elmish
open Elmish.React

type Props = {
    export: (RoleplayingData -> unit)
    args: unit
    }

// the existence of props.export is probably a sign that this is an abuse of useElmish: it means state isn't really local.
// probably any state that would need to be exported should instead be mastered externally and passed in on props.
// but the question is: how do we organize the messages and updates?
[<ReactComponent>]
let View (model:Model) dispatch =
    class' "characterHeader" Html.div [
        let char = model
        class' "title" Html.div [
            Html.text $"{char.name}"
            ]
        let line (txt:string) = Html.div [prop.text txt]
        match char.nationalOrigin with
        | "" ->
            line $"{char.sex}"
        | place ->
            line $"{char.sex} from {place}"
        Html.button [prop.text "New name"; prop.onClick (thunk1 dispatch RecomputeName)]
        ]
