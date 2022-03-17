namespace Chargen
// This should probably go in its own file but for now it's here.
// It's the thing this UI is intending to eventually produce.
module Domain =
    type CharacterSheet = {
        Str: int
        Dex: int
        Con: int
        Int: int
        Wis: int
        Cha: int
        originalRolls: int list
        }

// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Domain
    type Rolls = int list

    type Draft = {
        Str: int option
        Dex: int option
        Con: int option
        Int: int option
        Wis: int option
        Cha: int option
        originalRolls: Rolls
        unallocated: Rolls
        }


    let ofDraft (draft:Draft) : CharacterSheet option =
        match draft with
        | {
            Str = Some str
            Dex = Some dex
            Con = Some con
            Int = Some int
            Wis = Some wis
            Cha = Some cha
            originalRolls = rolls
            }
            ->
            {
                Str = str
                Dex = dex
                Con = con
                Int = int
                Wis = wis
                Cha = cha
                originalRolls = rolls
                } |> Some
        | _ ->
            None
    let rand = System.Random()
    let d n = 1 + rand.Next n
    let inOrder (draft:Draft) =
        let r n = draft.originalRolls[n] |> Some
        {
          draft with
            Str = r 0
            Dex = r 1
            Con = r 2
            Int = r 3
            Wis = r 4
            Cha = r 5
            unallocated = []
            }
    let roll4d6k3 assign =
        assign [for _ in 1..6 do
                    List.init 4 (fun _ -> d 6) |> List.sortDescending |> List.take 3 |> List.sum
                    ]
    let darkSunMethodI assign =
        assign [
            for _ in 1..6 do
                4 + (List.init 4 (fun _ -> 1 + rand.Next(4)) |> List.sum)
            ]
        |> inOrder
    let darkSun6d4 assign =
        assign [
            for _ in 1..6 do
                List.init 6 (fun _ -> 1 + rand.Next(4)) |> List.sortDescending |> List.take 5 |> List.sum
            ]
    let create method : Draft =
        method(fun rolls -> {
                Str = None
                Dex = None
                Con = None
                Int = None
                Wis = None
                Cha = None
                originalRolls = rolls
                unallocated = rolls
                })

module View =
    open Interaction
    open Elmish

    type ChargenMethod = Method of name: string * ((Rolls -> Draft) -> Draft)
        with
        member this.Equals(Method(rhs, _)) =
            let (Method(lhs, _)) = this
            lhs = rhs
    let methods =
        [
            "4d6 drop lowest", roll4d6k3
            "Dark Sun Method I", darkSunMethodI
            "Dark Sun 6d4 drop lowest", darkSun6d4
            ]
        |> List.map Method
    type Model = {
        draft: Draft option
        export: Domain.CharacterSheet option
        method: ChargenMethod
        }
    type Msg =
        | Done of Domain.CharacterSheet
        | Cancel
        | Reroll
        | SetMethod of ChargenMethod
    let init _ =
        {
            draft = None
            export = None
            method = methods.Head
            },
            Cmd.ofMsg Reroll
    let update finish msg model =
        match msg with
        | Cancel -> model, (finish None)
        | Done char -> model, (Some char |> finish)
        | Reroll ->
            match model.method with
            | Method(text, f) ->
                let m = create f
                { model with draft = Some m; export = ofDraft m }, Cmd.Empty
        | SetMethod m ->
            { model with method = m }, Cmd.Empty

    open Feliz
    open Konva
    let view (model: Model) dispatch =
        Html.div [
            Html.text "Create a character!"
            stage [
                Stage.height 300
                Stage.width 400
                Stage.children [
                    Layer.create [
                        rect [
                            Rect.height 300
                            Rect.width 400
                            ]
                        match model.export with
                        | Some char ->
                            text [Text.x 10; Text.y 150; Text.text $"Str {char.Str} / Dex { char.Dex } / Con { char.Con } / Int { char.Int} / Wis { char.Wis} / Cha { char.Cha } "]
                        | None ->
                            match model.draft with
                            |Some draft ->
                                text [Text.x 10; Text.y 150; Text.text $"[draft] rolls{draft.originalRolls}"]
                            | None -> ()
                        ]
                    ]
                ]
            Html.button [
                prop.text "Reroll"
                prop.onClick (fun _ -> dispatch Reroll)
                ]
            Html.form [
                for Method(txt, _) as method in methods do
                    Html.input [prop.type'.radio; prop.ariaChecked (model.method.Equals method); prop.isChecked (model.method.Equals method); prop.id txt; prop.onClick (fun _ -> method |> SetMethod |> dispatch)]
                    Html.label [prop.for' txt; prop.text txt]
                    ]
            ]

