namespace Chargen

// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Domain
    open Model.Character
    type Rolls = int list

    type Draft = {
        allocations: (int * Stat option) list
        originalRolls: Rolls
        }

    let addUpStats (allocations: (int * Stat option) list) =
        allocations
            |> List.choose (function (roll, Some stat) -> Some(roll, stat) | _ -> None)
            |> List.groupBy snd
            |> List.map (fun (stat, lst) -> stat, lst |> List.map fst |> List.fold (+) 0)
            |> Map.ofList

    let ofDraft (draft:Draft) : Domain.Character option =
        match draft.allocations |> addUpStats with
        | Lookup Str str & Lookup Dex dex & Lookup Con con
            & Lookup Int int & Lookup Wis wis & Lookup Cha cha
            ->
            {
                Character.name = "Bob"
                Character.Str = str
                Character.Dex = dex
                Character.Con = con
                Character.Int = int
                Character.Wis = wis
                Character.Cha = cha
                originalRolls = rolls
                } |> Some
        | _ ->
            None
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
    let roll3d6InOrder assign =
        assign [for _ in 1..6 do
                    List.init 3 (fun _ -> d 6) |> List.sum
                    ]
        |> inOrder
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

    type ChargenMethod =
        | Roll3d6InOrder
        | Roll4d6k3
        | DarkSunMethodI
        | DarkSun6d4
        with
        static member All = [Roll3d6InOrder;Roll4d6k3;DarkSunMethodI;DarkSun6d4]
        member this.info =
            match this with
                | Roll3d6InOrder -> "3d6 in order", roll3d6InOrder
                | Roll4d6k3 -> "4d6 drop lowest", roll4d6k3
                | DarkSunMethodI -> "Dark Sun Method I", darkSunMethodI
                | DarkSun6d4 -> "Dark Sun 6d4 drop lowest", darkSun6d4
            |> MethodInfo
    and MethodInfo = MethodInfo of name: string * ((Rolls -> Draft) -> Draft)
        with
        member this.f = match this with (MethodInfo(name, f)) -> f
        member this.name' = match this with (MethodInfo(name, f)) -> name

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
            method = ChargenMethod.All.Head
            },
            Cmd.ofMsg Reroll
    let update finish msg model =
        match msg with
        | Cancel -> model, (finish None)
        | Done char -> model, (Some char |> finish)
        | Reroll ->
            let char = create model.method.info.f
            { model with draft = Some char; export = ofDraft char }, Cmd.Empty
        | SetMethod m ->
            { model with method = m }, Cmd.Empty

    open Feliz
    open Konva
    let getPercentile =
        // for a given stat like 18, how many people have a lower stat?
        let normalPersonDistribution =
            [
                for x in 1..6 do
                    for y in 1..6 do
                        for z in 1..6 do
                            x+y+z
                ]
        let lessThanEqualGroups =
            [
                for x in 1..25 do
                    let count = normalPersonDistribution |> Seq.filter (fun stat -> stat < x) |> Seq.length
                    x, (float count/(float normalPersonDistribution.Length))
                ] |> dict
        fun statValue -> lessThanEqualGroups[statValue]

    let view (model: Model) dispatch =
        Html.div [
            Html.text "Create a character!"
            stage [
                Stage.height 300
                Stage.width 500
                Stage.children [
                    Layer.create [
                        rect [
                            Rect.height 300
                            Rect.width 500
                            Rect.stroke Black
                            Rect.strokeWidth 3
                            ]
                        let mutable y = 10
                        let t messages =
                            [
                                for x, txt in messages do
                                    text [Text.x (x+10); Text.y y; Text.fontSize 18; Text.text txt]
                                y <- y + 20
                                ]
                            |> React.fragment
                        let describe stat statValue =
                            let term =
                                match stat with
                                | "Str" -> "Stronger"
                                | "Dex" -> "Faster"
                                | "Con" -> "Tougher"
                                | "Int" -> "Smarter"
                                | "Wis" -> "Wiser"
                                | _ -> "More charismatic"
                            t [0, $"{stat} {statValue}"; 100, $"{term} than %0.1f{(getPercentile statValue)*100.}%% of humanity"]
                        match model.export with
                        | Some char ->
                            describe "Str" char.Str
                            describe "Dex" char.Dex
                            describe "Con" char.Con
                            describe "Int" char.Int
                            describe "Wis" char.Wis
                            describe "Cha" char.Cha
                        | None ->
                            match model.draft with
                            |Some draft ->
                                t [0,$"[draft] rolls{draft.originalRolls}"]
                            | None -> ()
                        ]
                    ]
                ]
            Html.button [
                prop.text "Reroll"
                prop.onClick (fun _ -> dispatch Reroll)
                ]
            Html.div [
                for ix, method in ChargenMethod.All |> List.mapi (fun i x -> i, x) do
                    Html.div [
                        Html.input [prop.type'.radio; prop.ariaChecked (model.method = method); prop.isChecked (model.method = method); prop.id method.info.name'; prop.onClick (fun _ -> method |> SetMethod |> dispatch)]
                        Html.label [prop.for' method.info.name'; prop.text method.info.name']
                        ]
                ]
            ]


