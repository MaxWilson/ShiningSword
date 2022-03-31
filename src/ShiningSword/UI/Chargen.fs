namespace UI.Chargen

// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Domain
    open DerivedTraits
    open Domain.Character
    open Domain.Character.DND5e

    type Rolls = int list

    type RuleSet = ADND | DND5e
    type Mode = CumulativeFrom of min:int * max:int | Assign | InOrder
    type Draft = {
        name: string
        nationalOrigin: string
        sex: Sex
        allocations: (int * Stat option) list
        originalRolls: Rolls
        ruleset: RuleSet
        mode: Mode
        traits: DerivationInstance<Trait>
        }

    let addUpStats (statMods: (Stat * int) list) (allocations: (int * Stat option) list) =
        let rawTotals =
            allocations
            |> List.choose (function (roll, Some stat) -> Some(roll, stat) | _ -> None)
            |> List.groupBy snd
            |> List.map (fun (stat, lst) -> stat, lst |> List.map fst |> List.fold (+) 0)
            |> Map.ofList
        let addStat n = function
            | Some current -> current + n |> Some
            | None -> Some n
        statMods |> List.fold (fun map (stat, n) -> map |> Map.change stat (addStat n)) rawTotals

    let ofDraft (draft:Draft) : CharacterSheet option =
        let statModsOnly(_, _, _, decisions) =
            match decisions |> List.choose (function StatMod(stat, n) -> Some(stat, n) | _ -> None) with
            | [] -> None
            | mods -> Some mods
        let statMods = summarize statModsOnly DND5e.rules draft.traits [PC] |> List.collect id
        match draft.allocations |> addUpStats statMods with
        | Lookup Str str & Lookup Dex dex & Lookup Con con
            & Lookup Int int & Lookup Wis wis & Lookup Cha cha
            ->
            {
                CharacterSheet.name = draft.name
                nationalOrigin = draft.nationalOrigin
                Str = str
                Dex = dex
                Con = con
                Int = int
                Wis = wis
                Cha = cha
                sex = draft.sex
                traits = Map.empty |> toSetting Set.ofList rules [PC]
                originalRolls = draft.originalRolls
                } |> Some
        | _ ->
            None
    let d = rand
    let inOrder (draft:Draft) =
        match draft.allocations with
        | [str,_;dex,_;con,_;int,_;wis,_;cha,_] ->
            let mapSnd = List.map (fun (arg, stat) -> arg, Some stat)
            { draft with allocations = [str,Str;dex,Dex;con,Con;int,Int;wis,Wis;cha,Cha] |> mapSnd; mode = InOrder }
        | _ ->
            draft
    let roll3d6InOrder assign =
        assign [for _ in 1..6 do
                    List.init 3 (thunk1 d 6) |> List.sum
                    ]
        |> inOrder
    let roll4d6k3 assign =
        assign [for _ in 1..6 do
                    List.init 4 (thunk1 d 6) |> List.sortDescending |> List.take 3 |> List.sum
                    ]
    let rollPHBMethodVI assign =
        {
            assign [
                for _ in 1..7 do
                    d 6
                ]
            with mode = CumulativeFrom (8, 18)
        }
    let darkSunMethodI assign =
        assign [
            for _ in 1..6 do
                4 + (List.init 4 (thunk1 d 4) |> List.sum)
            ]
        |> inOrder
    let darkSun6d4 assign =
        assign [
            for _ in 1..6 do
                List.init 6 (thunk1 d 4) |> List.sortDescending |> List.take 5 |> List.sum
            ]
    let darkSunMethodV assign =
        {
            assign [
                for _ in 1..10 do
                    d 4
                ]
            with mode = CumulativeFrom (10, 20)
        }
    let unassign (stat:Stat) (draft:Draft) =
        { draft with allocations = draft.allocations |> List.map (function (ix, Some stat') when stat' = stat -> ix, None | same -> same) }
    let assign (rollIndex:int) (stat:Stat) (draft:Draft): Draft =
        let replaceStat i ((value, currentStat) as unchanged) =
            if i = rollIndex then
                (value, Some stat)
            else
                match currentStat with
                | Some s when draft.mode = Assign && stat = s ->
                    value, None
                | _ -> unchanged
        let enforceMaximum allocations =
            match draft.mode with
            | Assign | InOrder -> allocations
            | CumulativeFrom(min', max') ->
                let rec recur totalSoFar ix = function
                | [] -> []
                | unchanged::rest when ix = rollIndex ->
                    // we already accounted for this stat when totalSoFar was initialized,
                    // and for UX reasons we will not un-assign the roll you just dropped.
                    unchanged::(recur (totalSoFar) (ix+1) rest)
                | ((value, Some stat') as unchanged)::rest when stat' = stat ->
                    if totalSoFar + value <= max' then
                        unchanged::(recur (totalSoFar + value) (ix+1) rest)
                    else
                        // this is the key case: un-assign this stat
                        (value, None)::(recur totalSoFar (ix+1) rest)
                | unchanged::rest ->
                    unchanged::(recur totalSoFar (ix+1) rest)
                recur (min' + (allocations[rollIndex] |> fst)) 0 allocations
        { draft with allocations = draft.allocations |> List.mapi replaceStat |> enforceMaximum }

    let create method : Draft =
        let sex = chooseRandom [Male; Female]
        let nationalOrigin, name = makeName sex
        method(fun rolls -> {
                name = name
                sex = sex
                nationalOrigin = nationalOrigin
                originalRolls = rolls
                allocations = rolls |> List.map (fun x -> x, None)
                mode = Assign
                ruleset = DND5e
                traits = Map.empty
                })

open Interaction

module View =
    open Interaction
    open Elmish
    open Domain.Character
    open DND5e
    open Feliz
    open Feliz.UseElmish
    open DerivedTraits

    type ChargenMethod =
        | Roll3d6InOrder
        | Roll4d6k3
        | RollPHBMethodVI
        | DarkSunMethodI
        | DarkSun6d4
        | DarkSunMethodV
        with
        static member All = [Roll3d6InOrder;Roll4d6k3;RollPHBMethodVI;DarkSunMethodI;DarkSun6d4;DarkSunMethodV]
        member this.info =
            match this with
                | Roll3d6InOrder -> "3d6 in order", roll3d6InOrder
                | Roll4d6k3 -> "4d6 drop lowest", roll4d6k3
                | RollPHBMethodVI -> "7d6, assign to taste", rollPHBMethodVI
                | DarkSunMethodI -> "Dark Sun default", darkSunMethodI
                | DarkSun6d4 -> "Dark Sun 6d4 drop lowest", darkSun6d4
                | DarkSunMethodV -> "Dark Sun Method V", darkSunMethodV
            |> MethodInfo
    and MethodInfo = MethodInfo of name: string * ((Rolls -> Draft) -> Draft)
        with
        member this.f = match this with (MethodInfo(name, f)) -> f
        member this.name' = match this with (MethodInfo(name, f)) -> name

    type Model = {
        draft: Draft option
        export: CharacterSheet option
        method: ChargenMethod
        editMode: TextEditMode
        }
    and TextEditMode = | NotEditingText | EditingName // "not editing" is a bit of a misnomer--you can still edit stats and choices, but they aren't text
    type Msg =
        | Done of CharacterSheet
        | Cancel
        | Reroll
        | SetMethod of ChargenMethod
        | AssignRoll of ix:int * stat:Stat
        | UnassignRolls of stat: Stat
        | SetName of string
        | SetSex of Sex
        | ChooseTrait of head:Trait * choiceIx: int * decisionIx: int
        | SetEditMode of TextEditMode
    let init _ =
        {
            draft = None
            export = None
            method = ChargenMethod.All.Head
            editMode = NotEditingText
            },
            Cmd.ofMsg Reroll
    let update cmd finish msg model =
        match msg with
        | Cancel -> model, (finish None)
        | Done char -> model, (Some char |> finish)
        | Reroll ->
            let char = create model.method.info.f
            { model with draft = Some char; export = None }, Cmd.Empty
        | SetMethod m ->
            { model with method = m }, cmd Reroll
        | AssignRoll(ix, stat) ->
            { model with draft = model.draft |> Option.map (assign ix stat) }, Cmd.Empty
        | UnassignRolls stat ->
            { model with draft = model.draft |> Option.map (unassign stat) }, Cmd.Empty
        | SetName(name) ->
            { model with draft = model.draft |> Option.map (fun draft -> { draft with name = name; nationalOrigin = "" }) }, Cmd.Empty
        | SetEditMode mode ->
            { model with editMode = mode }, Cmd.Empty
        | SetSex sex ->
            let setSex (draft: Draft) =
                let nationalOrigin, name = makeName sex
                { draft with sex = sex; name = name; nationalOrigin = nationalOrigin }
            { model with draft = model.draft |> Option.map setSex }, Cmd.Empty
        | ChooseTrait(head, choiceIx, decisionIx) ->
            let choose (draft:Draft) =
                let rules = Domain.Character.DND5e.rules // todo: make this dynamic somehow, which entails making it able to use either kind of traits as head
                let chooseTrait (instance: DerivationInstance<Trait>) =
                    let rule =
                        match rules with
                        | Lookup head rule ->
                            rule[choiceIx]
                        | _ -> shouldntHappen()
                    instance |> Map.change head (function
                        | None -> Map.ofList [choiceIx, [decisionIx]] |> Some
                        | Some decisions ->
                            let change = function
                            | Some ixs ->
                                let d = match decisionIx::ixs with | ixs when rule.mustBeDistinct -> List.distinct ixs | ixs -> ixs
                                let d = if rule.numberAllowed >= d.Length then d else d |> List.take rule.numberAllowed
                                d |> Some
                            | None -> [decisionIx] |> Some
                            decisions |> Map.change choiceIx change |> Some
                        )

                { draft with traits = draft.traits |> chooseTrait }
            { model with draft = model.draft |> Option.map choose }, Cmd.Empty


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
                    let count = normalPersonDistribution |> Seq.filter (fun stat -> stat <= x) |> Seq.length
                    x, (float count/(float normalPersonDistribution.Length))
                ] |> dict
        fun statValue ->
            if statValue > lessThanEqualGroups.Count then 1.
            else lessThanEqualGroups[statValue]

    open Fable.Core.JsInterop
    open Fable.React
    open Feliz
    open UI
    open UI.Konva
    let ddprop = DragDrop.prop

    [<ReactComponent>]
    let view model dispatch =
        let window = Browser.Dom.window
        let width = (int window.innerWidth - 80)
        let mutable id = 0
        let key() =
            id <- id + 1
            Shape.key id
        // helper method to make assigning css classes more concise
        let class' f className children =
            f [
                prop.className (className: string)
                prop.children (children: _ list)
                ]
        class' Html.div "charGen" [
            class' Html.div "Title" [
                Html.text "Create a character!"
                ]
            let currentStat stat statValue =
                match statValue with
                | Some statValue ->
                    DragDrop.target [
                        ddprop.targetKey "stats"
                        ddprop.dropData stat
                        ddprop.children [
                            Html.span [prop.text $"{stat} {statValue}"; prop.onClick(fun _ -> dispatch (UnassignRolls stat))]
                            ]
                        ]
                | None ->
                    DragDrop.target [
                        ddprop.targetKey "stats"
                        ddprop.dropData stat
                        ddprop.children [
                            Html.span [prop.text $"{stat}     "]
                            ]
                        ]

            let describe (stat:Stat) statValue =
                let term =
                    match stat with
                    | Str -> "Stronger"
                    | Dex -> "Faster"
                    | Con -> "Tougher"
                    | Int -> "Smarter"
                    | Wis -> "Wiser"
                    | Cha -> "More charismatic"
                React.fragment [
                    currentStat stat (Some statValue)
                    DragDrop.target [
                        ddprop.targetKey "stats"
                        ddprop.dropData stat
                        ddprop.children [
                            Html.span [prop.text $"{term} than %0.1f{(getPercentile statValue)*100.}%% of humanity"]
                            ]
                        ]
                    ]
            match model.export with
            | Some char ->
                class' Html.div "middle" [
                    class' Html.div "characterHeader" [
                        Html.text $"{char.name} from {char.nationalOrigin} ({char.sex})"
                        ]
                    class' Html.div "assignedStats" [
                        describe Str char.Str
                        describe Dex char.Dex
                        describe Con char.Con
                        describe Int char.Int
                        describe Wis char.Wis
                        describe Cha char.Cha
                        ]
                    ]
            | None ->
                match model.draft with
                |Some draft ->
                    class' Html.div "middle" [
                        class' Html.div "characterHeader" [
                            class' Html.div "title" [
                                match model.editMode with
                                | NotEditingText ->
                                    Html.span [
                                        prop.text draft.name;
                                        prop.onClick (thunk1 dispatch (SetEditMode EditingName))
                                        ]
                                | EditingName ->
                                    Html.input [
                                        prop.value draft.name;
                                        prop.onChange (fun (txt:string) -> SetName txt |> dispatch)
                                        prop.onKeyDown (fun ev -> if ev.code = "Enter" then SetEditMode NotEditingText |> dispatch); prop.onBlur (fun ev -> SetEditMode NotEditingText |> dispatch)
                                        ]

                                if draft.nationalOrigin <> "" then
                                    Html.text $" from {draft.nationalOrigin}"
                                ]
                            class' Html.div "details" [
                                for sex in [Male; Female] do
                                    let id = sex.ToString()
                                    Html.input [prop.type'.radio; prop.ariaChecked (draft.sex = sex); prop.isChecked (draft.sex = sex); prop.id id; prop.onClick (fun _ -> SetSex sex |> dispatch); prop.readOnly true]
                                    Html.label [prop.htmlFor id; prop.text id]
                                ]
                            ]
                        class' Html.div "assignedStats" [
                            let statModsOnly(_, _, _, decisions) =
                                match decisions |> List.choose (function StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                                | [] -> None
                                | mods -> Some mods
                            let statMods =
                                match draft.mode with
                                | CumulativeFrom(min, _) -> Stat.All |> List.map (fun stat -> stat, min)
                                | _ -> []
                                @
                                (summarize statModsOnly DND5e.rules draft.traits [PC]
                                |> List.collect (fun x -> x))
                            let assignments = addUpStats statMods draft.allocations
                            for stat in Stat.All do
                                match assignments |> Map.tryFind stat with
                                | Some v -> describe stat v
                                | None ->
                                    currentStat stat None
                                    Html.span []
                            ]
                        Html.div [
                            // try to keep layout height consistent--I'm sure there's a better way
                            if draft.mode = InOrder then prop.classes ["statRolls";"hide"] else prop.className "statRolls"
                            prop.children [
                                Html.span [prop.text "Unassigned (drag and drop)"; prop.className "label"]
                                for ix, (roll, stat) in draft.allocations |> List.mapi tuple2 do
                                    match stat with
                                    | None ->
                                        Html.span [
                                            prop.className "roll"
                                            prop.children [
                                                DragDrop.container [
                                                    ddprop.targetKey "stats"
                                                    ddprop.onDrop(fun e ->
                                                        match e.dropData with
                                                        | Some(:? Stat as stat) ->
                                                            AssignRoll(ix, stat) |> dispatch
                                                        | _ -> ())
                                                    ddprop.children [
                                                        Html.text roll
                                                        ]
                                                    ]
                                                ]
                                            ]
                                    | Some _ ->
                                        class' Html.span "roll" [Html.div [prop.text "0"; prop.className "hide"]]
                                ]
                            ]
                        ]
                | None -> ()
            class' Html.div "footer" [
                Html.button [
                    prop.text "Reroll"
                    prop.onClick (fun _ -> dispatch Reroll)
                    ]

                for ix, method in ChargenMethod.All |> List.mapi tuple2 do
                    Html.div [
                        Html.input [prop.type'.radio; prop.ariaChecked (model.method = method); prop.isChecked (model.method = method); prop.id method.info.name'; prop.onClick (fun _ -> method |> SetMethod |> dispatch); prop.readOnly true]
                        Html.label [prop.htmlFor method.info.name'; prop.text method.info.name']
                        ]

                match model.draft with
                | None -> ()
                | Some draft ->
                    let describeChoiceInReact (head, choiceIx, choice: DerivedTraits.Choice<_>, decision: Trait list) =
                        let toString x = x.ToString()
                        if choice.options.Length = decision.Length then
                            Html.div [
                                class' Html.div "choice" [
                                    for ix, option in choice.options |> List.mapi tuple2 do
                                        let name = option |> DND5e.describe
                                        Html.span [
                                            Html.text name
                                            ]
                                    ]
                                ]
                            |> Some
                        else
                            Html.div [
                                class' Html.div "choice" [
                                    for ix, option in choice.options |> List.mapi tuple2 do
                                        let name = option |> DND5e.describe
                                        Html.input [prop.type'.checkbox; prop.ariaChecked (decision |> List.contains option); prop.isChecked (decision |> List.contains option); prop.id name; prop.onClick (fun _ -> ChooseTrait(head, choiceIx, ix) |> dispatch); prop.readOnly true]
                                        Html.label [prop.htmlFor name; prop.text name]
                                    ]
                                ]
                            |> Some

                    yield! summarize describeChoiceInReact DND5e.rules draft.traits [PC]
                ]
            ]
