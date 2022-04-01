namespace UI.Chargen

// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Domain
    open DerivedTraits
    open Domain.Character
    open Domain.Character.DND5e

    type Rolls = int array

    type Ruleset = ADND | DND5e
    type Traits = ADND of ADND2nd.Trait DerivationInstance | DND5e of DND5e.Trait DerivationInstance
        with
        member this.map (f: (ADND2nd.Trait DerivationInstance -> ADND2nd.Trait DerivationInstance)) =
            match this with
            | ADND instance -> ADND (f instance)
            | unchanged -> unchanged
        member this.map (f: (DND5e.Trait DerivationInstance -> DND5e.Trait DerivationInstance)) =
            match this with
            | DND5e instance -> DND5e (f instance)
            | unchanged -> unchanged

    type Mode = CumulativeFrom of min:int * max:int | Assign | InOrder | PointBuy
    type Draft = {
        name: string
        nationalOrigin: string
        sex: Sex
        allocations: (int * Stat option) array
        exceptionalStrength: int option
        originalRolls: Rolls
        mode: Mode
        traits: Traits
        }

    let addUpStats (statMods: (Stat * int) list) (allocations: (int * Stat option) array) =
        let rawTotals =
            allocations
            |> Array.choose (function (roll, Some stat) -> Some(roll, stat) | _ -> None)
            |> Array.groupBy snd
            |> Array.map (fun (stat, lst) -> stat, lst |> Array.map fst |> Array.fold (+) 0)
            |> Map.ofArray
        let addStat n = function
            | Some current -> current + n |> Some
            | None -> Some n
        statMods |> List.fold (fun map (stat, n) -> map |> Map.change stat (addStat n)) rawTotals

    let (|CharacterSheetADND2nd|_|) (draft:Draft) : CharacterSheet option =
        match draft.traits with
        | DND5e(traits) ->
            let statModsOnly(_, _, _, decisions) =
                match decisions |> List.choose (function StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                | [] -> None
                | mods -> Some mods
            let statMods = summarize statModsOnly DND5e.rules traits [DND5e.Trait.PC] |> List.collect id
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
        | _ -> None
    let (|CharacterSheet5E|_|) (draft:Draft) : CharacterSheet option =
        match draft.traits with
        | DND5e(traits) ->
            let statModsOnly(_, _, _, decisions) =
                match decisions |> List.choose (function StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                | [] -> None
                | mods -> Some mods
            let statMods = summarize statModsOnly DND5e.rules traits [DND5e.Trait.PC] |> List.collect id
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
        | _ -> None


    let d = rand
    let inOrder (draft:Draft) =
        match draft.allocations with
        | [|str,_;dex,_;con,_;int,_;wis,_;cha,_|] ->
            let mapSnd = Array.map (fun (arg, stat) -> arg, Some stat)
            { draft with allocations = [|str,Str;dex,Dex;con,Con;int,Int;wis,Wis;cha,Cha|] |> mapSnd; mode = InOrder }
        | _ ->
            draft
    let roll3d6InOrder assign =
        assign [|for _ in 1..6 do
                    List.init 3 (thunk1 d 6) |> List.sum
                    |]
        |> inOrder
    let roll4d6k3 assign =
        assign [|for _ in 1..6 do
                    List.init 4 (thunk1 d 6) |> List.sortDescending |> List.take 3 |> List.sum
                    |]
    let rollPHBMethodVI assign =
        {
            assign [|
                for _ in 1..7 do
                    d 6
                |]
            with mode = CumulativeFrom (8, 18)
        }
    let darkSunMethodI assign =
        assign [|
            for _ in 1..6 do
                4 + (List.init 4 (thunk1 d 4) |> List.sum)
            |]
        |> inOrder
    let darkSun6d4 assign =
        assign [|
            for _ in 1..6 do
                List.init 6 (thunk1 d 4) |> List.sortDescending |> List.take 5 |> List.sum
            |]
    let darkSunMethodV assign =
        {
            assign [|
                for _ in 1..10 do
                    d 4
                |]
            with mode = CumulativeFrom (10, 20)
        }
    let pointBuy points assign =
        let blank = assign [|points|]
        {
            // kind of a hack to make the UI "faster than..." etc. work
            blank with mode = PointBuy; allocations = blank.allocations |> Array.append [|for stat in Stat.All -> 8, Some stat|]
        }
    let unassign (stat:Stat) (draft:Draft) =
        { draft with allocations = draft.allocations |> Array.map (function (ix, Some stat') when stat' = stat -> ix, None | same -> same) }
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
            | PointBuy -> shouldntHappen() // PointBuy should not use AssignRoll message, should use ChangePointAllocation instead
        { draft with allocations = draft.allocations |> Array.mapi replaceStat |> List.ofArray |> enforceMaximum |> Array.ofList }
    let changePointAllocation stat amount draft : Draft =
        match draft.mode with
        | PointBuy ->
            let (|For|_|) (targetStat: Stat option) (allocs: (int * Stat option) array) =
                allocs |> Array.tryPick (function (amt, target) when target = targetStat -> Some amt | _ -> None)

            // we won't touch other allocations
            let otherAllocations = draft.allocations |> Array.filter (function (n, Some otherStat) when stat <> otherStat -> true | _ -> false)
            let between min' max' x =
                min' <= x && x <= max'
            let allocate pointsLeft currentBonus =
                let costOf = function
                | 8 -> 0
                | 9 -> 1
                | 10 -> 2
                | 11 -> 3
                | 12 -> 4
                | 13 -> 5
                | 14 -> 7
                | 15 -> 9
                | _ -> shouldntHappen()
                let deltaCost = (costOf (currentBonus + amount)) - costOf currentBonus
                match pointsLeft - deltaCost with
                | pointsLeft when pointsLeft >= 0 ->
                    let newValues = [|pointsLeft, None; currentBonus + amount, Some stat|]
                    let lst = newValues |> Array.append otherAllocations
                    { draft with allocations = lst }
                | _ -> draft
            match draft.allocations with
            | For None pointsLeft & For (Some stat) currentValue when (currentValue + amount) |> between 8 15 ->
                allocate pointsLeft currentValue
            | For None pointsLeft & For (Some stat) currentValue -> draft // unchanged
            | For None pointsLeft when amount |> between 8 15 ->
                allocate pointsLeft amount
            | _ -> draft // unchanged
        | _ -> shouldntHappen()
    let create ruleset method : Draft =
        let sex = chooseRandom [Male; Female]
        let nationalOrigin, name = makeName sex
        method(fun rolls -> {
                name = name
                sex = sex
                nationalOrigin = nationalOrigin
                originalRolls = rolls
                allocations = rolls |> Array.map (fun x -> x, None)
                mode = Assign
                exceptionalStrength = match ruleset with Ruleset.ADND -> Some (rand 100) | _ -> None
                traits = match ruleset with Ruleset.ADND -> ADND Map.empty | _ -> DND5e Map.empty
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
        | PointBuyN of int
        with
        static member ADND = [Roll3d6InOrder;Roll4d6k3;RollPHBMethodVI;DarkSunMethodI;DarkSun6d4;DarkSunMethodV]
        static member DND5e = [Roll3d6InOrder;Roll4d6k3;PointBuyN 27; PointBuyN 31]
        member this.info =
            match this with
                | Roll3d6InOrder -> "3d6 in order", roll3d6InOrder
                | Roll4d6k3 -> "4d6 drop lowest", roll4d6k3
                | RollPHBMethodVI -> "7d6, assign to taste", rollPHBMethodVI
                | DarkSunMethodI -> "Dark Sun default", darkSunMethodI
                | DarkSun6d4 -> "Dark Sun 6d4 drop lowest", darkSun6d4
                | DarkSunMethodV -> "Dark Sun Method V", darkSunMethodV
                | PointBuyN n -> $"Point buy ({n})", pointBuy n
            |> MethodInfo
    and MethodInfo = MethodInfo of name: string * ((Rolls -> Draft) -> Draft)
        with
        member this.f = match this with (MethodInfo(name, f)) -> f
        member this.name' = match this with (MethodInfo(name, f)) -> name

    type ParentMsg =
        | Complete of CharacterSheet
        | Cancel
        | NavigateTo of Ruleset
    type Model = {
        draft: Draft option
        export: CharacterSheet option
        method: ChargenMethod
        editMode: TextEditMode
        ruleset: Ruleset
        }
    and TextEditMode = | NotEditingText | EditingName // "not editing" is a bit of a misnomer--you can still edit stats and choices, but they aren't text
    type Msg =
        | Done of CharacterSheet
        | Cancel
        | Reroll
        | SetMethod of ChargenMethod
        | AssignRoll of ix:int * stat:Stat
        | UnassignRolls of stat: Stat
        | ChangePointAllocation of stat: Stat * amount: int
        | SetName of string
        | SetSex of Sex
        | ToggleADNDTrait of head:ADND2nd.Trait * choiceIx: int * decisionIx: int
        | Toggle5ETrait of head:DND5e.Trait * choiceIx: int * decisionIx: int
        | SetEditMode of TextEditMode
        | SetRuleset of Ruleset
    let init _ =
        {
            draft = None
            export = None
            method = ChargenMethod.ADND.Head
            editMode = NotEditingText
            ruleset = Ruleset.ADND
            },
            Cmd.ofMsg Reroll
    let update cmd informParent msg model =
        match msg with
        | Cancel -> model, (informParent ParentMsg.Cancel)
        | Done char -> model, (Complete char |> informParent)
        | Reroll ->
            let char = create model.ruleset model.method.info.f
            { model with draft = Some char; export = None }, Cmd.Empty
        | SetMethod m ->
            { model with method = m }, cmd Reroll
        | AssignRoll(ix, stat) ->
            { model with draft = model.draft |> Option.map (assign ix stat) }, Cmd.Empty
        | UnassignRolls stat ->
            { model with draft = model.draft |> Option.map (unassign stat) }, Cmd.Empty
        | ChangePointAllocation(stat, plusOrMinus) ->
            let draft' = model.draft |> Option.map (changePointAllocation stat plusOrMinus)
            printfn "%A" draft'.Value.allocations
            { model with draft = draft' }, Cmd.Empty
        | SetRuleset ruleset ->
            let msg = SetMethod (if ruleset = Ruleset.ADND then ChargenMethod.ADND.Head else ChargenMethod.DND5e.Head)
            { model with ruleset = ruleset }, Cmd.batch [msg |> cmd;informParent (NavigateTo ruleset)]
        | SetName(name) ->
            { model with draft = model.draft |> Option.map (fun draft -> { draft with name = name; nationalOrigin = "" }) }, Cmd.Empty
        | SetEditMode mode ->
            { model with editMode = mode }, Cmd.Empty
        | SetSex sex ->
            let setSex (draft: Draft) =
                let nationalOrigin, name = makeName sex
                { draft with sex = sex; name = name; nationalOrigin = nationalOrigin }
            { model with draft = model.draft |> Option.map setSex }, Cmd.Empty
        | Toggle5ETrait(head, choiceIx, decisionIx) ->
            let toggle (draft:Draft) =
                let rules = Domain.Character.DND5e.rules
                let toggleTrait (instance: DerivationInstance<Trait>) =
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
                                let d =
                                    if ixs |> List.contains decisionIx then ixs |> List.filter ((<>) decisionIx)
                                    else
                                        match decisionIx::ixs with | ixs when rule.mustBeDistinct -> List.distinct ixs | ixs -> ixs
                                if rule.numberAllowed >= d.Length then d else d |> List.take rule.numberAllowed
                                |> Some
                            | None -> [decisionIx] |> Some
                            decisions |> Map.change choiceIx change |> Some
                        )
                { draft with traits = draft.traits.map toggleTrait }
            { model with draft = model.draft |> Option.map toggle }, Cmd.Empty
        | ToggleADNDTrait(head, choiceIx, decisionIx) ->
            let toggle (draft:Draft) =
                let rules = Domain.Character.ADND2nd.rules
                let toggleTrait (instance: DerivationInstance<_>) =
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
                                let d =
                                    if ixs |> List.contains decisionIx then ixs |> List.filter ((<>) decisionIx)
                                    else
                                        match decisionIx::ixs with | ixs when rule.mustBeDistinct -> List.distinct ixs | ixs -> ixs
                                if rule.numberAllowed >= d.Length then d else d |> List.take rule.numberAllowed
                                |> Some
                            | None -> [decisionIx] |> Some
                            decisions |> Map.change choiceIx change |> Some
                        )
                { draft with traits = draft.traits.map toggleTrait }
            { model with draft = model.draft |> Option.map toggle }, Cmd.Empty

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
                match model.ruleset with
                | Ruleset.ADND ->
                    Html.text "Create a character for Advanced Dungeons and Dragons!"
                | Ruleset.DND5e ->
                    Html.text "Create a character for Fifth Edition Dungeons and Dragons!"
                for ix, ruleset in [Ruleset.ADND; Ruleset.DND5e] |> List.mapi tuple2 do
                    let name = match ruleset with Ruleset.ADND -> "AD&D" | _ -> "5th Edition"
                    // there's probably a better way to navigate/set the URL...
                    Html.input [prop.type'.checkbox; prop.ariaChecked (model.ruleset = ruleset); prop.isChecked (model.ruleset = ruleset); prop.id name; prop.onClick (fun _ -> SetRuleset ruleset |> dispatch); prop.readOnly true]
                    Html.label [prop.htmlFor name; prop.text name]
                ]
            let currentStat stat statValue =
                match model.draft with
                | Some { mode = PointBuy } ->
                    let statValue = (defaultArg statValue 8)
                    Html.span [
                        prop.text $"{stat} {statValue}  " // dot NOT unassign rolls on click, for point buy
                        prop.key $"{stat}"
                        prop.children [
                            Html.text $"{stat} {statValue} "
                            Html.button [prop.text " - "; prop.className "button1"; prop.onClick (fun _ -> ChangePointAllocation(stat, -1) |> dispatch)]
                            Html.button [prop.text " + "; prop.className "button2"; prop.onClick (fun _ -> ChangePointAllocation(stat, +1) |> dispatch)]
                        ]
                    ]
                | _ ->
                    match statValue with
                    | Some statValue ->
                        DragDrop.target [
                            ddprop.targetKey "stats"
                            ddprop.dropData stat
                            ddprop.children [
                                Html.span [
                                    prop.key $"{stat}"
                                    prop.text $"{stat} {statValue}"
                                    prop.onClick(fun _ -> dispatch (UnassignRolls stat))
                                    ]
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
                    currentStat stat statValue
                    match statValue with
                    | Some statValue ->
                        DragDrop.target [
                            ddprop.targetKey "stats"
                            ddprop.dropData stat
                            ddprop.children [
                                Html.span [prop.key $"{stat}descr"; prop.text $"{term} than %0.1f{(getPercentile statValue)*100.}%% of humanity"]
                                ]
                            ]
                    | None ->
                        Html.span [prop.key $"{stat}descr"; prop.text ""]
                    ]
            match model.export with
            | Some char ->
                class' Html.div "middle" [
                    class' Html.div "characterHeader" [
                        Html.text $"{char.name} from {char.nationalOrigin} ({char.sex})"
                        ]
                    class' Html.div "assignedStats" [
                        let describe stat value = describe stat (Some value)
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
                            let statMods =
                                match draft.traits with
                                | DND5e traits ->
                                    let statModsOnly(_, _, _, decisions) =
                                        match decisions |> List.choose (function StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                                        | [] -> None
                                        | mods -> Some mods
                                    match draft.mode with
                                    | CumulativeFrom(min, _) -> Stat.All |> List.map (fun stat -> stat, min)
                                    | _ -> []
                                    @
                                    (summarize statModsOnly DND5e.rules traits [PC]
                                    |> List.collect (fun x -> x))
                                | ADND traits ->
                                    let statModsOnly(_, _, _, decisions) =
                                        match decisions |> List.choose (function ADND2nd.StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                                        | [] -> None
                                        | mods -> Some mods
                                    match draft.mode with
                                    | CumulativeFrom(min, _) -> Stat.All |> List.map (fun stat -> stat, min)
                                    | _ -> []
                                    @
                                    (summarize statModsOnly ADND2nd.rules traits [ADND2nd.Trait.PC]
                                    |> List.collect (fun x -> x))
                            let assignments = addUpStats statMods draft.allocations
                            for stat in Stat.All do
                                match assignments |> Map.tryFind stat with
                                | Some v -> describe stat (Some v)
                                | None ->
                                    describe stat None
                            ]
                        Html.div [
                            // try to keep layout height consistent--I'm sure there's a better way
                            if draft.mode = InOrder then prop.classes ["statRolls";"hide"] else prop.className "statRolls"
                            prop.children [
                                match draft.mode with
                                | PointBuy ->
                                    Html.span [prop.text "Points remaining"; prop.className "label"]
                                    let total = draft.allocations |> Array.sumBy (function (n, None) -> n | _ -> 0)
                                    class' Html.span "roll" [Html.div [prop.text (total.ToString())]]
                                | _ ->
                                    Html.span [prop.text "Unassigned (drag and drop)"; prop.className "label"]
                                    for ix, (roll, stat) in draft.allocations |> Array.mapi tuple2 do
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

                let allowedRollingMethods = if model.ruleset = Ruleset.ADND then ChargenMethod.ADND else ChargenMethod.DND5e
                for ix, method in allowedRollingMethods |> List.mapi tuple2 do
                    Html.div [
                        Html.input [prop.type'.radio; prop.ariaChecked (model.method = method); prop.isChecked (model.method = method); prop.id method.info.name'; prop.onClick (fun _ -> method |> SetMethod |> dispatch); prop.readOnly true]
                        Html.label [prop.htmlFor method.info.name'; prop.text method.info.name']
                        ]

                match model.draft with
                | None -> ()
                | Some { traits = DND5e traits } as draft ->
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
                                        Html.input [prop.type'.checkbox; prop.ariaChecked (decision |> List.contains option); prop.isChecked (decision |> List.contains option); prop.id name; prop.onClick (fun _ -> Toggle5ETrait(head, choiceIx, ix) |> dispatch); prop.readOnly true]
                                        Html.label [prop.htmlFor name; prop.text name]
                                    ]
                                ]
                            |> Some

                    yield! summarize describeChoiceInReact DND5e.rules traits [PC]
                | Some { traits = ADND traits } as draft ->
                    let describeChoiceInReact (head: ADND2nd.Trait, choiceIx, choice: DerivedTraits.Choice<ADND2nd.Trait>, decision: ADND2nd.Trait list) =
                        let toString x = x.ToString()
                        if choice.options.Length = decision.Length then
                            Html.div [
                                class' Html.div "choice" [
                                    for ix, option in choice.options |> List.mapi tuple2 do
                                        let name = option |> ADND2nd.describe
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
                                        let name = option |> ADND2nd.describe
                                        Html.input [prop.type'.checkbox; prop.ariaChecked (decision |> List.contains option); prop.isChecked (decision |> List.contains option); prop.id name; prop.onClick (fun _ -> ToggleADNDTrait(head, choiceIx, ix) |> dispatch); prop.readOnly true]
                                        Html.label [prop.htmlFor name; prop.text name]
                                    ]
                                ]
                            |> Some
                    yield! summarize describeChoiceInReact ADND2nd.rules traits [ADND2nd.Trait.PC]
                ]
            ]
