namespace UI.Chargen

// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Domain
    open DerivedTraits
    open Domain.Character
    open Domain.Character.Universal

    type Rolls = int array

    type Mode = CumulativeFrom of min:int * max:int | Assign | InOrder | PointBuy
    type Draft = {
        name: string
        nationalOrigin: string
        sex: Sex
        allocations: (int * Stat option) array
        exceptionalStrength: int option
        originalRolls: Rolls
        mode: Mode
        traits: DerivationInstance
        }

    let addUpStats (statMods: (Stat * int) seq) (allocations: (int * Stat option) array) =
        let rawTotals =
            allocations
            |> Array.choose (function (roll, Some stat) -> Some(roll, stat) | _ -> None)
            |> Array.groupBy snd
            |> Array.map (fun (stat, lst) -> stat, lst |> Array.map fst |> Array.fold (+) 0)
            |> Map.ofArray
        let addStat n = function
            | Some current -> current + n |> Some
            | None -> Some n
        statMods |> Seq.fold (fun map (stat, n) -> map |> Map.change stat (addStat n)) rawTotals

    let (|CharacterSheetADND2nd|_|) makeOrigin (draft:Draft) : CharacterSheet2e option =
        match draft.traits with
        | Universal.IsADND(traits) ->
            let statModsOnly(_, _, _, decisions) =
                match decisions |> Array.choose (function ADND2nd.StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                | [||] -> None
                | mods -> Some mods
            let statMods = summarize statModsOnly ADND2nd.rules traits [ADND2nd.Trait.PC] |> List.collect List.ofArray
            match draft.allocations |> addUpStats statMods with
            | Lookup Str str & Lookup Dex dex & Lookup Con con
                & Lookup Int int & Lookup Wis wis & Lookup Cha cha
                ->
                let traits = traits |> toSetting Set.ofList rules2e [ADND2nd.PC]
                if traits.validated then
                    Some {
                        name = draft.name
                        origin = makeOrigin "AD&D"
                        Str = str
                        Dex = dex
                        Con = con
                        Int = int
                        Wis = wis
                        Cha = cha
                        sex = draft.sex
                        traits = traits
                        originalRolls = draft.originalRolls
                        xp = 0
                        levels = [||]
                        }
                else None
            | _ ->
                None
        | _ -> None
    let (|CharacterSheet5E|_|) makeOrigin (draft:Draft) : CharacterSheet5e option =
        match draft.traits with
        | Universal.Is5e(traits) ->
            let statModsOnly(_, _, _, decisions) =
                match decisions |> Array.choose (function DND5e.StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                | [||] -> None
                | mods -> Some mods
            let statMods = summarize statModsOnly DND5e.rules traits [DND5e.PC] |> List.collect List.ofArray
            match draft.allocations |> addUpStats statMods with
            | Lookup Str str & Lookup Dex dex & Lookup Con con
                & Lookup Int int & Lookup Wis wis & Lookup Cha cha
                ->
                let traits = traits |> toSetting Set.ofList rules5e [DND5e.PC]
                if traits.validated then
                    Some {
                        name = draft.name
                        origin = makeOrigin "D&D 5th Edition"
                        Str = str
                        Dex = dex
                        Con = con
                        Int = int
                        Wis = wis
                        Cha = cha
                        sex = draft.sex
                        traits = traits
                        originalRolls = draft.originalRolls
                        xp = 0
                        levels = Array.empty
                        }
                else None
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
    let create (traits: Universal.Detail<_,_>) method : Draft =
        let sex = chooseRandom [Male; Female]
        let nationalOrigin, name = makeName sex
        method(fun rolls -> {
                name = name
                sex = sex
                nationalOrigin = nationalOrigin
                originalRolls = rolls
                allocations = rolls |> Array.map (fun x -> x, None)
                mode = Assign
                exceptionalStrength = if traits.isADND then Some (rand 100) else None
                traits = traits
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
    open Domain.Character.Universal

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

    type Ruleset = TSR | WotC
    type Model = {
        id: int option
        draft: Draft option
        export: Universal.CharacterSheet option
        method: ChargenMethod
        editMode: TextEditMode
        ruleset: Ruleset
        }
    and TextEditMode = | NotEditingText | EditingName // "not editing" is a bit of a misnomer--you can still edit stats and choices, but they aren't text
    type ParentMsg =
        | SaveAndQuit of Model
        | BeginAdventuring of Domain.Adventure.AdventureState
        | Cancel
        | UpdateUrl of suffix: string
    type Msg =
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
        | FinalizeCharacterSheet of Universal.CharacterSheet
    let rec init _ =
        {
            id = None
            draft = None
            export = None
            method = ChargenMethod.ADND.Head
            editMode = NotEditingText
            ruleset = Ruleset.TSR
            } |> reroll
    and reroll model =
        let traits = if model.ruleset = TSR then DetailADND Map.empty else Detail5e Map.empty
        let char = create traits model.method.info.f
        { model with draft = Some char; export = None }
    let update cmd informParent msg model =
        match msg with
        | Reroll ->
            reroll model, Cmd.Empty
        | SetMethod m ->
            { model with method = m }, cmd Reroll
        | AssignRoll(ix, stat) ->
            { model with draft = model.draft |> Option.map (assign ix stat) }, Cmd.Empty
        | UnassignRolls stat ->
            match model.draft with
            | Some { mode = (Assign | CumulativeFrom _) } ->
                { model with draft = model.draft |> Option.map (unassign stat) }, Cmd.Empty
            | _ -> model, Cmd.Empty
        | ChangePointAllocation(stat, plusOrMinus) ->
            let draft' = model.draft |> Option.map (changePointAllocation stat plusOrMinus)
            printfn "%A" draft'.Value.allocations
            { model with draft = draft' }, Cmd.Empty
        | SetRuleset ruleset ->
            let msg = SetMethod (if ruleset = Ruleset.TSR then ChargenMethod.ADND.Head else ChargenMethod.DND5e.Head)
            { model with ruleset = ruleset }, Cmd.batch [msg |> cmd;informParent (UpdateUrl (if ruleset = TSR then "adnd" else "5e"))]
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
                { draft with traits = draft.traits.map5e (toggleTrait(rules5e, head, choiceIx, decisionIx)) }
            { model with draft = model.draft |> Option.map toggle }, Cmd.Empty
        | ToggleADNDTrait(head, choiceIx, decisionIx) ->
            let toggle (draft:Draft) =
                { draft with traits = draft.traits.map2e (toggleTrait(rules2e, head, choiceIx, decisionIx)) }
            { model with draft = model.draft |> Option.map toggle }, Cmd.Empty
        | FinalizeCharacterSheet sheet ->
            { model with export = Some sheet }, Cmd.Empty

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
            elif statValue > 0 then lessThanEqualGroups[statValue]
            else 0.

    let describeCharacter =
        let line (txt: string) = Html.div [Html.text txt]
        function
        | DetailADND (char: CharacterSheet2e) ->
            [
                let describe = ADND2nd.describeTrait
                match char.traits.instance with
                | FirstTrait rules2e Trait2e.Race race & FirstTrait rules2e Trait2e.SingleClass class' ->
                    match char.origin.nationalOrigin with
                    | "" ->
                        line $"{char.sex} {describe race} {describe class'} "
                    | place ->
                        line $"{char.sex} {describe race} {describe class'} from {place}"
                | _ ->
                    shouldntHappen() // should always have a race and class before exporting
                line $"{char.origin.ruleSystem}, {char.origin.statRollMethod}, from level {char.origin.startingLevel}"
                line ""
                line $"HP: 0, AC: 0, THAC0: 0"
                line $"XP: 0"
                ]
        | Detail5e (char: CharacterSheet5e) ->
            [
                let describe = DND5e.describeTrait
                match char.traits.instance with
                | FirstTrait rules5e Trait5e.Race race & FirstTrait rules5e Trait5e.StartingClass class' ->
                    match char.origin.nationalOrigin with
                    | "" ->
                        line $"{char.sex} {describe race} {describe class'} "
                    | place ->
                        line $"{char.sex} {describe race} {describe class'} from {place}"
                | _ ->
                    shouldntHappen() // should always have a race and class before exporting
                line $"{char.origin.ruleSystem}, {char.origin.statRollMethod}, from level {char.origin.startingLevel}"
                line ""
                line $"HP: 0, AC: 0, THAC0: 0"
                line $"XP: 0"
                ]

    open Fable.Core.JsInterop
    open Fable.React
    open Feliz
    open UI
    let ddprop = DragDrop.prop

    // helper method to make assigning css classes more concise
    let class' f className children =
        f [
            prop.className (className: string)
            prop.children (children: _ list)
            ]

    let currentStat model dispatch stat statValue =
        match model.draft with
        | Some { mode = PointBuy } ->
            let statValue = (defaultArg statValue 8)
            Html.span [
                prop.classes ["statValue"; "stat" + stat.ToString()]
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
                Html.div [
                    prop.classes ["statValue"; "stat" + stat.ToString()]
                    prop.children [
                        DragDrop.target [
                            ddprop.targetKey "stats"
                            ddprop.dropData stat
                            ddprop.children [
                                Html.span [
                                    prop.key $"{stat}"
                                    match stat, model.draft with
                                    | Str, (Some { exceptionalStrength = Some exStr; traits = IsADND(HasTrait ADND2nd.rules ADND2nd.Trait.SingleClass (ADND2nd.Trait.Level(ADND2nd.Fighter, 1)) true) }) when model.ruleset = Ruleset.TSR && statValue = 18 ->
                                        prop.text $"{stat} {statValue} ({exStr}) "
                                    | _ ->
                                        prop.text $"{stat} {statValue} "
                                    prop.onClick(fun _ -> dispatch (UnassignRolls stat))
                                    ]
                                ]
                            ]
                        ]
                    ]
            | None ->
                Html.div [
                    prop.classes ["statValue"; "stat" + stat.ToString()]
                    prop.children [
                        DragDrop.target [
                            ddprop.targetKey "stats"
                            ddprop.key ("currentStat" + stat.ToString())
                            ddprop.dropData stat
                            ddprop.children [
                                Html.span [
                                    prop.text $"{stat}     "
                                    prop.key $"{stat}"
                                    ]
                                ]
                            ]
                        ]
                    ]
    let describe model dispatch (stat:Stat) statValue =
        let term =
            match stat with
            | Str -> "Stronger"
            | Dex -> "Faster"
            | Con -> "Tougher"
            | Int -> "Smarter"
            | Wis -> "Wiser"
            | Cha -> "More charismatic"
        React.fragment [
            currentStat model dispatch stat statValue
            match statValue with
            | Some statValue ->
                Html.div [
                    prop.classes ["statDescription"; "stat" + stat.ToString()]
                    prop.children [
                        DragDrop.target [
                            ddprop.targetKey "stats"
                            ddprop.key ("statDescr" + stat.ToString())
                            ddprop.dropData stat
                            ddprop.children [
                                Html.span [prop.key $"{stat}descr"; prop.text $"{term} than %0.1f{(getPercentile statValue)*100.}%% of humanity"]
                                ]
                            ]
                        ]
                    ]
            | None -> ()
            ]

    type ChoiceStatus = Fixed | Open | Resolved
    let describeChoiceInReact dispatch msg describe preconditionContext (head, choiceIx, choice: DerivedTraits.Choice<_,_>, decision: _ array) =
        let toString x = x.ToString()
        if choice.options.Length = decision.Length then
            React.fragment [
                for ix, option in choice.options |> Array.mapi tuple2 do
                    let name = option |> describe
                    class' Html.span "plainTrait" [
                        Html.text (name: string)
                        ]
                ]
            |> fun x -> Some(Fixed, x)
        elif choice.numberAllowed = decision.Length then
            React.fragment [
                // filter out the non-chosen options after the choice is made, to save on screen space
                for ix, option in choice.options |> Array.mapi tuple2 do
                    if decision |> Array.contains option then
                        let name = option |> describe
                        Html.input [prop.type'.checkbox; prop.ariaChecked (decision |> Array.contains option); prop.isChecked (decision |> Array.contains option); prop.id name; prop.onClick (fun _ -> msg(head, choiceIx, ix) |> dispatch); prop.readOnly true]
                        Html.label [prop.htmlFor name; prop.text name]
                ]
            |> fun x -> Some(Resolved, x)
        else
            Html.div [
                class' Html.section "choice" [
                    for ix, option in choice.options |> Array.mapi tuple2 do
                        let name = option |> describe
                        let isValid = choice.preconditions.IsNone || choice.preconditions.Value(option, preconditionContext)
                        if isValid then
                            Html.span [
                                Html.input [prop.type'.checkbox; prop.ariaChecked (decision |> Array.contains option); prop.isChecked (decision |> Array.contains option); prop.id name; prop.onClick (fun _ -> msg(head, choiceIx, ix) |> dispatch); prop.readOnly true]
                                Html.label [prop.htmlFor name; prop.text name]
                                ]
                        else
                            class' Html.span "lacksPrereq" [
                                Html.input [prop.type'.checkbox; prop.ariaChecked (decision |> Array.contains option); prop.isChecked (decision |> Array.contains option); prop.id name; prop.readOnly true; prop.disabled true; prop.ariaDisabled true]
                                Html.label [prop.htmlFor name; prop.text name; prop.disabled true; prop.ariaDisabled true]
                                ]
                    ]
                ]
            |> fun x -> Some(Open, x)

    [<ReactComponent>]
    let view (model: Model) (control: ParentMsg -> unit) dispatch =
        let describe = describe model dispatch
        class' Html.div "charGen" [
            class' Html.div "header" [
                if model.export.IsNone then
                    match model.ruleset with
                    | Ruleset.TSR ->
                        Html.text "Create a character for Advanced Dungeons and Dragons!"
                    | Ruleset.WotC ->
                        Html.text "Create a character for Fifth Edition Dungeons and Dragons!"
                    for ix, ruleset in [Ruleset.TSR; Ruleset.WotC] |> List.mapi tuple2 do
                        let name = match ruleset with Ruleset.TSR -> "AD&D" | _ -> "5th Edition"
                        // there's probably a better way to navigate/set the URL...
                        Html.input [prop.type'.checkbox; prop.ariaChecked (model.ruleset = ruleset); prop.isChecked (model.ruleset = ruleset); prop.id name; prop.onClick (fun _ -> SetRuleset ruleset |> dispatch); prop.readOnly true]
                        Html.label [prop.htmlFor name; prop.text name]
                class' Html.div "controls" [
                    Html.button [prop.text "Save and quit"; prop.onClick(fun _ -> SaveAndQuit model |> control)]
                    Html.button [prop.text "Quit without saving"; prop.onClick(fun _ -> Cancel |> control)]
                    ]
                ]
            match model.export with
            | Some (Universal.GenericCharacterSheet char as sheet) ->
                class' Html.div "characterHeader" [
                    class' Html.div "title" [
                        Html.text $"{char.name}"
                        ]
                    ]
                let describe stat value = describe stat (Some value)
                describe Str char.Str
                describe Dex char.Dex
                describe Con char.Con
                describe Int char.Int
                describe Wis char.Wis
                describe Cha char.Cha
                class' Html.div "prettyMiddle" []
                class' Html.div "summary" (
                    describeCharacter model.export.Value
                    )
                let beginAdventure _ =
                    Domain.Adventure.createAdventure sheet
                    |> BeginAdventuring
                    |> control
                class' Html.div "finalize" [
                    Html.button [prop.text "Begin adventure"; prop.onClick beginAdventure]
                    ]
            | None ->
                match model.draft with
                | Some draft ->
                    Html.section [prop.className "prettyMiddle"]
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
                                    prop.onKeyDown (fun ev -> if ev.code = "Enter" then SetEditMode NotEditingText |> dispatch); prop.onBlur (fun ev -> SetEditMode NotEditingText |> dispatch)                                    ]

                            if draft.nationalOrigin <> "" then
                                Html.text $" from {draft.nationalOrigin}"
                            ]
                        ]
                    class' Html.div "chooseSex" [
                        for sex in [Male; Female] do
                            let id = sex.ToString()
                            Html.input [prop.type'.radio; prop.ariaChecked (draft.sex = sex); prop.isChecked (draft.sex = sex); prop.id id; prop.onClick (fun _ -> SetSex sex |> dispatch); prop.readOnly true]
                            Html.label [prop.htmlFor id; prop.text id]
                        ]
                    let statMods traits =
                        match traits with
                        | Detail5e traits ->
                            let statModsOnly(_, _, _, decisions) =
                                match decisions |> Array.choose (function StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                                | [||] -> None
                                | mods -> Some mods
                            match draft.mode with
                            | CumulativeFrom(min, _) -> Stat.All |> List.map (fun stat -> stat, min)
                            | _ -> []

                            @
                            (summarize statModsOnly DND5e.rules traits [PC]
                            |> List.collect (List.ofArray))
                        | DetailADND traits ->
                            let statModsOnly(_, _, _, decisions) =
                                match decisions |> Array.choose (function ADND2nd.StatMod(stat, n) -> Some(stat, n) | _ -> None) with
                                | [||] -> None
                                | mods -> Some mods
                            match draft.mode with
                            | CumulativeFrom(min, _) -> Stat.All |> List.map (fun stat -> stat, min)
                            | _ -> []
                            @
                            (summarize statModsOnly ADND2nd.rules traits [ADND2nd.Trait.PC]
                            |> List.collect (List.ofArray))
                    let statAssignments = addUpStats (statMods draft.traits) draft.allocations
                    for stat in Stat.All do
                        match statAssignments |> Map.tryFind stat with
                        | Some v -> describe stat (Some v)
                        | None ->
                            describe stat None
                    Html.div [
                        // try to keep layout height consistent--I'm sure there's a better way
                        if draft.mode = InOrder then prop.classes ["statRolls";"hide"] else prop.className "statRolls"
                        prop.children [
                            match draft.mode with
                            | PointBuy ->
                                Html.span [prop.text "Points remaining"; prop.className "label"]
                                let total = draft.allocations |> Array.sumBy (function (n, None) -> n | _ -> 0)
                                class' Html.span "roll" [Html.div [prop.text (total.ToString()); prop.className "value"]]
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
                                        class' Html.span "roll" [Html.span [prop.text "0"; prop.classes ["hide";"value"]]]
                            ]
                        ]
                    class' Html.div "rollingMethods" [
                        Html.button [
                            prop.text "Reroll"
                            prop.onClick (thunk1 dispatch Reroll)
                            ]

                        let allowedRollingMethods = if model.ruleset = Ruleset.TSR then ChargenMethod.ADND else ChargenMethod.DND5e
                        for ix, method in allowedRollingMethods |> List.mapi tuple2 do
                            Html.div [
                                Html.input [prop.type'.radio; prop.ariaChecked (model.method = method); prop.isChecked (model.method = method); prop.id method.info.name'; prop.onClick (fun _ -> method |> SetMethod |> dispatch); prop.readOnly true]
                                Html.label [prop.htmlFor method.info.name'; prop.text method.info.name']
                                ]
                        ]

                    let display (lst: (_ * ReactElement) list) =
                        let chosen = lst |> List.choose (fun (pri, e) -> if pri = Resolved then Some e else None)
                        let traits = lst |> List.choose (fun (pri, e) -> if pri = Fixed then Some e else None)
                        let choice = lst |> List.choose (fun (pri, e) -> if pri = Open then Some e else None)
                        [
                            if chosen.Length > 0 then
                                class' Html.div "chosen" chosen
                            if traits.Length > 0 then
                                class' Html.div "traits" traits
                            for element in choice do
                                element
                            ]
                    let makeOrigin s = { ruleSystem = s; nationalOrigin = draft.nationalOrigin; startingLevel = 1; statRollMethod = model.method.info.name' }
                    match draft.traits with
                    | Detail5e traits ->
                        let roots = [PC]
                        let rules = DND5e.rules
                        let currentTraits = DerivedTraits.collect rules traits roots (fun traits -> (statAssignments, traits |> Set.ofSeq) ) |> Set.ofSeq
                        let toReact = describeChoiceInReact dispatch Toggle5ETrait DND5e.describeTrait (statAssignments, currentTraits)
                        let traits = summarize toReact rules traits roots
                        class' Html.div "chooseTraits" [
                            yield! (traits |> display)
                            ]
                        match draft with
                        | CharacterSheet5E makeOrigin sheet when (traits |> List.exists(fun (pri, e) -> pri = Open) |> not) ->
                            class' Html.div "finalize" [
                                Html.button [prop.text "OK"; prop.onClick (fun _ -> Universal.Detail5e sheet |> FinalizeCharacterSheet |> dispatch)]
                                ]
                        | _ -> ()

                    | DetailADND traits ->
                        let roots = [ADND2nd.PC]
                        let rules = ADND2nd.rules
                        let makeContext traits : PreconditionContext2e =
                            { traits = traits |> Set.ofSeq; preracialStats = addUpStats (Map.empty |> DetailADND |> statMods) draft.allocations; postracialStats = statAssignments }
                        let currentTraits = DerivedTraits.collect rules traits roots makeContext
                        let toReact = describeChoiceInReact dispatch ToggleADNDTrait ADND2nd.describeTrait (makeContext currentTraits)
                        let traits = summarize toReact ADND2nd.rules traits [ADND2nd.Trait.PC]
                        class' Html.div "chooseTraits" [
                            yield! (traits |> display)
                            ]
                        match draft with
                        | CharacterSheetADND2nd makeOrigin sheet when (traits |> List.exists(fun (pri, e) -> pri = Open) |> not) ->
                            class' Html.div "finalize" [
                                Html.button [prop.text "OK"; prop.onClick (fun _ -> Universal.DetailADND sheet |> FinalizeCharacterSheet |> dispatch)]
                                ]
                        | _ -> ()

                | None -> ()
            ]
