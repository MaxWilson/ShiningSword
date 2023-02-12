namespace UI.Chargen
open UI
// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Core.DerivedTraits
    open Domain
    open Domain.Metrics
    open Domain.Random
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
        decisions: DerivationInstance
        }

    let addUpStats (statMods: (Stat * int) seq) =
        let addStat n = function
            | Some current -> current + n |> Some
            | None -> Some n
        statMods |> Seq.fold (fun map (stat, n) -> map |> Map.change stat (addStat n)) Map.empty

    let (|CharacterSheet2E|_|) (ctx: PreconditionContext2e) makeOrigin (draft:Draft) : CharacterSheet2e option =
        match draft.decisions with
        | Universal.IsADND(decisions) ->
            match ctx.postracialStats with
            | Map.Lookup Str str & Map.Lookup Dex dex & Map.Lookup Con con
                & Map.Lookup Int int & Map.Lookup Wis wis & Map.Lookup Cha cha
                ->
                let traitSetting = decisions |> toSetting Set.ofSeq rules2e [Trait2e.PC] ctx
                let traits = traitSetting.summary
                let classLevels =
                    traits |> Seq.choose (function Trait2e.Level(cl,lvl) -> Some (cl, lvl) | _ -> None)
                    |> Seq.groupBy fst |> Seq.map (fun (cl, lst) -> cl, lst |> Seq.map snd |> Seq.max)
                    |> Array.ofSeq
                let isWarrior = classLevels |> Seq.exists (function (ADND2nd.Fighter | ADND2nd.Ranger | ADND2nd.Paladin), _  -> true | _ -> false)
                let has = traits.Contains
                let char: CharacterSheet2e = {
                    id = None
                    name = draft.name
                    origin = makeOrigin "AD&D"
                    Str = str
                    Dex = dex
                    Con = con
                    Int = int
                    Wis = wis
                    Cha = cha
                    exceptionalStrength = if isWarrior && str = 18 then draft.exceptionalStrength else None
                    sex = draft.sex
                    traits = traitSetting
                    originalRolls = draft.originalRolls
                    hp = Array.empty // will be computed
                    attacks = 1 // computed
                    toHitBonus = 0 // computed
                    ac = 0 // computed
                    damage = StaticBonus 0 // computed
                    xp = 0<xp>
                    levels = classLevels
                    wealth = 0<gp>
                    }
                char
                |> ADND2nd.recompute
                |> Some
            | _ ->
                None
        | _ -> None
    let (|CharacterSheet5E|_|) (ctx: PreconditionContext5e) makeOrigin (draft:Draft) : CharacterSheet5e option =
        match draft.decisions with
        | Universal.Is5e(decisions) ->
            match ctx with
            | Map.Lookup Str str & Map.Lookup Dex dex & Map.Lookup Con con
                & Map.Lookup Int int & Map.Lookup Wis wis & Map.Lookup Cha cha
                ->
                let traitSetting = decisions |> toSetting Set.ofList rules5e [DND5e.PC] ctx
                let classLevels = traitSetting.summary |> Seq.choose (function Trait5e.Level(cl,lvl) when lvl > 0 -> Some(cl, lvl) | _ -> None) |> Array.ofSeq
                let char: CharacterSheet5e =
                    {
                        CharacterSheet5e.id = None
                        name = draft.name
                        origin = makeOrigin "D&D 5th Edition"
                        Str = str
                        Dex = dex
                        Con = con
                        Int = int
                        Wis = wis
                        Cha = cha
                        sex = draft.sex
                        traits = traitSetting
                        originalRolls = draft.originalRolls
                        xp = 0<xp>
                        levels = classLevels
                        hp = Array.empty // will be computed
                        toHit = 0 // computed
                        ac = 0 // computed
                        damage = StaticBonus 0 // computed
                        wealth = 0<gp>
                        }
                char
                |> DND5e.recompute
                |> Some
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
    let dungeonFantasy assign =
        assign [| 10; 10; 10; 10; 10; 10 |]

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
    let create (traits: Universal.Detail<_,_,_>) method : Draft =
        let sex = chooseRandom [Male; Female]
        let nationalOrigin, name = makeNameAnyNation sex
        method(fun rolls -> {
                name = name
                sex = sex
                nationalOrigin = nationalOrigin
                originalRolls = rolls
                allocations = rolls |> Array.map (fun x -> x, None)
                mode = Assign
                exceptionalStrength = if traits.isADND then Some (rand 100) else None
                decisions = traits
                })

open Interaction

module View =
    open Interaction
    open Elmish
    open Domain
    open Domain.Character
    open DND5e
    open Feliz
    open Feliz.UseElmish
    open Core.DerivedTraits
    open Domain.Character.Universal

    type ChargenMethod =
        | Roll3d6InOrder
        | Roll4d6k3
        | RollPHBMethodVI
        | DarkSunMethodI
        | DarkSun6d4
        | DarkSunMethodV
        | PointBuyN of int
        | DungeonFantasy
        with
        static member ADND = [Roll3d6InOrder;Roll4d6k3;RollPHBMethodVI;DarkSunMethodI;DarkSun6d4;DarkSunMethodV]
        static member DND5e = [Roll3d6InOrder;Roll4d6k3;PointBuyN 27; PointBuyN 31]
        static member DF = [DungeonFantasy]
        member this.info =
            match this with
                | Roll3d6InOrder -> "3d6 in order", roll3d6InOrder
                | Roll4d6k3 -> "4d6 drop lowest", roll4d6k3
                | RollPHBMethodVI -> "7d6, assign to taste", rollPHBMethodVI
                | DarkSunMethodI -> "Dark Sun default", darkSunMethodI
                | DarkSun6d4 -> "Dark Sun 6d4 drop lowest", darkSun6d4
                | DarkSunMethodV -> "Dark Sun Method V", darkSunMethodV
                | PointBuyN n -> $"Point buy ({n})", pointBuy n
                | DungeonFantasy -> "Dungeon Fantasy", dungeonFantasy
            |> MethodInfo
    and MethodInfo = MethodInfo of name: string * ((Rolls -> Draft) -> Draft)
        with
        member this.f = match this with (MethodInfo(name, f)) -> f
        member this.name' = match this with (MethodInfo(name, f)) -> name

    type TextEditMode = | NotEditingText | EditingName // "not editing" is a bit of a misnomer--you can still edit stats and choices, but they aren't text
    type DnDModel = {
        draft: Draft option
        method: ChargenMethod
        editMode: TextEditMode
        }
    type Ruleset = TSR of DnDModel | WotC of DnDModel | DungeonFantasy
    type Model = {
        ruleset: Ruleset
        }
    type ParentMsg =
        | SaveAndQuit of CharacterSheet
        | BeginAdventuring of CharacterSheet
        | Cancel
        | UpdateUrl of suffix: string
    type Msg =
        | Reroll
        | SetRuleset of Ruleset
    let initDnd =
        {
            draft = None
            method = ChargenMethod.ADND.Head
            editMode = NotEditingText
            }
    let rec init _ =
        let sex = chooseRandom [Male; Female]
        let nationalOrigin, name = makeNameAnyNation sex

        {
            ruleset = Ruleset.TSR {
                draft = None
                method = ChargenMethod.ADND.Head
                editMode = NotEditingText
                }
            } |> reroll
    and reroll model =
        match model.ruleset with
        | TSR m ->
            let traits = Detail2e Map.empty
            let char = create traits m.method.info.f
            { model with ruleset = TSR { m with draft = Some char } }
        | WotC m ->
            let traits = Detail5e Map.empty
            let char = create traits m.method.info.f
            { model with ruleset = WotC { m with draft = Some char } }
        | DungeonFantasy ->
            model

    let update cmd informParent msg model =
        match msg with
        | Reroll ->
            reroll model, Cmd.Empty
        | SetRuleset ruleset ->
            match ruleset with
            | Ruleset.TSR _ -> ChargenMethod.ADND.Head, "adnd"
            | Ruleset.WotC _ -> ChargenMethod.DND5e.Head, "5e"
            | Ruleset.DungeonFantasy -> ChargenMethod.DF.Head, "df"
            |> fun (method, urlFragment) ->
                { model with ruleset = ruleset },
                    Cmd.batch [
                        //SetMethod method |> cmd
                        informParent (UpdateUrl urlFragment)]

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
        | Detail2e (char: CharacterSheet2e) ->
            [
                let classes' =
                    let classes = char.levels |> Array.map fst |> Array.distinct
                    let getLevel class' =
                        char.levels |> Array.filter (fst >> (=) class') |> Array.max
                    let classLevels = classes |> Array.map getLevel
                    classLevels |> Array.map (fun (class', lvl) -> $"{class'} {lvl}") |> String.join "/"
                let race = char.traits.summary |> Seq.pick (function (Trait2e.RaceOf _) as race -> Some (ADND2nd.describeTrait race) | _ -> None)
                match char.origin.nationalOrigin with
                | "" ->
                    line $"{char.sex} {race} {classes'} "
                | place ->
                    line $"{char.sex} {race} {classes'} from {place}"
                line $"{char.origin.ruleSystem}, {char.origin.statRollMethod}, from level {char.origin.startingLevel}"
                line ""
                let hpTotal = char.hp |> Array.sumBy(fun (hpRoll,conBonus) -> hpRoll + conBonus)
                let hpBreakdown =
                    char.hp |> Array.map (function (hpRoll, 0) -> hpRoll.ToString() | (hpRoll, conBonus) -> $"{hpRoll}%+d{conBonus}")
                    |> List.ofArray
                    |> String.oxfordJoin
                line $"AC: {char.ac}, HP: {hpTotal} ({hpBreakdown})"
                line $"""{if char.attacks > 1 then $"{char.attacks} attacks, " else ""}THAC0 {20 - char.toHitBonus}, damage: {char.damage}"""
                line $"XP: {char.xp}"
                let displayFilter = Set.filter (function Trait2e.StatMod _ | Trait2e.RaceOf _ | Trait2e.Level _ | Trait2e.SingleClass -> false | _ -> true)
                line $"""{char.traits.summary |> displayFilter |> Seq.map ADND2nd.describeTrait |> String.join "; "}"""
                line $"{char.wealth} gp"
                ]
        | Detail5e (char: CharacterSheet5e) ->
            [
                let describe = DND5e.describeTrait
                let race = char.traits.summary |> Seq.find (fun trait1 -> DND5e.races |> List.contains trait1)
                let classes' =
                    let classes = char.levels |> Array.map fst |> Array.distinct
                    let getLevel class' =
                        char.levels |> Array.filter (fst >> (=) class') |> Array.max
                    let classLevels = classes |> Array.map getLevel
                    classLevels |> Array.map (fun (class', lvl) -> $"{class'} {lvl}") |> String.join "/"
                match char.origin.nationalOrigin with
                | "" ->
                    line $"{char.sex} {describe race} {classes'} "
                | place ->
                    line $"{char.sex} {describe race} {classes'} from {place}"
                line $"{char.origin.ruleSystem}, {char.origin.statRollMethod}, from level {char.origin.startingLevel}"
                line ""
                let hpTotal = char.hp |> Array.sumBy(fun (hpRoll,conBonus) -> hpRoll + conBonus)
                let hpBreakdown =
                    char.hp |> Array.map (function (hpRoll, 0) -> hpRoll.ToString() | (hpRoll, conBonus) -> $"{hpRoll}%+d{conBonus}")
                    |> List.ofArray
                    |> String.oxfordJoin
                line $"AC: {char.ac}, HP: {hpTotal} ({hpBreakdown})"
                let attacks = char.traits.summary |> Seq.choose (function Trait5e.ExtraAttack n -> Some (n+1) | _ -> None) |> Seq.fold max 1
                line $"""{if attacks > 1 then $"{attacks} attacks, " else ""}%+d{char.toHit} to hit, damage: {char.damage}"""
                line $"XP: {char.xp}"
                let displayFilter = Set.filter (function Trait5e.StatMod _  | Trait5e.Level _ | Trait5e.Race _ | Trait5e.StartingClass _ | Trait5e.Feat -> false | r when races |> List.contains r -> false | _ -> true)
                line $"""{char.traits.summary |> displayFilter |> Seq.map DND5e.describeTrait |> String.join "; "}"""
                line $"{char.wealth} gp"
                ]
            | DetailDF (char:CharacterSheetDF) ->
                []

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
    [<ReactComponent>]
    let autoFocusInput props =
        let self = React.useRef None
        React.useEffectOnce(fun _ ->
            if self.current.IsSome then
                self.current?focus()
                self.current?select())
        Html.input (prop.ref self::props)

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
                    let ChangePointAllocation = notImpl
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
                                    //| Str, (Some { exceptionalStrength = Some exStr; decisions = IsADND(HasTrait ADND2nd.rules ADND2nd.Trait.SingleClass (ADND2nd.Trait.Level(ADND2nd.Fighter, 1)) true) }) when model.ruleset = Ruleset.TSR && statValue = 18 ->
                                    //    prop.text $"{stat} {statValue} ({exStr}) "
                                    | _ ->
                                        prop.text $"{stat} {statValue} "
                                    //prop.onClick(fun _ -> dispatch (UnassignRolls stat))
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
    let describeStatTextOnly stat statValue =
        let term =
            match stat with
            | Str -> "Stronger"
            | Dex -> "Faster"
            | Con -> "Tougher"
            | Int -> "Smarter"
            | Wis -> "Wiser"
            | Cha -> "More charismatic"
        $"{term} than %0.1f{(getPercentile statValue)*100.}%% of humanity"

    let describe model dispatch (stat:Stat) statValue =
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
                                Html.span [prop.key $"{stat}descr"; prop.text (describeStatTextOnly stat statValue)]
                                ]
                            ]
                        ]
                    ]
            | None -> ()
            ]

    type ChoiceStatus = Fixed | Open | Resolved
    let describeChoiceInReact plainTraitFilter dispatch msg describe preconditionContext (head, choiceIx, choice: Core.DerivedTraits.Choice<_,_>, decision: _ array) =
        let toString x = x.ToString()
        if choice.options.Length = decision.Length then
            React.fragment [
                for ix, option in choice.options |> Array.mapi tuple2 do
                    if plainTraitFilter option then
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

    let viewCharacter = function
        | Universal.GenericCharacterSheet char as sheet -> [
            class' Html.div "characterHeader" [
                class' Html.div "title" [
                    Html.text $"{char.name}"
                    ]
                ]
            let describe stat statValue = [
                Html.span [
                    prop.classes ["statValue"; "stat" + stat.ToString()]
                    match char.exceptionalStrength with
                    | Some exceptionalStrength when stat = Str ->
                        prop.text $"{stat} {statValue} ({exceptionalStrength}) "
                    | _ ->
                        prop.text $"{stat} {statValue}  "
                    prop.key $"{stat}"
                    ]
                Html.div [
                    prop.classes ["statDescription"; "stat" + stat.ToString()]
                    prop.children [
                        Html.span [prop.key $"{stat}descr"; prop.text (describeStatTextOnly stat statValue)]
                        ]
                    ]
                ]

            yield! describe Str char.Str
            yield! describe Dex char.Dex
            yield! describe Con char.Con
            yield! describe Int char.Int
            yield! describe Wis char.Wis
            yield! describe Cha char.Cha
            class' Html.div "prettyMiddle" []
            class' Html.div "summary" (
                describeCharacter sheet
                )
            ]

    [<ReactComponent>]
    let View (model: Model) (control: ParentMsg -> unit) dispatch =
        class' Html.div "charGen" [
            class' Html.div "header" [
                match model.ruleset with
                | Ruleset.TSR model ->
                    let describe = describe model dispatch
                    Html.text "Create a character for Advanced Dungeons and Dragons!"
                | Ruleset.WotC model ->
                    let describe = describe model dispatch
                    Html.text "Create a character for Fifth Edition Dungeons and Dragons!"
                | Ruleset.DungeonFantasy ->
                    Html.text "Create a character for Dungeon Fantasy RPG (powered by GURPS)!"
                ]
            UI.CharData.view()
    ]
