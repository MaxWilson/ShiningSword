namespace Chargen

// module for doing stuff that isn't part of the target domain but isn't just ViewModel boilerplate either
//    Effectively, this for for treating user interaction as a domain of its own.
module Interaction =
    open Domain
    open Domain.Character
    type Rolls = int list

    type Mode = CumulativeFrom of min:int * max:int | Assign
    type Draft = {
        name: string
        allocations: (int * Stat option) list
        originalRolls: Rolls
        mode: Mode
        }

    let addUpStats (allocations: (int * Stat option) list) =
        allocations
            |> List.choose (function (roll, Some stat) -> Some(roll, stat) | _ -> None)
            |> List.groupBy snd
            |> List.map (fun (stat, lst) -> stat, lst |> List.map fst |> List.fold (+) 0)
            |> Map.ofList

    let ofDraft (draft:Draft) : CharacterSheet option =
        match draft.allocations |> addUpStats with
        | Lookup Str str & Lookup Dex dex & Lookup Con con
            & Lookup Int int & Lookup Wis wis & Lookup Cha cha
            ->
            {
                CharacterSheet.name = draft.name
                Str = str
                Dex = dex
                Con = con
                Int = int
                Wis = wis
                Cha = cha
                sex = Male
                traits = DerivedTraits.toSetting Set.ofList rules [PC] Map.empty
                originalRolls = draft.originalRolls
                } |> Some
        | _ ->
            None
    let d = rand
    let inOrder (draft:Draft) =
        match draft.allocations with
        | [str,_;dex,_;con,_;int,_;wis,_;cha,_] ->
            let mapSnd = List.map (fun (arg, stat) -> arg, Some stat)
            { draft with allocations = [str,Str;dex,Dex;con,Con;int,Int;wis,Wis;cha,Cha] |> mapSnd }
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
            | Assign -> allocations
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
        method(fun rolls -> {
                name = makeName()
                originalRolls = rolls
                allocations = rolls |> List.map (fun x -> x, None)
                mode = Assign
                })

open Interaction

module View =
    open Interaction
    open Elmish
    open Domain.Character
    open Feliz
    open Feliz.UseElmish

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
        }
    type Msg =
        | Done of CharacterSheet
        | Cancel
        | Reroll
        | SetMethod of ChargenMethod
        | AssignRoll of ix:int * stat:Stat
        | UnassignRolls of stat: Stat
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
        | AssignRoll(ix, stat) ->
            { model with draft = model.draft |> Option.map (assign ix stat) }, Cmd.Empty
        | UnassignRolls stat ->
            { model with draft = model.draft |> Option.map (unassign stat) }, Cmd.Empty


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
        fun statValue -> lessThanEqualGroups[statValue]

    open Fable.Core.JsInterop
    open Fable.React
    open Feliz
    open Konva

    let inline printKeys arg =
        let rec recur arg =
            match arg?key with
            | Some key ->
                printfn $"Key: {key}"
            | None ->
                printfn $"No key! {arg}"
                Browser.Dom.window?noKey <- arg
            match arg?children with
            | Some children ->
                recur children
            | None -> ()
        recur arg
        arg

    [<ReactComponent>]
    let view = Fable.React.FunctionComponent.Of(memoizeWith = equalsButFunctions, render = fun (arg: {| model: Model; dispatch: Msg -> unit |}) ->
        let model = arg.model
        let dispatch = arg.dispatch
        let window = Browser.Dom.window
        let width = (int window.innerWidth - 80)
        let myStage = React.useRef None
        let myLayer = React.useRef None
        let dragLayer = React.useRef None
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
        let mutable recentDrag = None // there's probably a better way to do this
        class' Html.div "charGen" [
            class' Html.div "Title" [
                Html.text "Create a character!"
                ]
            let dragOverHandler stat (e:Browser.Types.DragEvent) =
                recentDrag <- Some stat
                e.preventDefault()
            let dragEnterHandler (e:Browser.Types.DragEvent) =
                e.preventDefault()
            let currentStat stat statValue =
                match statValue with
                | Some statValue ->
                    Html.span [prop.text $"{stat} {statValue}"; prop.onDragOver (dragOverHandler stat); prop.onDragEnter dragEnterHandler; prop.onClick(fun _ -> dispatch (UnassignRolls stat))]
                | None ->
                    Html.span [prop.text $"{stat}     "; prop.onDragOver (dragOverHandler stat); prop.onDragEnter dragEnterHandler]
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
                    Html.span [prop.text $"{term} than %0.1f{(getPercentile statValue)*100.}%% of humanity" ; prop.onDragOver (dragOverHandler stat); prop.onDragEnter dragEnterHandler]
                    ]
            match model.export with
            | Some char ->
                class' Html.div "middle" [
                    class' Html.div "characterHeader" [
                        Html.text char.name
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
                            Html.text draft.name
                            ]
                        class' Html.div "assignedStats" [
                            let assignments = addUpStats draft.allocations
                            let totalFor stat =
                                match assignments |> Map.tryFind stat, draft.mode with
                                | None, CumulativeFrom(min, _) -> Some min
                                | Some v, CumulativeFrom(min, _) -> Some (min + v)
                                | Some v, Assign -> Some v
                                | None, Assign -> None
                            for stat in Stat.All do
                                match totalFor stat with
                                | Some v -> describe stat v
                                | None ->
                                    currentStat stat None
                                    Html.span []
                            ]
                        class' Html.div "statRolls" [
                            Html.span [prop.text "Unassigned (drag and drop)"]
                            for ix, (roll, stat) in draft.allocations |> List.mapi tuple2 do
                                match stat with
                                | None ->
                                    let dropHandler ev =
                                        // this is a little bit wrong. It will still drop the stat even if you've moved off before releasing the mouse.
                                        match recentDrag with
                                        | Some stat ->
                                            dispatch (AssignRoll(ix, stat))
                                        | None -> ()
                                    Html.span [
                                        prop.className "roll"
                                        prop.children [
                                            Html.span [prop.text (roll); prop.draggable true; prop.onDragEnd dropHandler; prop.onDragStart (fun e -> e.dataTransfer.setData("text/html", "dummy") |> ignore)]
                                            ]
                                        ]
                                | Some _ ->
                                    Html.span [prop.draggable true; prop.className "roll"]
                            ]
                        ]
                | None -> ()
            class' Html.div "footer" [
                Html.button [
                    prop.text "Reroll"
                    prop.onClick (fun _ -> dispatch Reroll)
                    ]

                for ix, method in ChargenMethod.All |> List.mapi (fun i x -> i, x) do
                    Html.div [
                        Html.input [prop.type'.radio; prop.ariaChecked (model.method = method); prop.isChecked (model.method = method); prop.id method.info.name'; prop.onClick (fun _ -> method |> SetMethod |> dispatch); prop.readOnly true]
                        Html.label [prop.htmlFor method.info.name'; prop.text method.info.name']
                        ]
                ]
            ])
