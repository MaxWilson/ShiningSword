module UI.App

open Browser.Dom
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz
open Konva

// make sure errors are not silent: show them as Alerts (ugly but better than nothing for now)
open Fable.Core.JsInterop
open Fable.Core

importSideEffects "../sass/main.sass"

module App =
    open Domain.Character
    open Domain.Character.DND5e
    open Thoth.Json
    open UI.Chargen.Interaction

    type Page =
        | Home
        | Generate of Chargen.View.Model
        | Adventure of Domain.Adventure.AdventureState
    type Model = { current: Page; error: string option; roster: Chargen.View.Model array }
    type Msg =
        | Error of msg: string
        | Transform of (Model -> Model)
        | Chargen of Chargen.View.Msg
        | Open of Page
        | GoHome
        | Navigate of url: string
        | UpdateUrl of url: string // change URL without adding it to history--use this when a user would be surprised/irritated if "back button" acted as "undo"
        | AddOrUpdateRoster of Chargen.View.Model
        | ClearRoster
        | DeleteCharacter of Name

    module LocalStorage =
        let key = "PCs"
        let inline jsonParse<'t> fallback str : 't =
            match Decode.Auto.fromString str with
            | Ok result -> result
            | Result.Error err ->
                printfn "Parse Error: %A " err
                fallback
        let inline read<'t> fallback =
            try
                Browser.Dom.window.localStorage[key] |> jsonParse<'t> fallback
            with _ ->
                fallback
        let inline write value =
            Browser.Dom.window.localStorage[key] <- Encode.Auto.toString<'t>(0, value)

    let init initialCmd =
        let json = Browser.Dom.window.localStorage["PCs"]
        let models = LocalStorage.read<Chargen.View.Model array> Array.empty
        { current = Page.Home; error = None; roster = models }, Cmd.batch initialCmd
    let chargenControl dispatch = function
        | Chargen.View.SaveAndQuit chargenModel ->
            AddOrUpdateRoster chargenModel |> dispatch
            GoHome |> dispatch
        | Chargen.View.Cancel -> dispatch GoHome
        | Chargen.View.UpdateUrl ruleset ->
            match ruleset with
            | Ruleset.TSR -> Navigate "chargen/adnd"
            | Ruleset.WOTC -> Navigate "chargen/5e"
            |> dispatch
        | Chargen.View.BeginAdventuring adv ->
            Transform (fun m -> { m with current = Page.Adventure adv }) |> dispatch
            UpdateUrl "/" |> dispatch // TODO: make a URL router directly to character's adventure
    let update msg model =
        match msg, model.current with
        | Error msg, _ -> { model with error = Some msg }, Cmd.Empty
        | Transform f, _ -> { f model with error = None }, Cmd.Empty
        | Navigate url, _ -> model, Navigation.Navigation.newUrl ("#" + url)
        | UpdateUrl url, _ -> model, Navigation.Navigation.modifyUrl ("#" + url)
        | AddOrUpdateRoster chargenModel, _ ->
            // recent updated entries should be at the head of the list, so filter and then re-add at front
            let roster' =
                model.roster
                |> Array.filter (function
                    | { draft = Some { name = name } } when chargenModel.draft.IsSome && name = chargenModel.draft.Value.name -> false
                    | _ -> true)
            let roster' = Array.append [|chargenModel|] roster'
            LocalStorage.write roster'
            { model with roster = roster' }, Cmd.Empty
        | ClearRoster, _ ->
            let roster' = Array.empty
            LocalStorage.write roster'
            { model with roster = roster' }, Cmd.Empty
        | DeleteCharacter characterName, _ ->
            let roster' = model.roster |> Array.filter (function { draft = Some { name = name' } } when name' = characterName -> false | _ -> true)
            LocalStorage.write roster'
            { model with roster = roster' }, Cmd.Empty
        | Chargen msg, (Page.Generate chargenModel)->
            let cmd = (Chargen >> Cmd.ofMsg)
            let chargenModel, cmd = Chargen.View.update (Chargen >> Cmd.ofMsg) (flip chargenControl >> Cmd.ofSub) msg chargenModel
            { model with current = (Page.Generate chargenModel)}, cmd
        | GoHome, _ ->
            // default to welcome screen if stack is empty
            let model', msg = Chargen.View.init()
            { model with current = Home}, Navigate "/" |> Cmd.ofMsg
        | Open(page), stack -> { model with current = page}, Cmd.Empty
        | _ -> model, (Error $"Message '{msg}' not compatible with current page ({model.current}))" |> Cmd.ofMsg)
    open Feliz.Router
    let view (model: Model) dispatch =
        let window = Browser.Dom.window;
        match model.current with
        | Page.Generate model ->
            Chargen.View.view model (chargenControl dispatch) (Chargen >> dispatch)
        | Page.Adventure adventure ->
            Html.div [
                Html.text "We're going on an adventure (Under Construction)!"
                Html.button [prop.text "Home"; prop.onClick (thunk1 dispatch GoHome)]
                ]
        | _ ->
            let class' element (className: string) (children: ReactElement list) =
                element [prop.className className; prop.children children]
            Html.div [
                prop.className "homePage"
                prop.children [
                    Html.div [
                        prop.children [
                            Html.text (match model.error with Some msg -> msg | None -> "Welcome to Shining Sword")
                            ]
                        prop.style [style.marginBottom 10]
                        ]
                    Html.button [
                        prop.text "Create a character"
                        prop.onClick(thunk1 dispatch (Navigate "chargen/adnd"))
                        ]
                    class' Html.div "growToFill" [
                        class' Html.div "existingCharacters" [
                            for ch in model.roster do
                                let txt, flair, cssClass =
                                    match ch with
                                    | { ruleset = Ruleset.TSR; draft = Some { name = name } } ->
                                        name, "AD&D", "flairADND"
                                    | { ruleset = Ruleset.WOTC; draft = Some { name = name } } ->
                                        name, "D&D 5E", "flairDND5e"
                                    | _ -> shouldntHappen()
                                Html.span [
                                    prop.text flair
                                    prop.className cssClass
                                    ]
                                Html.span [prop.text txt; prop.className "characterName"]
                                Html.button [
                                    prop.text $"Resume"
                                    prop.className "resumeCommand"
                                    prop.onClick (fun _ ->
                                        Page.Generate ch |> Open |> dispatch
                                        )
                                    ]
                                Html.button [
                                    prop.text $"Delete"
                                    prop.className "deleteCommand"
                                    prop.onClick (thunk1 dispatch (DeleteCharacter txt))
                                    ]

                            if model.roster.Length > 0 then
                                Html.button [prop.text "Delete all characters"; prop.className "deleteAllCommand"; prop.onClick (thunk1 dispatch ClearRoster)]
                            ]
                        ]

                    Html.div [
                        prop.className "footer"
                        prop.children [
                            Html.a [prop.href "https://www.flaticon.com/free-icons/sword"; prop.text "Sword icon created by pongsakornRed - Flaticon"]
                            ]
                        ]
                    ]
               ]

module Url =
    open Chargen.View
    open App
    module Parse =
        open Browser.Types
        open Packrat
        let locationParser (rootActivePattern: ParseRule<_>) (loc: Location) =
            let (|Root|_|) = rootActivePattern
            match ParseArgs.Init loc.hash with
            | Str "#" (Root(v, End)) -> v
            | _ -> []

        let (|Page|_|) = function
            | Str "chargen/adnd/DarkSun" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Open (Page.Generate model'))
                        Cmd.ofMsg (SetMethod DarkSunMethodI |> Chargen)
                        msg |> Cmd.map Chargen
                        ]
                Some(cmd, ctx)
            | Str "chargen/adnd" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Open (Page.Generate model'))
                        msg |> Cmd.map Chargen
                        ]
                Some(cmd, ctx)
            | Str "chargen/5e" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Open (Page.Generate model'))
                        msg |> Cmd.map Chargen
                        Cmd.ofMsg (Chargen (SetRuleset Domain.Character.Ruleset.WOTC))
                        ]
                Some(cmd, ctx)
            | Str "chargen" ctx ->
                let model', msg = Chargen.View.init()
                let cmd =
                    [
                        Cmd.ofMsg (Open (Page.Generate model'))
                        msg |> Cmd.map Chargen
                        ]
                Some(cmd, ctx)
            | _ -> None

        let page = locationParser (|Page|_|)
    let parse loc =
        let parsed = Parse.page loc
        parsed
    let unpack cmds model =
        model, Cmd.batch cmds

open App
open Elmish
open Elmish.Navigation

Program.mkProgram init update view
|> Program.withSubscription(fun m -> Cmd.ofSub(fun dispatch ->
    Browser.Dom.window.onerror <-
    fun msg ->
        if msg.ToString().Contains "SocketProtocolError" = false then
            dispatch (sprintf "Error: %A" msg |> Error)
            Browser.Dom.window.alert ("Unhandled Exception: " + msg.ToString())
        ))
|> Program.toNavigable Url.parse Url.unpack
|> Program.withReactBatched "feliz-app"
|> Program.run
