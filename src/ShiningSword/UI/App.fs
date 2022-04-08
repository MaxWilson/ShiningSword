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
        | Open of Page * url: string option
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
        | Chargen.View.UpdateUrl suffix ->
            UpdateUrl $"chargen/{suffix}"
            |> dispatch
        | Chargen.View.BeginAdventuring adv ->
            Transform (fun m -> { m with current = Page.Adventure adv }) |> dispatch
            UpdateUrl "/" |> dispatch // TODO: make a URL router directly to character's adventure
    let update msg model =
        match msg with
        | Error msg -> { model with error = Some msg }, Cmd.Empty
        | Transform f -> { f model with error = None }, Cmd.Empty
        | Navigate url ->
            model, Navigation.Navigation.newUrl ("#" + url)
        | UpdateUrl url ->
            model, Navigation.Navigation.modifyUrl ("#" + url)
        | AddOrUpdateRoster chargenModel ->
            // recent updated entries should be at the head of the list, so filter and then re-add at front
            let roster' =
                model.roster
                |> Array.filter (function
                    | { draft = Some { name = name } } when chargenModel.draft.IsSome && name = chargenModel.draft.Value.name -> false
                    | _ -> true)
            let roster' = Array.append [|chargenModel|] roster'
            LocalStorage.write roster'
            { model with roster = roster' }, Cmd.Empty
        | ClearRoster ->
            let roster' = Array.empty
            LocalStorage.write roster'
            { model with roster = roster' }, Cmd.Empty
        | DeleteCharacter characterName ->
            let roster' = model.roster |> Array.filter (function { draft = Some { name = name' } } when name' = characterName -> false | _ -> true)
            LocalStorage.write roster'
            { model with roster = roster' }, Cmd.Empty
        | GoHome ->
            { model with current = Home}, Navigate "/" |> Cmd.ofMsg
        | Open(page, Some url) -> { model with current = page}, Navigate url |> Cmd.ofMsg
        | Open(page, None) -> { model with current = page}, Cmd.Empty
        | Chargen msg ->
            match model.current with
            | (Page.Generate chargenModel) ->
                let cmd = (Chargen >> Cmd.ofMsg)
                let chargenModel, cmd = Chargen.View.update (Chargen >> Cmd.ofMsg) (flip chargenControl >> Cmd.ofSub) msg chargenModel
                { model with current = (Page.Generate chargenModel)}, cmd
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
                        // "remember" the user's ruleset preference
                        prop.onClick(fun _ ->
                            if model.roster.Length = 0 || model.roster[0].ruleset = Chargen.View.TSR then
                                Open(Page.Generate (Chargen.View.init()), Some "chargen/adnd") |> dispatch
                            else
                                Open(Page.Generate (Chargen.View.init()), Some "chargen/5e") |> dispatch
                                Chargen(Chargen.View.SetRuleset Chargen.View.WotC) |> dispatch
                            )
                        ]
                    class' Html.div "growToFill" [
                        class' Html.div "existingCharacters" [
                            for ch in model.roster do
                                let txt, flair, cssClass =
                                    match ch with
                                    | { ruleset = Chargen.View.TSR; draft = Some { name = name } } ->
                                        name, "AD&D", "flairADND"
                                    | { ruleset = Chargen.View.WotC; draft = Some { name = name } } ->
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
                                        Open(Page.Generate ch, None) |> dispatch
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
                let model'= Chargen.View.init()
                let cmd = [
                    Open (Page.Generate model', None)
                    SetMethod DarkSunMethodI |> Chargen
                    ]
                Some(cmd, ctx)
            | Str "chargen/adnd" ctx ->
                let model' = Chargen.View.init()
                let cmd = [
                    Open (Page.Generate model', None)
                    ]
                Some(cmd, ctx)
            | Str "chargen/5e" ctx ->
                let model' = Chargen.View.init()
                let cmd = [
                    Open (Page.Generate model', None)
                    Chargen (SetRuleset Chargen.View.WotC)
                    ]
                Some(cmd, ctx)
            | Str "chargen" ctx ->
                let model' = Chargen.View.init()
                let cmd = [
                    Open (Page.Generate model', None)
                    ]
                Some(cmd, ctx)
            | Str "/" ctx | Str "" (End as ctx) ->
                Some([GoHome], ctx)
            | _ -> None

        let page = locationParser (|Page|_|)
    let parse loc =
        let parsed = Parse.page loc
        parsed |> List.map Cmd.ofMsg
    let unpack (cmds: Cmd<Msg> list) model =
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
