module Dev.App.Tracker.UI.Components

open Dev.App.Tracker
open Game.DataTypes
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz

open Fable.Core.JsInterop
open Fable.Core
open Feliz.Router
let class' (className: string) ctor (children: ReactElement list) =
    ctor [prop.className className; prop.children children]

type StringWithLinebreaks = string // a string where newline should mean <br> or similar
let withHelp showHelp (helpText: StringWithLinebreaks) sendToggleMessage otherwise =
    React.fragment [
        if showHelp then
            Html.div [
                for line in helpText.Split("\n") do
                    Html.div line
                Html.button [prop.text "OK"; prop.onClick(fun _ -> sendToggleMessage false)]
                ]
        else
            class' "header" Html.div [
                Html.a [prop.text "Help"; prop.onClick (fun _ -> sendToggleMessage (not showHelp))]
                ]
            yield! otherwise
        ]

let textHeaders (columns: string list) =
    Html.thead [
        Html.tr [
            for c in columns do
                Html.th [prop.text c]
            ]
        ]
let textCell (txt: string) = Html.td [prop.text txt]

let editableNumberCell (txt: string) supplyNumber =
    Html.td [
        Html.input [prop.valueOrDefault txt; prop.onChange (fun (txt:string) -> match System.Int32.TryParse(txt) with true, hp -> supplyNumber hp | _ -> ())]
        ]
