module Dev.Tracker.UI.Components

open Dev.Tracker
open Game.DataTypes
open Fable.Core.JsInterop
open Elmish
open Elmish.React
open Feliz

open Fable.Core.JsInterop
open Fable.Core
open Feliz.Router
let class' ctor (className: string) (children: ReactElement list) =
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
            class' Html.div "header" [
                Html.a [prop.text "Help"; prop.onClick (fun _ -> sendToggleMessage (not showHelp))]
                ]
            yield! otherwise
        ]

module CollapsibleSection =
    type 't d = { label: string; show: bool; setShowAsync: bool -> unit; view: unit -> ReactElement }
    let create(label, show, set) view = { label = label; show = show; setShowAsync = set; view = view }
    let render (section: _ d) =
        if not section.show then
            class' Html.div $"collapsible {section.label} closed" [
                Html.button [
                    prop.text $"Show {section.label}"
                    prop.onClick (thunk1 section.setShowAsync true)
                    ]
                ]
        else
            Html.div [
                prop.className $"collapsible {section.label} open"
                prop.children [
                    section.view()
                    ]
                prop.onDoubleClick (thunk1 section.setShowAsync false)
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
