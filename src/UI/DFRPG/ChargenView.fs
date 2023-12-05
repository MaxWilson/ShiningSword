module UI.DFRPG.Chargen.View
open Feliz
open UI.DFRPG.Chargen

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update

    Html.div [
        let rec renderOffer = function
            | OneOf offers ->
                Html.ul [
                    for offer in offers do
                        Html.li [prop.children [renderOffer offer]]
                ]
            | Offer offer ->
                Html.text (offer.ToString())
        let rec renderOffers = function
            | ChooseBudget (points, offers) ->
                Html.div [
                    Html.div [prop.text $"Choose {points} points:"]
                    for offer in offers do renderOffer offer
                ]
            | ChooseN (n, offers) ->
                Html.div [
                    Html.div [prop.text $"Choose {n}"]
                    for offer in offers do renderOffer offer
                ]
        for offers in model.template do
            renderOffers offers
        ]