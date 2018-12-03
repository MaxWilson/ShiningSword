module ShiningSword.Test.Interaction.Unit

open Xunit
open Interaction

[<Fact(DisplayName="EventualBindTest: Eventual.bind should yield correct results")>]
let EventualBindTest() =
    let iMax = 10
    let e =
        Eventual.bind (Final "Hi my name is ": Eventual<string, string, string>)
            (fun prefix ->
                let rec loop i (accum:string) : Eventual<string, _, _> =
                    if i > 0 then
                        Intermediate(accum, fun arg ->
                                        loop (i-1) (accum + i.ToString() + arg))
                    else
                        Final(accum)
                (loop iMax prefix))
    Assert.Equal(Eventual.resolveSynchronously (fun _ -> "Bob") e, "Hi my name is 10Bob9Bob8Bob7Bob6Bob5Bob4Bob3Bob2Bob1Bob")

// types for test scenario
type GetNumber = Query of string
type Confirmation = Query of string

type InteractionQuery =
    | Number of GetNumber
    | Confirmation of Confirmation

module Recognizer =
    open Packrat
    let (|Number|_|) = (|Int|_|)
    let (|Bool|_|) = function
        | Word(AnyCase("y" | "yes" | "true" | "t"), ctx) -> Some(true, ctx)
        | Word(AnyCase("n" | "no" | "false" | "f"), ctx) -> Some(false, ctx)
        | _ -> None

module Query =
    open Packrat

    let tryParse recognizer arg =
        match ParseArgs.Init arg |> recognizer with
        | Some(v, End) -> Some v
        | _ -> None

    let confirm txt =
        InteractionQuery.Confirmation(Confirmation.Query txt), (tryParse Recognizer.``|Bool|_|``)
    let getNumber txt =
        InteractionQuery.Number(GetNumber.Query txt), (tryParse Recognizer.``|Number|_|``)

#nowarn "40" // recursive getBurgers is fine
[<Theory>]
[<InlineData(4,true,0,"That will be $8.00 please")>]
[<InlineData(4,true,3,"Thanks! That will be $11.00 please")>]
[<InlineData(3,false,3,"Thanks! That will be $6.00 please")>]
let ``Simulated user interaction``(burgers, getFries, tip, expected) =

    let interaction = InteractionBuilder<string, InteractionQuery>()
    let rec getBurgers : Interactive<_,_,_> =
        interaction {
            let! burger = Query.confirm "Would you like a burger?"
            if burger then
                let! fries = Query.confirm "Would you like fries with that?"
                let price = 1 + (if fries then 1 else 0)
                let! more = getBurgers
                return price + more
            else
                return 0
        }
    let getOrder: Eventual<_, InteractionQuery, _> =
        interaction {
            let! price = getBurgers
            let! tip = Query.confirm "Would you like to leave a tip?"
            if tip then
                let! tip = Query.getNumber "How much?"
                return sprintf "Thanks! That will be $%d.00 please" (tip + price)
            else
                return sprintf "That will be $%d.00 please" price
        }
    let mutable burgerCount = 0
    let question = function
        | Confirmation(Confirmation.Query txt) ->
            if txt.Contains "burger" then
                if burgerCount < burgers then
                    burgerCount <- burgerCount + 1
                    "yes"
                else
                    "no"
            elif txt.Contains "tip" && tip > 0 then
                "yes"
            elif txt.Contains "fries" && getFries then
                "yes"
            else
                "no"
        | Number(GetNumber.Query txt) -> tip.ToString() // must always answer question by typing text
    Assert.Equal(expected, getOrder |> Eventual.resolveSynchronously question)
