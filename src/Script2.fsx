#load "Common.fs"
#load "Optics.fs"
open Common
open Optics
open Optics.Operations
module Scope =
    type PropertyName = string
    type RowId = int
    type Key = RowId option * PropertyName
    type Value =
        | Number of int | Text of string
        | List of Value[]
        | StructuredData of Map<string, Value>
        with
        static member AsText =
            function
                | Text v -> v
                | _ -> shouldntHappen()
        static member AsNumber =
            function
                | Number v -> v
                | _ -> shouldntHappen()
    type Data = {
        propertyValues: Map<Key, Value>
        outstandingQueries: Key list
        }
    module Lens =
        let PropertyValues = lens (fun d -> d.propertyValues) (fun v d -> { d with propertyValues = v })
    type DataResult = { data: Data; value: Value } with
        static member Create(d,v) = { data = d; value = v }
    let getData d = d.data
    let getValue d = d.value
    type QueryResult = Immediate of DataResult | Deferred of requiredValues: Key list
    type PropertyDefinition = {
        name: PropertyName
        defaultValue: Key -> Data -> QueryResult
        }
    let parseNumber input =
        match System.Int32.TryParse input with
        | true, x -> Number x |> Some
        | _ -> None
    let parseText input =
        if System.String.IsNullOrWhiteSpace input then None
        else Some (Text <| input.Trim())
    let nameList = { name = "NameList"; defaultValue = fun _ d -> Immediate <| DataResult.Create(d, List [||]) }
    let empty : Data = { propertyValues = Map.empty; outstandingQueries = [] }
    let write property rowId v data =
        DataResult.Create(data |> over Lens.PropertyValues (Map.add (rowId, property.name) v), v)
    let tryRead property rowId data : DataResult option =
        let key = (rowId, property.name)
        match data.propertyValues |> Map.tryFind key with
        | Some v ->
            DataResult.Create(data, v) |> Some
        | None ->
            None
    let read property rowId data : QueryResult =
        match data |> tryRead property rowId with
        | Some v -> Immediate v
        | None ->
            match property.defaultValue (rowId, property.name) data with
            | Immediate { data = data; value = v } ->
                Immediate (write property rowId v data)
            | deferred -> deferred
    let rec private inquireBase getName parser (key: Key) data =
        Deferred [key]
    let rec private inquire0 inquireBase getName parser (key: Key) data =
        let data =
            match key with
            | (Some id, propertyName) ->
                let dr: DataResult = getName (Some id) data
                let name = (dr.value |> Value.AsText)
                printfn "Enter %s's %s:" name propertyName
                data
            | None, propertyName ->
               printfn "Enter %s:" propertyName
               data
        match parser (System.Console.ReadLine()) with
        | Some v -> DataResult.Create(data, v)
        | _ ->
            printf "I don't understand. "
            inquireBase getName parser key data
    let name =
        let getNameSubstitute (rowId: RowId option) data = DataResult.Create(data, Text (sprintf "Creature #%d" rowId.Value))
        let generateNameOrAsk key data =
            match key with
            | Some rowId, _ ->
                match read nameList (Some rowId) data with
                | Immediate { data = data; value = List names } when names.Length > 0 ->
                    Immediate <| DataResult.Create(data, chooseRandom names)
                | _ -> Deferred [Some rowId, "Name"] // inquireBase getNameSubstitute parseText (Some rowId, "Name") data
            | _ -> failwith "Name doesn't make sense at the global scope"
        { name = "Name"; defaultValue = generateNameOrAsk }
    let inquire = inquireBase (read name)

open Scope

let d = empty
let getValue = function | Deferred _ -> None | Immediate dr -> Some dr.value
let str = { PropertyDefinition.name = "Strength"; defaultValue = inquire parseNumber }
d |> write str (Some 11) (Number 19) |> getData |> read str (Some 11) |> getValue


module REPL =
    type Command = Command
    let parse (input: string): Command option =
        Some Command
    let exec (cmd: Command) (data: Data) : Data =
        data


