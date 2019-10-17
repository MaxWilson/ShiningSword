#load "Common.fs"
open Common
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
    type Data = Map<Key, Value>
    type DataResult = { data: Data; value: Value } with
        static member Create(d,v) = { data = d; value = v }
    type PropertyDefinition = {
        name: PropertyName
        defaultValue: Key -> Data -> DataResult
        }
    let parseNumber input =
        match System.Int32.TryParse input with
        | true, x -> Number x |> Some
        | _ -> None
    let parseText input =
        if System.String.IsNullOrWhiteSpace input then None
        else Some (Text <| input.Trim())
    let nameList = { name = "NameList"; defaultValue = fun _ d -> DataResult.Create(d, List [||]) }
    let empty : Data = Map.empty
    let write property rowId v data =
        DataResult.Create(data |> Map.add (rowId, property.name) v, v)
    let tryRead property rowId data : DataResult option =
        let key = (rowId, property.name)
        match data |> Map.tryFind key with
        | Some v ->
            DataResult.Create(data, v) |> Some
        | None ->
            None
    let read property rowId data : DataResult =
        match data |> tryRead property rowId with
        | Some v -> v
        | None ->
            let { data = data; value = v } = property.defaultValue (rowId, property.name) data
            write property rowId v data
    let rec private inquireBase getName parser (key: Key) data =
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
                | { data = data; value = List names } when names.Length > 0 ->
                    DataResult.Create(data, chooseRandom names)
                | _ -> inquireBase getNameSubstitute parseText (Some rowId, "Name") data
            | _ -> failwith "Name doesn't make sense at the global scope"
        { name = "Name"; defaultValue = generateNameOrAsk }
    let inquire = inquireBase (read name)

open Scope
let d = empty
let str = { PropertyDefinition.name = "Strength"; defaultValue = inquire parseNumber }
d |> read str (Some 11)


module REPL =
    type Command = Command
    let parse (input: string): Command option =
        Some Command
    let exec (cmd: Command) (data: Data) : Data =
        data


