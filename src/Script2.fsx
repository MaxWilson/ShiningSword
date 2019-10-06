module Scope =
    type PropertyName = string
    type RowId = int
    type Key = RowId option * PropertyName
    type Value = Number of int | Text of string
    type Data = Map<Key, Value>
    type DataResult = { data: Data; value: Value } with
        static member Create(d,v) = { data = d; value = v }
    type PropertyDefinition = {
        name: PropertyName
        defaultValue: Key -> Data -> DataResult
        }
    let write property rowId v data =
        DataResult.Create(data |> Map.add (rowId, property.name) v, v)
    let read property rowId data : DataResult =
        let key = (rowId, property.name)
        match data |> Map.tryFind key with
        | Some v ->
            DataResult.Create(data, v)
        | None ->
            let { data = data; value = v } = property.defaultValue key data
            write property rowId v data

            
        


type Data = { data: unit }
let fresh = { data = () }
type Command = Command
let parse (input: string): Command option =
    Some Command
let exec (cmd: Command) (data: Data) : Data =
    data


