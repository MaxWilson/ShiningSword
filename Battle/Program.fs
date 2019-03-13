// Learn more about F# at http://fsharp.org

open System
open Common
open Common.Hierarchy
open Model.Functions
open Model.Types.Battle2
open DataEngine

type CloudStorage() =
    let folderPath = IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData), "shiningsword")
    let accessTokenPath = IO.Path.Combine(folderPath, "accessToken.txt")
    let mutable _accessToken = None
    let accessToken() =
        match _accessToken with
        | Some v -> v
        | None ->
            let token =
                if IO.File.Exists(accessTokenPath) then
                    IO.File.ReadAllText(accessTokenPath)
                else
                    let rec loop() =
                        printf "Enter WilsonData access token: "
                        let token = Console.ReadLine().Trim()
                        if token.Length > 0 then
                            printfn "Storing '%s' in %s" token accessTokenPath
                            IO.Directory.CreateDirectory(folderPath) |> ignore
                            IO.File.WriteAllText(accessTokenPath, token)
                            token
                        else loop()
                    loop()
            _accessToken <- Some token
            token
    interface IDataStorage with
        member this.Save (label:Label) data callback =
           DataStorage.save (accessToken()) "battle" label data |> callback
        member this.Load label callback =
            DataStorage.load (accessToken()) "battle" label |> callback

let display detailLevel logEntries =
    for (cmd, e) in logEntries do
        let rec help currentDepth logEntry =
            match cmd with
            | Log(_) when currentDepth = 0 -> printf "* " // visually distinguish log entries from commands, among other things so that a user can recognize commands that failed to parse
            | _ -> ()
            printfn "%s%s" (String.replicate currentDepth "  ") (Log.getText logEntry)
            match logEntry with
            | Nested(_, children) when detailLevel > currentDepth -> // if detailLevel = 2, last recursion will happen when currentDepth = 1.
                children |> List.iter (help <| currentDepth + 1)
            | _ -> ()
        help 0 e

let consoleLoop (initialState: State) =
    let rec loop (state: State) =
        if state.view.finished then
            ()
        else
            match state.view.lastCommand, state.view.lastOutput with
            | Some (Log _), [_, Leaf _] -> () // if they just logged some simple text, don't echo it to output
            | _, outputs->
                display state.view.logDetailLevel outputs
            | None, _ when state.view.lastInput.IsSome -> printfn "Come again?" // probably shouldn't happen
            | _ -> ()
            let answer = execute (CloudStorage()) state
            printf ">> "
            let cmd = System.Console.ReadLine()
            answer cmd loop
    loop initialState

[<EntryPoint>]
let main argv =
    Console.SetIn(new System.IO.StreamReader(Console.OpenStandardInput(8192))) // allow more than 256 characters of input
    Model.Functions.Battle2.init() |> consoleLoop
    0
