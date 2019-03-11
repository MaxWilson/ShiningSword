// Learn more about F# at http://fsharp.org

open System
open Common
open DataEngine
open Model.Types.Battle2

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

let formatExplanation (explanation: Model.Types.Roll.Explanation) =
    let rec helper indent (Model.Types.Roll.Explanation(_, summary, children)) =
        String.join "\n" (sprintf "%s%s" indent summary::(children |> List.map (helper (indent+"  "))))
    helper emptyString explanation

let consoleLoop (initialState: State) =
    let rec loop (state: State) =
        if state.view.finished then
            ()
        else
            match state.view.lastCommand, state.view.lastOutput with
            | _, Some response ->
                printfn "%s" response
            | Some _cmd, None -> ()
            | None, _ when state.view.lastInput.IsSome -> printfn "Come again?" // probably shouldn't happen
            | _ -> ()
            let answer = execute (String.join "\n") formatExplanation (CloudStorage()) state
            printf ">> "
            let cmd = System.Console.ReadLine()
            answer cmd loop
    loop initialState

[<EntryPoint>]
let main argv =
    Console.SetIn(new System.IO.StreamReader(Console.OpenStandardInput(8192))) // allow more than 256 characters of input
    Model.Functions.Battle2.init() |> consoleLoop
    0
