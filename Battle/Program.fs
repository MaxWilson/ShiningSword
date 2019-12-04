// Learn more about F# at http://fsharp.org

open System
open Common
open Common.Hierarchy

type Label = string

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
    member this.Save (label:Label) data callback =
        DataStorage.save (accessToken()) "battle" label data |> callback
    member this.Load label callback =
        DataStorage.load (accessToken()) "battle" label |> callback

[<EntryPoint>]
let main argv =
    Console.SetIn(new System.IO.StreamReader(Console.OpenStandardInput(8192))) // allow more than 256 characters of input
    0
