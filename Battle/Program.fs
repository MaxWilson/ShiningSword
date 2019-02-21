// Learn more about F# at http://fsharp.org

open System
open Common
open DataEngine
open Model.Types.Battle2

type LocalStorage() =
    interface IDataStorage with
        member this.Save label d = Common.notImpl()
        member this.Load label = Common.notImpl()

let consoleLoop (initialState: State) =
    let rec loop (state: State) =
        if state.view.finished then
            ()
        else
            let maybePrint = function
            | Some v -> printfn "%s" v
            | None -> ()
            match state.view.lastCommand, state.view.lastOutput with
            | Some _cmd, Some response ->
                printfn "%s" response
            | Some _cmd, None -> ()
            | None, _ when state.view.lastInput.IsSome -> printfn "Come again?" // probably shouldn't happen
            | _ -> ()
            let answer = execute (String.join "\n  ") (LocalStorage()) state
            printf ">> "
            let cmd = System.Console.ReadLine()
            loop (answer cmd)
    loop initialState

[<EntryPoint>]
let main argv =
    Model.Functions.Battle2.init() |> consoleLoop
    0
