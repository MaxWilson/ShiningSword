#load "SketchUI.fsx"
open SketchUI
let r = System.Random()
type Model = {
    truth: int
    guesses: string list
    wins: int
    }
    with static member fresh() = { truth = r.Next(1, 10); guesses = []; wins = 0 }
let init _ = Model.fresh()
let update msg model =
    match msg with
    | n when model.truth = n ->
        let model = { Model.fresh() with wins = model.wins + 1 }
        printfn "You guessed it! {n} is correct. You've now won %d times!" model.wins
        model
    | n ->
        let feedback = if n < model.truth then $"{n}: Higher!" else $"{n}: Lower!"
        { model with guesses = model.guesses @ [feedback] }
let view model =
    $"You've won {model.wins} times. What you know about this next number: {model.guesses}"
let send, state = connect init update view

send 10
send 5
send 3
send 4

let view2 model =
    let fullGuesses = model.guesses |> List.map (sprintf "%A") |> String.concat ", "
    $"Score: {model.wins}. Hints: {fullGuesses}"
let send, state = reconnect state update view2
send 5
