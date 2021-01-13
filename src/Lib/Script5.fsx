
#I __SOURCE_DIRECTORY__
#I "bin/debug/netstandard2.1"
#load @"Optics.fs"
#load @"Common.fs"

// If we want to do FSI development in the same pattern as React development, this script POCs the development process

let gameIter (model: byref<'t>) view update =
    let showView model = printfn "%s" (view model)
    let m = model
    model <- update model
    if m <> model then
        showView model

type GuessingGame = {
    theAnswer: int option
    tableStakes: int
    score: int
    }
    with static member fresh() = { theAnswer = None; tableStakes = 10; score = 0 }
type UI = {
    display: string
    state: GuessingGame
    }
    with static member fresh() = { display = "Please guess a number 1-1000"; state = GuessingGame.fresh() }
// This way is awkward because it ignores the first userInput
let guessingGame userInput (ui: UI) =
    match ui.state.theAnswer with
    | None -> 
        { ui with
            display = $"Please guess a number 1-1000";
            state = {
                ui.state with
                    theAnswer = rand 1000 |> Some;
                    tableStakes = 10;
                }
            }
    | Some n ->
        match n.CompareTo userInput with
        | 0 ->
            { ui with
                display = $"Correct! You earn {ui.state.tableStakes} points!";
                state = {
                    ui.state with
                        theAnswer = None;
                        score = ui.state.score + ui.state.tableStakes;
                    }
                }
        | _ when ui.state.tableStakes = 1 ->
            { ui with
                display = $"Sorry! You're out of guesses. The answer was {n}.";
                state = {
                    ui.state with
                        theAnswer = None;                        
                    }
                }
        | n when n > 0 ->
            { ui with
                display = "Higher!"
                state = {
                    ui.state with tableStakes = ui.state.tableStakes - 1
                    }
                }
        | n ->
            { ui with
                display = "Lower!"
                state = {
                    ui.state with tableStakes = ui.state.tableStakes - 1
                    }
                }

let mutable game = UI.fresh()

let guess n = gameIter &game (fun ui -> $"{ui.display}          \t[Score: {ui.state.score}]") (guessingGame n)
guess 500
guess 750
guess 625

guess 690
guess 725
guess 740

guess 733
guess 737











