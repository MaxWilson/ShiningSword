
//#I __SOURCE_DIRECTORY__
#I "."
#I ".."
#I "../Core"
#load @"Optics.fs"
#load @"Common.fs"

// If we want to do FSI development in the same pattern as React development, this script POCs the development process

let gameIter (model: byref<_>) view update =
    let m = model
    model <- update model
    view model

type 'msg Dispatch = 'msg -> unit
type 'msg Cmd = 'msg Dispatch -> unit
module Cmd =
    let ofMsg msg = fun d -> d msg
    let msg msg = [ofMsg msg]
    let none = []
let closureOfUpdate (update: 'msg -> 'model -> 'model * ('msg Cmd list)) (msg: 'msg) (model: 'model)
    : 'model =
    let rec recur model msgs msg =
        let mutable queue = []
        let dispatch msg = queue <- queue@[msg]
        let model, cmds' = update msg model
        for cmd in cmds' do
            cmd dispatch
        match msgs@queue with
        | [] -> model
        | headMsg::rest -> recur model rest headMsg
    recur model [] msg

type GuessingGame = {
    theAnswer: int
    tableStakes: int
    score: int
    }
    with static member fresh() = { theAnswer = rand 1000; tableStakes = 10; score = 0 }
type UI = {
    display: string
    state: GuessingGame
    }
    with static member fresh() = { display = "Please guess a number 1-1000"; state = GuessingGame.fresh() }
type GuessMsg = NewGame | Guess of guess:int
let rec guessingGame msg (ui: UI) =
    match msg with
    | NewGame -> 
        { ui with
            display = $"Please guess a number 1-1000";
            state = {
                ui.state with
                    theAnswer = rand 1000;
                    tableStakes = 10;
                }
            }, Cmd.none
    | Guess userInput ->
        match ui.state.theAnswer.CompareTo userInput with
        | 0 ->
            { ui with
                display = $"Correct! You earn {ui.state.tableStakes} points!";
                state = {
                    ui.state with
                        score = ui.state.score + ui.state.tableStakes;
                    }                
                }, Cmd.none
        | _ when ui.state.tableStakes = 1 ->
            { ui with
                display = $"Sorry! You're out of guesses. The answer was {ui.state.theAnswer}.";
                }, Cmd.none
        | n when n > 0 ->
            { ui with
                display = "Higher!"
                state = {
                    ui.state with tableStakes = ui.state.tableStakes - 1
                    }
                }, Cmd.none
        | n ->
            { ui with
                display = "Lower!"
                state = {
                    ui.state with tableStakes = ui.state.tableStakes - 1
                    }
                }, Cmd.none

let print = printfn "%s"
let view = (fun ui -> $"{ui.display}          \t[Score: {ui.state.score} Guesses left: {ui.state.tableStakes}]" |> print)
let update = closureOfUpdate guessingGame
let mutable game = UI.fresh()
(view game) // do the up-front print here
let cmd msg = gameIter &game view (update msg)
let guess n = cmd (Guess n)
guess 500
guess 750
guess 875
guess 950
guess 975
guess 965
guess 957
guess 953
cmd NewGame
guess 500
guess 750
guess 900
guess 952
guess 927
guess 914
guess 920
guess 917
cmd NewGame
guess 500
guess 750
guess 875
guess 940
guess 904
guess 890
guess 897
guess 893
guess 891
cmd NewGame
