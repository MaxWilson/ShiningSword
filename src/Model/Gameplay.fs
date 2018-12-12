module Model.Gameplay
open Interaction
open Model.Types
open Model.Operations
open Common

let queryInteraction = Interaction.InteractionBuilder<Query, string>()

let game() : Eventual<_,_,_> =
    let mutable target = rand 1000
    let mutable victories = 0
    let rec round n : Eventual<_,_,_> = queryInteraction {
        let! guess = Query.number (sprintf "Guess a number! You have made %d guesses so far. (Hint: the answer rhymes with %d)" n target)
        if guess < target - 100 then
            do! Query.alert "You are 100 or less off"
            return! round (n+1)
        elif guess > target + 100 then
            do! Query.alert "You are 100 or more off"
            return! round (n+1)
        elif guess > target then
            do! Query.alert "Guess lower"
            return! round (n+1)
        elif guess < target then
            do! Query.alert "Guess higher"
            return! round (n+1)
        else
            victories <- victories + 1000
            let! keepGoing = Query.confirm (sprintf "Hooray! You got the right answer. You've played %d rounds and won them all. Want to keep going?" victories)
            if keepGoing then
                target <- rand 1000 // choose a new target
                return! round 0
            else
                return ()
        }
    round 0
