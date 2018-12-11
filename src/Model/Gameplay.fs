module Model.Gameplay
open Interaction
open Model.Types
open Model.Operations
open Common

let queryInteraction = Interaction.InteractionBuilder<Query, string>()

let game() : Eventual<_,_,_> =
    let mutable target = rand 1000
    let mutable victories = 0
    let rec victory(): Eventual<_,_,_> = queryInteraction {
        victories <- victories + 1
        let! keepGoing = Query.confirm (sprintf "Hooray! You got the right answer. You've played %d rounds and won them all. Want to keep going?" victories)
        if keepGoing then
            target <- rand 1000 // choose a new target
            return! round 0
        else
            return ()
        }
    and round n : Eventual<_,_,_> = queryInteraction {
        let! guess = Query.number (sprintf "Guess a number! You have made %d guesses so far" n)
        if guess < target then
            do! Query.alert "Guess higher"
            return! round (n+1)
        else if guess > target then
            do! Query.alert "Guess lower"
            return! round (n+1)
        else
            return! victory()
        }
    round 0
