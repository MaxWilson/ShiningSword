// This file is for playing interactively with scenarios, similarly to unit tests
// but with more interactive intent, either for fun, for API usability testing, or
// as a precursor to writing or updating unit tests.

#I __SOURCE_DIRECTORY__
#load @"Optics.fs"
#load @"Common.fs"
#load @"AutoWizard.fs"
#load @"Domain\Model.fs"
#load @"Domain\AutoGen.fs"
#load @"Domain\Augment.fs"
#load @"Domain\RuleEngine.fs"
open Domain.RuleEngine
open Domain.RuleEngine.Logic
open Domain.Model
open Domain.Model.Ribbit
open Domain.RuleEngine
open Domain.RuleEngine.Logic
open Domain.RuleEngine.Logic.Builder

let mutable model = Ribbit.State.fresh
let HP = intProp "HP"
let Target = rowKeyProp "Target"
let Amount = intProp "Amount"
iter &model (defineAffordance "Take Damage" [Target; Amount]
    (logic
    {
        let! target = readCurrent Target
        let! amount = readCurrent Amount
        let! hp = read target HP
        do! write target HP (hp - amount) 
        return sprintf "Bob takes %d damage, has %d HP left" amount hp
    }))
iter &model (triggerAffordance "Take Damage" [Target, 2])
iter &model (fulfill (2,HP) 27)
tryRead 2 HP model |> Option.get = 25
