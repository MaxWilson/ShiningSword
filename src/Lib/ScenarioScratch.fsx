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
let HP : Prop<int> = { name = "HP" }
iter &model (spawn
    (logic
    {
        let! hp = read 2 HP
        return sprintf "Bob has %d HP" hp
    }))
iter &model (fulfill (2,HP) 27)
tryRead 2 HP model |> Option.get
