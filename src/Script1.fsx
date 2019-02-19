#I __SOURCE_DIRECTORY__
#r "System.Net.Http"
#r "NewtonSoft.Json"
#r @"bin\Fable.JsonConverter.dll"
#load @"Common.fs"
#load @"Abstractions\Parsing.fs"
#load @"Abstractions\Interaction.fs"
#load @"Model\Types.fs"
#load @"Model\Dice.fs"
#load @"Model\Names.fs"
#load @"Model\Tables.fs"
#load @"Model\Operations.fs"
#load @"Model\Chargen.fs"
#load @"Model\Battle.fs"
#load @"Model\MonsterManual.fs"
#load @"Model\Gameplay.fs"
#load @"Engine\DataEngine.fs"
#load @"Abstractions\DataStorage.Globals.fs"
#load @"Abstractions\DataStorage.fs"

Model.MonsterManual.lookup "trex" ()
open Model
open Model.Types
let me = Model.Operations.CharSheet.create "Max" Male (18,18,18,14,12,13) false None (Model.Chargen.Templates.charTemplates.["Brute"])
let b = Battle.create |> Battle.addExistingCharacter TeamId.Blue (Model.Operations.CharInfo.ofCharSheet me)

type ActorId = Id
type TargetId = Id
type Target = Pos of Position | Creature of TargetId
type Intention =
    | Attack of Attack option * TargetId option
    | Dodge
    | Move of Target
    | Kill of Attack option * TargetId option // todo: this should probably be some kind of composability abstraction over Intentions, like "repeat until condition" or something
type Intentions = Intention list
type Declaration = Declaration of ActorId * Intentions
type Declarations = Declaration list
type DeclarationStatus = Success | Failure | Incomplete of Declarations
type Effect = Hit of ActorId * Effect | Miss of ActorId * TargetId | Move of ActorId * Position| Damage of TargetId * int | Chatter of ActorId option * string
type Outcome = { effects: Effect list; status: DeclarationStatus; log: string list }
type ExecOutcome = { effects: Effect list; unresolvedDeclarations: Declarations }

let combineOutcomes (outcomes: Outcome list) : ExecOutcome =
    { ExecOutcome.effects = outcomes |> List.collect (fun o -> o.effects);
        unresolvedDeclarations = outcomes |> List.collect (function { status = Incomplete(i) } -> i | _ -> [])}

let apply effects (battle: Battle) : Battle = Common.notImpl()

let exec (declarations: Declarations) (battle: Battle) : ExecOutcome =
    let exec (Declaration(id, intentions)) battle: Outcome =
        let resolve (battle: Battle) intention : Effect list * DeclarationStatus =
            match intention with
            | Attack(attack, target) ->
                Common.notImpl()
            | Dodge -> Common.notImpl()
            | Intention.Move(target) -> Common.notImpl()
            | Kill(attack, target) -> Common.notImpl()
        let rec exec battle effects = function
            | [] ->
                { effects = effects; status = Success; log = [] }
            | intention::rest ->
                let effects', status = resolve battle intention
                match status with
                | Success ->
                    exec (apply effects' battle) (effects'@effects) rest
                | Failure ->
                    // abort further processing on failure
                    { effects = effects; status = Failure; log = [] }
                | Incomplete(_) as st ->
                    // delay further processing on failure
                    { effects = effects; status = st; log = [] }
        exec battle [] intentions

    let rec loop declarations priorOutcomes battle =
        match declarations with
        | [] -> combineOutcomes priorOutcomes
        | d::rest ->
            let outcome = exec d battle
            let battle = apply outcome.effects battle
            loop rest (outcome::priorOutcomes) battle
    loop declarations [] battle

combineOutcomes [
    { effects = [Move(1, (0,0))]; status = Success; log = ["Bob charges over to the orc"] }
    { effects = [Hit(1,(Damage(2,10))); Miss(1, 2); Chatter (Some 1, "Kreeegah! Bob Bundolo!")]; status = Incomplete [Declaration(1, [Kill(None, Some 2)])]; log = [] }
    { effects = [Miss(2, 1); Chatter (None, "The orc looks nervously around")]; status = Incomplete [Declaration(2, [Kill(None, None)])]; log = [] }
    ]

let decls = [
    Declaration(1, [Intention.Move(Target.Creature(2))])
    Declaration(1, [Kill(None, Some 2)])
    Declaration(2, [Kill(None, None)])
    ]

exec decls b
