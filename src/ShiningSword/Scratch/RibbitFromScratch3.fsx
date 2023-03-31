module Rewrite

#I __SOURCE_DIRECTORY__
#I ".."
#I "../Core"
#load "Optics.fs"
#load "Common.fs"
open Optics
open type Optics.Operations

#r "nuget: Unquote"
open Swensen.Unquote
test <@ 2+2 = 4 @>

type ExecutionContext() =
    do notImpl()
    member this.d n = rand n

type Prop<'t>() =
    member _.get (ctx: ExecutionContext): 't = notImpl()
    member _.set (ctx: ExecutionContext) (v:'t): ExecutionContext = notImpl()

let hp = Prop<int>()
let self = Prop<int>()
let dx = Prop<int>()
let lightLevel = Prop<int>()
let nightVision = Prop<int>()
let toHit = Prop<int>()

let cutpoint name f (ctx: ExecutionContext) = notImpl()

type SuccessLevel = CritFail | Fail | Success | CritSuccess
type TestResult = TestResult of SuccessLevel * margin: int

let evaluateSuccess targetNumber roll =
    match roll with
    | _ when roll >= targetNumber + 10 -> CritFail
    | 18 -> CritFail
    | 17 when targetNumber < 16 -> CritFail
    | 17 -> Fail
    | 3 | 4 -> CritSuccess
    | 5 when targetNumber >= 15 -> CritSuccess
    | 6 when targetNumber >= 16 -> CritSuccess
    | _ when roll <= targetNumber -> Success
    | _ -> Fail
    |> fun result ->
        match result with
        | (Success | CritSuccess) -> (result, abs(targetNumber - roll))
        | Fail | CritFail -> (result, (roll - targetNumber) |> max 1) // failure margin is always at least 1 even if you needed an 18 and rolled a 17 or 18
        |> TestResult

let successRoll (ctx: ExecutionContext) targetNumber =
    let roll = (ctx.d 6) + (ctx.d 6) + (ctx.d 6)
    evaluateSuccess targetNumber roll

// this math is probably a little bit wrong so we spot check a few key points
[for targetNumber, roll in [17, 14;17, 17;16, 17;15, 17; 15, 18; 3, 18; 17, 18; 18, 18; -13, 3; 0, 3; 7, 3; 7,4; 14, 5; 15, 5; 16, 5; 6, 15; 6, 16; 6, 17] do
    targetNumber, roll, (evaluateSuccess targetNumber roll)
    ]
let attack (ctx: ExecutionContext) =
    (dx.get ctx)
    |> toHit.set ctx
    |> cutpoint "toHit" (fun (ctx: ExecutionContext) ->
        let penalty =
            match lightLevel.get ctx with
            | level when level <= -10 -> -10
            | level -> level + nightVision.get ctx |> min 0 // 0 to -10
        (toHit.get ctx) - penalty
        )
    |> toHit.set ctx
    |> cutpoint "attack" (fun (ctx: ExecutionContext) ->
        match successRoll ctx (toHit.get ctx) with
        | TestResult((Success|CritSuccess), _) ->
            ctx |> cutpoint "rollDamage" (fun ctx -> printfn "Hit!"; notImpl())
        | _ -> printfn "Miss!"; notImpl()
        )


