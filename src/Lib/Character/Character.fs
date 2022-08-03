[<AutoOpen>]
module Domain.Character.Core

open DerivedTraits

[<Measure>] type gp
[<Measure>] type xp

type Stat = Str | Dex | Con | Int | Wis | Cha
    with static member All = [Str;Dex;Con;Int;Wis;Cha]
type Sex = Male | Female | Neither
type Name = string
type Origin = { ruleSystem: string; nationalOrigin: string; startingLevel: int; statRollMethod: string }

type RollSpec = StaticBonus of int | RollSpec of n:int * d:int * rest: RollSpec option
    with
    member this.roll() =
        let rec loop = function
            | StaticBonus n -> n
            | RollSpec(n,d,rest) ->
                let sum = List.init (abs n) (thunk1 rand d) |> List.sum
                let sum = if n < 0 then -sum else sum
                match rest with | Some rest -> sum + loop rest | None -> sum
        loop this
    override this.ToString() =
        let rec loop needsOperator = function
            | StaticBonus n -> if needsOperator && n >= 0 then $"+{n}" else n.ToString()
            | RollSpec(n,d,rest) ->
                let prefix = if needsOperator && n >= 0 then $"+{n}d{d}" else $"{n}d{d}"
                match rest with
                | None | Some (StaticBonus 0) -> prefix
                | Some rest -> $"{prefix}{loop true rest}"
        loop false this
    static member create(bonus) = StaticBonus bonus
    static member create(n,d) = RollSpec(n,d,None)
    static member create(n,d,bonus) =
        if bonus <> 0 then RollSpec(n,d,Some (StaticBonus bonus))
        else RollSpec(n,d,None)
    static member create(n,d,rest) = RollSpec(n,d,Some (rest))
    static member (+)(lhs, rhs: int) =
        let rec addBonus (bonus: int) = function
            | StaticBonus n -> StaticBonus (n + bonus)
            | RollSpec(n, d, None) -> RollSpec(n, d, Some (StaticBonus bonus))
            | RollSpec(n, d, Some rest) -> RollSpec(n, d, addBonus bonus rest |> Some)
        addBonus rhs lhs
    static member (+)(lhs, rhs: RollSpec) =
        let rec addRhs = function
            | StaticBonus n -> (rhs + n)
            | RollSpec(n, d, None) -> RollSpec(n, d, Some rhs)
            | RollSpec(n, d, Some rest) -> RollSpec(n, d, Some (addRhs rest))
        addRhs lhs
    static member (-)(lhs, rhs: int) = lhs + (-rhs)
    static member (-)(lhs, rhs: RollSpec) =
        let rec invert = function
            | StaticBonus n -> StaticBonus -n
            | RollSpec(n, d, None) -> RollSpec(-n, d, None)
            | RollSpec(n, d, Some rest) -> RollSpec(-n, d, invert rest |> Some)
        lhs + invert rhs

// turn camel casing back into words with spaces, for display to user
let uncamel (str: string) =
    let caps = ['A'..'Z'] |> Set.ofSeq
    let lower = ['a'..'z'] |> Set.ofSeq
    let mutable spaceNeededBefore = []
    let mutable inWord = true
    for i in 1..str.Length-1 do
        match str[i] with
        | ' ' -> inWord <- false
        // When multiple caps are in a row, no spaces should be used, except before the last one if it's followed by a lowercase.
        // E.g. MySSNNumber => My SSN Number, but MySSN => My SSN not My SS N
        | letter when caps.Contains letter && inWord && ((caps.Contains str[i-1] |> not) || i+1 < str.Length && lower.Contains str[i+1])->
            spaceNeededBefore <- i::spaceNeededBefore
        | letter when System.Char.IsLetterOrDigit letter -> inWord <- true
        | _ -> ()
    let rec recur workingCopy spacesNeeded =
        match spacesNeeded with
        | [] -> workingCopy
        | index::rest ->
            recur $"{workingCopy[0..(index-1)]} {workingCopy[index..]}" rest
    recur str spaceNeededBefore

