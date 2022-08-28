namespace Domain.Random

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
