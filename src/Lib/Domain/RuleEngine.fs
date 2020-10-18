module Domain.RuleEngine

type Logic<'state, 'demand, 'result> = 'state -> 'state * LogicOutput<'state, 'demand, 'result>
and LogicOutput<'state, 'demand, 'result> = Ready of 'result | Awaiting of demand:'demand * followup:Logic<'state, 'demand, 'result>

type Prop<'t> = { name: string }
module Logic =
    // Like Task.Continue or Async.Map
    let continueWith f logic =
        let rec continueWith logic state =
            match logic state with
            | state, Ready v ->
                f v state
            | state, Awaiting(demand, logic) ->
                state, Awaiting (demand, logic |> continueWith)
        continueWith logic
