[<AutoOpen>]
module Domain.Character.DungeonFantasy.Core

open Domain
open Domain.Ribbit.Properties
open Domain.Character
open Domain.Character.DungeonFantasy.TraitsAndAttributes
open Domain.Character.DungeonFantasy.Templates
open Domain.Character.DungeonFantasy.Templates.Templates

type RandomizationMethod = NonRandom | Exponential | Average3d6
let rollStats method =
    match method with
    | NonRandom ->
        freshFrom(10, 10, 10, 10)
    | Exponential ->
        let rec exponentialHalt accum stepSize rate =
                if random.NextDouble() <= rate && abs accum <= 5 then
                    exponentialHalt (accum + stepSize) stepSize rate
                else
                    accum
        let gen() = 10 + exponentialHalt 0 (chooseRandom [-1; +1]) 0.5
        freshFrom(gen(), gen(), gen(), gen())
    | Average3d6 ->
        let gen() = (List.init 6 (thunk1 rand 6) |> List.sum) / 2
        freshFrom(gen(), gen(), gen(), gen())

type Character = {
    id: string
    header: RoleplayingData
    profession: Profession
    canChangeRace: bool
    race: Race
    stats: Stats.Attributes
    traits: Trait list
    }

type 't Constraint = Arbitrary | Specific of 't
type 't Preference = Prefer of 't | Require of 't
type Constraints = {
    randomizationMethod: RandomizationMethod
    race: Race Package Preference option
    sex: Sex Constraint
    nationPreference: string option
    professionPreference: Profession option
    }
    with
    static member fresh = {
        randomizationMethod = NonRandom
        race = None
        sex = Arbitrary
        nationPreference = None
        professionPreference = None
        }

let clear (char: Stats.Attributes) =
    {   Stats.fresh with
            ST = char.ST.clear
            DX = char.DX.clear
            IQ = char.IQ.clear
            HT = char.HT.clear
            SM = char.SM.clear }
let materialize fallback = function Arbitrary -> fallback() | (Specific v) -> v
let createRandom (c: Constraints) =
    let prof =
        match c.professionPreference with
        | None -> chooseRandom professions.Keys
        | Some p -> p

    let stats = rollStats c.randomizationMethod
    let race =
        match c.race with
        | None -> chooseWeightedRandom races
        | Some (Require r | Prefer r) -> r
    let sex = c.sex |> materialize (thunk1 chooseRandom [Male; Female])
    let nation, name =
        match c.nationPreference with
        | None ->
            makeNameAnyNation sex
        | Some nation ->
            // try again
            match makeName nation sex with
            | Some name -> nation, name
            | None ->
                // Maybe user specified an invalid combination. Give up on nation preference.
                match makeName nation sex with
                | Some name -> nation, name
                | None -> makeNameAnyNation sex

    let rp = {
        RoleplayingData.name = name
        sex = sex
        nationalOrigin = nation
        }

    {   id = System.Guid.NewGuid().ToString()
        header = rp
        profession = prof
        race = race.name
        canChangeRace = match c.race with None | Some (Prefer _) -> true | _ -> false // Can't change race if it's locked to something specific via Require
        stats = stats |> clear |> race.apply |> professions[prof].apply
        traits = race.traits @ professions[prof].traits
        }

let resetStatsAndTraits (char: Character) =
    let race = Templates.races |> List.find (fun (_, r) -> r.name = char.race) |> snd
    let prof = professions[char.profession]
    { char
        with
        stats = char.stats |> clear |> race.apply |> prof.apply
        traits = race.traits @ prof.traits}

let changeProfession prof (char: Character) =
    { char with profession = prof } |> resetStatsAndTraits

let changeRace race (char: Character) =
    { char with race = race } |> resetStatsAndTraits

