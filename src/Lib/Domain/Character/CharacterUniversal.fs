module Domain.Character.Universal

open Core.DerivedTraits
open Domain

type Detail<'t1, 't2, 't3> = Detail2e of 't1 | Detail5e of 't2 | DetailDF of 't3
    with
    member this.isADND = match this with Detail2e _ -> true | Detail5e _ -> false | DetailDF _ -> false
    member this.is5E = match this with Detail2e _ -> false | Detail5e _ -> true | DetailDF _ -> false
    member this.df = match this with Detail2e _ -> false | Detail5e _ -> false | DetailDF _ -> true
    member this.map2e (f: ('t1 -> 't1)) =
        match this with
        | Detail2e instance -> Detail2e (f instance)
        | unchanged -> unchanged
    member this.map5e (f: ('t2 -> 't2)) =
        match this with
        | Detail5e instance -> Detail5e (f instance)
        | unchanged -> unchanged
    member this.mapDF (f: ('t3 -> 't3)) =
        match this with
        | DetailDF instance -> DetailDF (f instance)
        | unchanged -> unchanged
    member this.map (f: ('t1 -> 't1)) =
        match this with
        | Detail2e instance -> Detail2e (f instance)
        | unchanged -> unchanged
    member this.map (f: ('t2 -> 't2)) =
        match this with
        | Detail5e instance -> Detail5e (f instance)
        | unchanged -> unchanged
    member this.map (f: ('t3 -> 't3)) =
        match this with
        | DetailDF instance -> DetailDF (f instance)
        | unchanged -> unchanged
    member this.converge (f2e, f5e, fdf) =
        match this with
        | Detail2e data -> f2e data
        | Detail5e data -> f5e data
        | DetailDF data -> fdf data
    member this.raw =
        match this with
        | Detail2e data -> Detail2e()
        | Detail5e data -> Detail5e()
        | DetailDF data -> DetailDF()

let (|IsADND|_|) = function
| Detail2e x -> Some x
| _ -> None
let (|Is5e|_|) = function
| Detail5e x -> Some x
| _ -> None

type CharacterSheet2e = ADND2nd.CharacterSheet
type CharacterSheet5e = DND5e.CharacterSheet
type CharacterSheetDF = { name: string; id: int option }
type CharacterSheet = Detail<CharacterSheet2e, CharacterSheet5e, CharacterSheetDF>
type Trait2e = ADND2nd.Trait
type Trait5e = DND5e.Trait
type Trait = Detail<Trait2e, Trait5e, unit>
type DerivationInstance2e = DerivationInstance<Trait2e>
type DerivationInstance5e = DerivationInstance<Trait5e>
type DerivationInstance = Detail<DerivationInstance2e, DerivationInstance5e, unit>
type PreconditionContext2e = ADND2nd.PreconditionContext
type PreconditionContext5e = DND5e.PreconditionContext
let rules2e: DerivationRules<Trait2e,PreconditionContext2e> = ADND2nd.rules
let rules5e: DerivationRules<Trait5e,PreconditionContext5e> = DND5e.rules
let (|GenericCharacterSheet|) = function
    | Detail2e (char: ADND2nd.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; origin = char.origin; sex = char.sex; exceptionalStrength = char.exceptionalStrength; hp = char.hp; ac = char.ac |}
    | Detail5e (char: DND5e.CharacterSheet) -> {| name = char.name; Str = char.Str; Dex = char.Dex; Con = char.Con; Int = char.Int; Wis = char.Wis; Cha = char.Cha; origin = char.origin; sex = char.sex; exceptionalStrength = None; hp = char.hp; ac = char.ac |}
    | DetailDF (char: CharacterSheetDF) -> {| name = char.name; Str = 10; Dex = 10; Con = 10; Int = 10; Wis = 10; Cha = 10; origin = { Domain.Character.Core.Origin.ruleSystem = "DFRPG"; nationalOrigin = "N/A"; startingLevel= 0; statRollMethod="N/A" }; sex = Male; exceptionalStrength = None; hp = [|10,10|]; ac = 10 |}

let recompute (char: CharacterSheet) =
    (char.map2e ADND2nd.recompute).map5e DND5e.recompute
let levelUp (char: CharacterSheet) =
    (char.map2e ADND2nd.levelUp).map5e DND5e.levelUp
let xpNeeded (char: CharacterSheet) =
    char.converge (ADND2nd.xpNeeded, DND5e.xpNeeded, thunk 0<Metrics.xp>)

let randomNation _ = chooseRandom ["Tir na n'Og"; "Abysia"; "Kailasa"; "Ermor"; "Undauntra"; "Arboria"; "Mordor"]

let makeName (nationOfOrigin: string) (sex: Sex) =
    let rec chooseFromLists =
        function
        | potentialCategory::rest ->
            match Onomastikon.nameLists |> Map.tryFind (nationOfOrigin, potentialCategory) with
            | Some nameList -> chooseRandom nameList
            | None -> chooseFromLists rest
        | [] -> "" // sometimes e.g. there is no last name for a given national origin
    let firstName = chooseFromLists [sex.ToString()]
    match firstName with
    | "" -> None // invalid nation/sex combination (e.g. no females in Mordor), try again
    | _ ->
        let lastName name =
            let surname = chooseFromLists [$"Last";$"Cognomen{sex}";$"Last{sex}";]
            $"{name} {surname}".Trim()
        let prefix name =
            let prefixes = ["Insanity"; "Black"; "Merciless"; "Gentle"; "Calamity"]
            $"{chooseRandom prefixes} {name}".Trim()
        let title name =
            let suffixes = ["Defender of Humanity"; "Last of the Dwarflords"; "the Accursed"; "Esquire"; "the Undying"]
            $"{name}, {chooseRandom suffixes}".Trim()
        let allThree = (prefix >> lastName >> title)
        chooseRandomExponentialDecay 0.4 Seq.head [lastName; (lastName >> title); prefix; allThree] firstName
        |> Some

let makeNameAnyNation sex =
    let rec recur nation =
        match makeName nation sex with
        | None -> recur (randomNation())
        | Some name -> nation, name
    recur (randomNation())
