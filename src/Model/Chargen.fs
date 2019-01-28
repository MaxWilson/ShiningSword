module Model.Chargen
open Model.Types
open Model.Names
open Model.Operations
open Interaction
open Common

let descriptions = function
    | HeavyArmorDamageResistance n ->
        sprintf "Damage resistance %d vs. nonmagical damage while wearing heavy armor" n
    | ASI(ability, bonus) ->
        sprintf "+%d to %s" bonus (ability.ToString())
    | Race race ->
        sprintf "You are a %A" race
    | Subrace subrace ->
        sprintf "You are a %A" subrace
    | ClassLevel(cl, n) ->
        sprintf "You are a level %d or higher %A" n cl
    | Faster n ->
        sprintf "Your speed increases by %d' per round" n
    | CharmResist ->
        "You have advantage on saving throws against charm effects"
    | NoSleep ->
        "You do not need to sleep, nor can magic put you to sleep"
    | ExtraHP n ->
        sprintf "You gain %d additional HP per level" n
    | PoisonResist -> "You have advantage on saving throws against poison, and you are resistant to poison damage"
    | MediumArmorProficiency -> "You are proficient in the use of medium armors like breastplates and half-plate"
    | Darkvision -> "You can see normally in shadows or dim light up to 60' away, and you can see dimly in darkness up to 60' away"
    | v -> sprintf "%A" v

type Consequent =
    | Grants of Feature // grants this other feature automatically
    | GrantsAll of Feature list // grants all of these features automatically
    | Choose of Feature list // grants ONE of the choices
    | ChooseN of N: int * Feature list // grants N of the choices

let featureGraph : (Feature * Consequent) [] = [|
    Race Dwarf, GrantsAll [ASI(Con, +2); PoisonResist; Darkvision]
    Race Dwarf, Choose [Subrace "Hill dwarf"; Subrace "Mountain dwarf"]
    Subrace "Hill dwarf", Grants (ASI(Wis, +1))
    Subrace "Hill dwarf", Grants (ExtraHP 1)
    Subrace "Mountain dwarf", Grants (ASI(Str, +2))
    Subrace "Mountain dwarf", Grants MediumArmorProficiency
    Race Elf, Choose [Subrace "Wood elf"; Subrace "High elf"]
    Race Elf, Grants (ASI(Dex, +2))
    Race Elf, Grants CharmResist
    Race Elf, Grants NoSleep
    Race Elf, Grants Darkvision
    Subrace "Wood elf", Grants (Faster 10)
    Subrace "Wood elf", Grants (ASI(Wis, +1))
    Subrace "High elf", Grants (ASI(Int, +1))
    Subrace "High elf", Grants (ExtraCantrip Wizard)
    Race HalfElf, Grants (ASI (Cha, +2))
    Race Human, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1)])
    Race HalfElf, Grants CharmResist
    Race HalfElf, Grants NoSleep
    Race Human, ChooseN (2, [ASI (Str, +1); ASI (Dex, +1); ASI (Con, +1); ASI (Int, +1); ASI (Wis, +1); ASI (Cha, +1)])
    Race Human, Grants Feat
    Feat, Choose [Sharpshooter; GreatWeaponMaster; HeavyArmorMaster; DefensiveDuelist; Mobile]
    HeavyArmorMaster, Grants (ASI (Str, +1))
    Feature.ClassLevel (Champion, 1), Choose [ArcheryStyle; DefenseStyle; DuelingStyle]
    Feature.ClassLevel (PurpleDragonKnight, 1), Choose [ArcheryStyle; DefenseStyle; DuelingStyle]
    |]

// do all the auto-grants
let grants features =
    let rec fixpoint features =
        let notAlreadyChosen feature =
            features |> List.contains feature |> not
        let grants =
            [for feature in features do
                yield feature
                for consequent in featureGraph |> Array.filter (fst >> (=) feature) |> Array.map snd do
                    match consequent with
                    | Grants feature ->
                        if notAlreadyChosen feature then yield feature
                    | GrantsAll features ->
                        for feature in features do
                            if notAlreadyChosen feature then yield feature
                    | _ -> ()
                ]
        if grants = features then features
        else fixpoint grants
    fixpoint features

// get all the choices that can legally be added, based on features already selected
let choices features =
    let chosen choices =
        choices |> Seq.filter (fun choice -> List.contains choice features) |> Seq.length
    let getChoices features =
        [for feature in features do
            for consequent in featureGraph |> Array.filter (fst >> (=) feature) |> Array.map snd do
                match consequent with
                | Choose choices when chosen choices < 1 ->
                     for c in choices do
                        yield c
                | ChooseN(n, choices) when chosen choices < n ->
                     for c in choices do
                        yield c
                | _ -> ()
            ]
    (grants features |> getChoices) |> List.distinct

let validate (race, classLevels) features =
    failwith "Not implemented"

module Templates =
    let charTemplates =
        [
        {   name = "Dwarven Einhere"; description = "The stout battleragers of Odin's Halls are famous for three things: their fury in battle, their loyalty to their bearded brethren, and their love of glittering things."
            statPriorities = (1, 5, 2, 6, 6, 4); race = Some Dwarf; advancementPriorities = List.init 20 (thunk Battlerager); featurePriorities = []; homeRegion = Some ["Abyisa";"Ermor"]
            }
        {   name = "Emerald Bodyguard"; description = "From the far north come exotic warriors sworn to protect and preserve. The most honorable are called samurai and deal swift death to their enemies."
            statPriorities = (3, 1, 2, 4, 4, 4); race = None; advancementPriorities = List.init 20 (thunk Samurai); featurePriorities = []; homeRegion = Some ["Kailasa"]
            }
        {   name = "Knight of the Round Table"; description = "From the lands of Albion and far-off Undauntra have come stout allies, armored knights pledged to courage and valor. Some among them can heal the wounded."
            statPriorities = (1, 3, 2, 4, 4, 4); race = Some Human; advancementPriorities = List.init 20 (thunk PurpleDragonKnight); featurePriorities = [HeavyArmorMaster]; homeRegion = Some ["Undauntra"]
            }
        {   name = "Brute"; description = "A hired thug, capable and deadly yet obedient to command."
            statPriorities = (1, 3, 2, 4, 4, 4); race = Some Human; advancementPriorities = List.init 20 (thunk Champion); featurePriorities = [HeavyArmorMaster;DefenseStyle]; homeRegion = None
            }
        {   name = "Wind Warrior"; description = "The Kuzarni clan ninjas of the far north, known as Wind Warriors by southrons, have many strange and mystical abilities. Some of them are able to stun enemies with a blow or even to hurl explosive blasts of flame."
            statPriorities = (4, 1, 2, 4, 1, 4); race = Some Human; advancementPriorities = List.init 20 (thunk Elemonk); featurePriorities = [HeavyArmorMaster]; homeRegion = Some ["Kailasa"]
            }
        {   name = "Border ranger"; description = "Border rangers rely on mobility and their mighty longbows to protect civilization from the monsters of the Outlands."
            statPriorities = (4, 1, 2, 4, 3, 4); race = None; advancementPriorities = List.init 20 (thunk Champion); featurePriorities = [Sharpshooter;ArcheryStyle;Mobile]; homeRegion = None
            }
        ]
        |> List.map (fun t -> t.name, t)
        |> Map.ofSeq

module Workflow =
    let queryInteraction = Interaction.InteractionBuilder<Query * GameState, string>()

    let getNameSexTemplate state firstPerson isFriend : Eventual<_,_,Name * Sex * CharTemplate * string option> = queryInteraction {
        let charTemplateChoice = ("Don't care"::(Templates.charTemplates |> Map.toList |> List.map fst))
        let randomTemplate region = chooseRandom (Templates.charTemplates |> Map.filter (fun _ t -> match t.homeRegion with Some regions -> List.contains region regions | _ -> true) |> Map.toArray |> Array.map snd)
        let findTemplate key =
            match Templates.charTemplates.TryFind key with
            | Some v -> v
            | None -> chooseRandom (Templates.charTemplates |> Map.toArray |> Array.map snd)
        if firstPerson then
            let! name = Query.text state "What's your name?"
            let! sex = Query.choose state "What's your sex?" [Male; Female]
            let! template = Query.choose state "What kind of adventurer are you?" charTemplateChoice
            return (name, sex, findTemplate template, None)
        elif isFriend then
            let! name = Query.text state "What's your friend's name?"
            let! sex = Query.choose state "What's their sex?" [Male; Female]
            let! template = Query.choose state (sprintf "What kind of adventurer is %s?" (match sex with Male -> "he" | Female -> "she")) charTemplateChoice
            return (name, sex, findTemplate template, None)
        else
            let! sex = Query.choose state "Are you looking for males or females? (This affects which regions you can recruit from.)" ["Don't care";"Male"; "Female"]
            let sex = match sex with "Don't care" -> chooseRandom [|Male;Female|] | "Male" -> Male | _ -> Female
            let eligibleLists = Model.Names.names |> List.filter (fun ((n,t),_) -> t = (sex.ToString()))
            let captions = (eligibleLists |> List.map (fst >> fst))
            let! nameType = Query.choose state "What region do you want to recruit from?" ("Don't care" :: captions)
            let nameType = match nameType with "Don't care" -> chooseRandom (Array.ofList captions) | v -> v
            let chosenList = eligibleLists |> List.pick (fun ((n,_),l) -> if nameType = n then Some l else None)
            let firstname = chooseRandom chosenList
            let lastname =
                match sex, Model.Names.names |> List.tryFind (fun ((n,t),_) -> (t = ("Last" + sex.ToString()) || t = "Last") && n = nameType) with
                | _, Some (_, lst) -> chooseRandom lst
                | Male, None -> sprintf "%s%s" (chooseRandom [|" ben ";" son ";" dak ";" s'";" "|]) (chooseRandom chosenList)
                | Female, None -> (chooseRandom [|" bat " + (chooseRandom chosenList); (chooseRandom chosenList) + "dotter"; "d'"+(chooseRandom chosenList); chooseRandom chosenList|])
            let template =
                randomTemplate nameType
            match Model.Names.names |> List.tryFind (fun ((n,t),_) -> t = ("Cognomen" + sex.ToString()) && n = nameType) with
            | Some (_, cognomens) ->
                let name = sprintf "%s %s %s" firstname lastname (chooseRandom cognomens)
                return (name, sex, template, Some nameType)
            | None ->
                let name = sprintf "%s %s" firstname lastname
                return (name, sex, template, Some nameType)
        }

    let rec newPC (state: GameState) firstPerson isFriend : Eventual<_,_,_> = queryInteraction {
        let! name, sex, template, region = getNameSexTemplate state firstPerson isFriend
        let stats =
            let r() = [for _ in 1..4 -> rand 6] |> List.sortDescending |> List.take 3 |> List.sum
            (r(),r(),r(),r(),r(),r())
        let pc = CharSheet.create name sex stats (not (firstPerson || isFriend)) region template |> CharInfo.ofCharSheet
        return pc
    }
