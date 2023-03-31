#I __SOURCE_DIRECTORY__
#I ".."
#I "..\Core"
#load @"Optics.fs"
#load @"Common.fs"

type 't MenuItem =
    | Selected of key: string list * 't option
    | NotSelected of key: string list
    | List of label: string option * 't MenuItem list

type Ctx(data) =
    member this.Has (path:string list) = data |> Set.contains path

type Profession = Swashbuckler | Knight | Wizard | Cleric | MartialArtist
type Skill = Stealth | Acrobatics | Intimidation | Spell of name: string
type WeaponType = Rapier | Bow | Broadsword | MainGauche | Staff | Spear | Polearm
type WeaponFocus = All | Swords | OneWeapon of WeaponType | TwoWeapon of WeaponType * WeaponType
type Trait = Magery of n: int | WeaponMaster of WeaponFocus
type Datum = Meta of Profession | Skill of Skill | Trait of Trait
let skills = [Stealth; Acrobatics; Intimidation]

let offer data (pathHere, ctx: Ctx) =
    let path = (data.ToString())::pathHere
    if ctx.Has path then
        Selected(path, Some data)
    else
        NotSelected path

let offer1 (ctor, data) (pathHere, ctx: Ctx) =
    let path = (data.ToString())::pathHere
    if ctx.Has path then
        Selected(path, Some (ctor data))
    else
        NotSelected path

let offerRange (label, ctor, min, max) (pathHere, ctx: Ctx) =
    List (label, [
        for ix in [min..max] do
            let path = (ix.ToString())::pathHere
            if ctx.Has path then
                Selected(path, Some (ctor ix))
            else
                NotSelected path
        ])

let offerList (label, ctor) lst (pathHere, ctx: Ctx) =
    List(label, [
        for entry in lst do
            let path = entry.ToString()::pathHere
            if ctx.Has pathHere then
                Selected(path, Some (ctor entry))
            else
                NotSelected path
        ])

let weaponTypeParam weaponFilter (pathHere, ctx:Ctx) =
    let weapons = [Rapier ; Bow ; Broadsword ; MainGauche ; Staff ; Spear ; Polearm] |> List.filter weaponFilter
    match weapons |> List.tryFind(fun weapon -> (weapon.ToString())::pathHere |> ctx.Has) with
    | Some weapon ->
        let path = (weapon.ToString())::pathHere
        Some weapon, [Selected(path, None)] // we do NOT try to return a value. It's just a placeholder for the param.
    | None ->
        None, weapons |> List.map(fun weapon -> (weapon.ToString())::pathHere |> NotSelected)

let weaponMaster weaponFilter ctor (pathHere, ctx: Ctx) =
    let path = "WeaponMaster"::pathHere
    (* We may return:
        Selected WeaponMaster(All)
        Selected WeaponMaster(Swords)
        Selected WeaponMaster None, Selected WeaponFocus None, List [various options]
    *)
    if ctx.Has path then [
        Selected(path, None)
        let focii = ["All"; "Swords"; "OneWeapon"; "TwoWeapon"]
        match focii |> List.tryFind (fun focus -> focus::path |> ctx.Has) with
        | Some ("All" as focus) -> Selected(focus::path, WeaponMaster(All) |> ctor |> Some)
        | Some ("Swords" as focus) -> Selected(focus::path, WeaponMaster(Swords) |> ctor |> Some)
        | Some ("OneWeapon" as focus) ->
            let path = focus::path
            let weapon, selections = weaponTypeParam weaponFilter (path, ctx)
            match weapon with
            | Some focus ->
                Selected(path, WeaponMaster(OneWeapon(focus)) |> ctor |> Some)
            | None ->
                Selected(path, None)
                yield! selections
        | Some ("TwoWeapon" as focus) ->
            let path = focus::path
            let weapon, selections = weaponTypeParam weaponFilter (path, ctx)
            let weapon2, selections2 = weaponTypeParam weaponFilter ("Weapon2"::path, ctx)
            match weapon, weapon2 with
            | Some weapon, Some weapon2 ->
                Selected(path, WeaponMaster(TwoWeapon(weapon, weapon2)) |> ctor |> Some)
            | _ ->
                Selected(path, None)
                yield! selections
                yield! selections2
        | _ ->
            yield! focii |> List.map (fun focus -> focus::path |> NotSelected)
        ]
    else
        [NotSelected path]

let swashbucker ctor (pathHere, ctx:Ctx) = [
    let path = "Swashbuckler"::pathHere
    if ctx.Has path then
        Selected(path, Some (Meta Swashbuckler |> ctor))
        offerList (Some "Choose some skills", ctor << Skill) [Stealth; Acrobatics; Intimidation] ("skills"::pathHere, ctx)
        yield! weaponMaster (flip List.contains [Rapier; MainGauche]) (ctor << Trait) (path, ctx)
    else
        NotSelected path
    ]

let wizard ctor (pathHere, ctx:Ctx) = [
    let path = "Wizard"::pathHere
    if ctx.Has path then
        Selected(path, Some (Meta Wizard |> ctor))
        offerList (Some "Choose some skills", ctor << Skill) [Spell "Great Haste"; Spell "Explosive Fireball"; Spell "Shield"] ("spells"::path, ctx)
        offerRange (Some "How much magery do you want?", ctor << Trait << Magery, 3, 6) ("Magery"::path, ctx)
    else
        NotSelected path
    ]

let profession (ctx:Ctx) = [
    let path = []
    yield! swashbucker id (path, ctx)
    yield! wizard id (path, ctx)
    ]

let rec getTraits = function
    | Selected(_, Some trait1) -> [trait1]
    | List(_, lst) -> lst |> List.collect getTraits
    | _ -> []
let rec getOptions = function
    | Selected(_) -> []
    | List(_, lst) -> lst |> List.collect getOptions
    | NotSelected key -> [key]
let describe menu =
    printfn $"Traits: {menu |> List.collect getTraits}"
    printfn $"""Options: {menu |> List.collect getOptions |> List.map (List.rev >> String.join ":") |> String.join ", "}"""
profession(Ctx([["Swashbuckler"];["Stealth"; "skills"];["WeaponMaster"; "Swashbuckler"];["OneWeapon"; "WeaponMaster"; "Swashbuckler"]] |> Set.ofSeq))
|> describe

profession(Ctx([["Wizard"];["Stealth"; "skills"];["WeaponMaster"; "Swashbuckler"]] |> Set.ofSeq))
|> describe
