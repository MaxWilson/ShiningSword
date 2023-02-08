#I __SOURCE_DIRECTORY__
#I ".."
#I "..\Core"
#load @"Optics.fs"
#load @"Common.fs"
#r "nuget: Thoth.Json.Net"
open System.IO
open Thoth.Json.Net
let dir = System.Environment.CurrentDirectory

let write name data =
    File.WriteAllText(Path.Combine(dir, name + ".rbt"), Encode.Auto.toString data)

let read<'t> name: 't =
    File.ReadAllText(Path.Combine(dir, name + ".rbt")) |> Decode.Auto.fromString |> Result.toOption |> Option.get

type Profession =
    Swashbuckler | Knight | Wizard
type WeaponType = Rapier | Broadsword | Bow | MainGauche | TwoHandedSword | Spear | Staff | Crossbow
type WeaponFocus = All | Swords | OneWeapon of WeaponType | TwoWeapon of WeaponType * WeaponType
type Attribute = ST | DX | IQ | HT | Move | Per | Will | HP | FP
type Trait =
    | WeaponMaster of WeaponFocus
    | ToughSkin of int
    | TrainedByAMaster
    | Extra of Attribute * int
    | StrikingST of int
type Skill =
    | Weapon of WeaponType
    | Stealth
    | Tactics
write "Hero" {|
    name = "Hero Philandrius Meliseni"
    profession = Swashbuckler
    skills = [
        Weapon Rapier, +4
        Stealth, +2
        Tactics, -1
        Weapon MainGauche, +2
        ]
    traits = [
        WeaponMaster (TwoWeapon(Rapier, MainGauche))
        ]
    |}

type Character = {
    name: string
    profession: Profession
    skills: (Skill * int) array
    traits: Trait array
}

let hero = read<Character> "Hero"

