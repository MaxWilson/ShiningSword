module Chargen

open Common
open Expecto
open Expecto.Flip
open FsCheck
#if INTERACTIVE
#r "nuget: Unquote"
#endif
open Domain.Ribbit

open Domain.Character.DungeonFantasy.Templates
open Domain.Character.DungeonFantasy.TraitsAndAttributes
open Domain.Character.DungeonFantasy.TraitsAndAttributes.Data
open UI.Chargen.TraitView
let verify = Swensen.Unquote.Assertions.test

[<Tests>]
let tests = testList "Chargen" [

    test "A simple test" {
        let subject = "Hello World"
        verify <@ subject = "Hello World" @>
        }

    ptest "Verify initial Swashbuckler traits" {
        let traits0 = Menus.swash |> dataBuilder DataCtx.fresh |> List.sort
        let expected =
            [   Trait CombatReflexes
                Trait (Luck Standard)
                Trait (WeaponBond "")
                ]
            |> List.sort
        verify <@ expected = traits0 @>
        }

    ptest "Verify Swashbuckler traits after a few clicks" {
        let addKeys keys ctx =
            let keys = keys |> List.collect (List.rev >> List.prefixes)
            { ctx with queue = keys |> List.fold (fun q k -> q |> Map.add k "") ctx.queue }
        let addData keyVals ctx =
            let keyVals = keyVals |> List.map (Tuple2.mapfst List.rev)
            { ctx with queue = keyVals |> List.fold (fun q (k,v) -> q |> Map.add k v) ctx.queue }
        let ctx =
            DataCtx.fresh
            |> addKeys [
                ["Swashbuckler"; "Enhanced Parry 1"; "Rapier"]
                ["Swashbuckler"; "Weapon Master"; "OneWeapon"; "Rapier"]
                ]
            |> addData [
                ["Swashbuckler"; "Weapon Bond"], "Ancestral Rapier"
                ]
        let traits0 = Menus.swash |> dataBuilder ctx |> List.sort
        let expected =
            [   Trait CombatReflexes
                Trait (Luck Standard)
                Trait (WeaponBond "Ancestral Rapier")
                ]
            |> List.sort
        verify <@ expected = traits0 @>
        }
    test "Debug: Swashbuckler traits after a few clicks" {
        let addKeys keys ctx =
            let keys = keys |> List.collect (List.rev >> List.prefixes)
            { ctx with queue = keys |> List.fold (fun q k -> q |> Map.add k "") ctx.queue }
        let addData keyVals ctx =
            let keyVals = keyVals |> List.map (Tuple2.mapfst List.rev)
            { ctx with queue = keyVals |> List.fold (fun q (k,v) -> q |> Map.add k v) ctx.queue }
        let ctx =
            DataCtx.fresh
            |> addKeys [
                ["Swashbuckler"; "Enhanced Parry 1"; "Rapier"]
                ["Swashbuckler"; "Weapon Master"; "OneWeapon"; "Rapier"]
                ]
            |> addData [
                ["Swashbuckler"; "Weapon Bond"], "Ancestral Rapier"
                ]
        printfn $"""Is it there before the test? {ctx.queue[["Weapon Bond"; "Swashbuckler"]]}"""
        let swash = Create.aggregate "Swashbuckler" [
            let swashMeleeWeapons = [Broadsword; Rapier; Saber; Shortsword; Smallsword; MainGauche]
            let label' = Metadata.label'
            Create.grantAll [
                Create.binary (CombatReflexes |> Menus.Convert.Trait)
                Create.chooseLevels (Ctor.Luck |> Menus.Convert.Trait, [Standard])
                Create.chooseWithStringInput (Ctor.WeaponBond |> Menus.Convert.Trait, "Describe")
                ]
            ]
        let traits0 = swash |> dataBuilder ctx |> List.sort
        let expected =
            [   Trait (WeaponBond "Ancestral Rapier")
                ]
            |> List.sort
        verify <@ expected = traits0 @>
        }

    ]