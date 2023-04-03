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
open type Create
open type Menus.Convert
let verify = Swensen.Unquote.Assertions.test
let addKeys keys (ctx: DataCtx) =
    let keys = keys |> List.collect (List.rev >> List.prefixes)
    { ctx with queue = keys |> List.fold (fun q k -> q |> Map.add k "") ctx.queue }
let addData keyVals (ctx: DataCtx) =
    let keyVals = keyVals |> List.map (Tuple2.mapfst List.rev)
    { ctx with queue = keyVals |> List.fold (fun q (k,v) -> q |> Map.add k v) ctx.queue }

[<Tests>]
let tests = testList "Chargen" [

    test "Spot-check display names" {
        let stats = { Stats.freshFrom(11, 11, 11, 11) with Traits = Map.ofList [Ctor.Magery.name, Magery 6] }
        let weakStats = { Stats.freshFrom(11, 11, 11, 11) with Traits = Map.ofList [Ctor.Magery.name, Magery 2] }
        verify <@ Format.name        (Trait (Luck Standard)) = "Luck" @>
        verify <@ Format.value stats (Trait (Luck Standard)) = "Luck" @>
        verify <@ Format.name        (Trait (EnhancedParry(1, Rapier))) = "Enhanced Parry 1 (Rapier)" @>
        verify <@ Format.value stats (Trait (EnhancedParry(1, Rapier))) = "Enhanced Parry 1 (Rapier)" @>
        verify <@ Format.name        (StatBonus(IQ, 3)) = "IQ +3" @>
        verify <@ Format.value stats (StatBonus(IQ, 3)) = "IQ 14 (+3)" @>
        verify <@ Format.name        (Skill(Observation, 4)) = "Observation/Per +2" @>
        verify <@ Format.value stats (Skill(Observation, 4)) = "Observation-13 (Per +2)" @>
        verify <@ Format.name        (Skill(Camouflage, 4)) = "Camouflage/IQ +3" @>
        verify <@ Format.value stats (Skill(Camouflage, 4)) = "Camouflage-14 (IQ +3)" @>
        verify <@ Format.name        (Skill(Weapon(Net), 4)) = "Net/DX +1" @>
        verify <@ Format.value stats (Skill(Weapon(Net), 4)) = "Net-12 (DX +1)" @>
        verify <@ Format.name        (Spell(DeathVision, Wizardly, 1)) = "Death Vision (IQ -2)" @>
        verify <@ Format.value stats (Spell(DeathVision, Wizardly, 1)) = "Death Vision-15 (IQ +4)" @>
        verify <@ Format.value stats (Spell(Blink, Wizardly, 1)) = "Blink-15 (IQ +4)" @>
        verify <@ Format.value stats (Spell(BlinkOther, Wizardly, 1)) = "Blink Other-14 (IQ +3)" @>
        verify <@ Format.value weakStats (Spell(BlinkOther, Wizardly, 1)) = "Blink Other-10 (IQ -1)" @>
        verify <@ Format.value weakStats (Spell(BlinkOther, Wizardly, 2)) = "Blink Other-11 (IQ +0)" @>
        }

    test "Verify initial Swashbuckler traits" {
        let actual = Menus.swash |> dataBuilder DataCtx.fresh |> List.sort
        let expected =
            [   Trait CombatReflexes
                Trait (Luck Standard)
                Trait (WeaponBond "")
                ]
            |> List.sort
        Expect.equal "Traits" expected actual
        }

    test "Verify dataBuilder on some simple actual and potential traits" {
        let menu = aggregate [
            grantAll "Automatic" [
                binary (CombatReflexes |> Trait)
                chooseLevels (Ctor.Luck |> Trait, [Standard])
                choose2D (Ctor.EnhancedParry |> Trait, [1], [Broadsword; Rapier; Saber; Shortsword; Smallsword; MainGauche])
                chooseWithStringInput (Ctor.WeaponBond |> Trait, "Describe")
                ]
            budget 60 "Advantages" [
                chooseLevels(Menus.StatBonus HP, [1..6], Menus.showBonuses)
                chooseLevels(Menus.StatBonus DX, [1..3], Menus.showBonuses)
                chooseLevels(Menus.Speed, [1..3], Menus.showBonuses)
                binary(Trait Ambidexterity)
                ]
            ]
        let ctx =
            DataCtx.fresh
            |> addKeys [
                ["Enhanced Parry"; "Rapier"]
                ["Weapon Master"; "OneWeapon"; "Rapier"]
                ]
            |> addData [
                ["Weapon Bond"], "Ancestral Rapier"
                ]
        let actual = menu |> dataBuilder ctx |> List.sort
        let potential = menu |> dataBuilder { ctx with includePotentials = true } |> List.sort
        // selected options should show up in both place
        verify <@ actual |> List.contains (Trait CombatReflexes) @>
        verify <@ potential |> List.contains (Trait CombatReflexes) @>
        verify <@ actual |> List.contains (Trait (WeaponBond "Ancestral Rapier")) @>
        verify <@ potential |> List.contains (Trait (WeaponBond "Ancestral Rapier")) @>
        verify <@ actual |> List.contains (Trait (EnhancedParry(1, Rapier))) @>
        verify <@ potential |> List.contains (Trait (EnhancedParry(1, Rapier))) @>
        verify <@ actual |> List.contains (Trait (Luck Standard)) @>
        verify <@ potential |> List.contains (Trait (Luck Standard)) @>

        // unselected items should show up only under potential
        verify <@ actual |> List.contains (Trait Ambidexterity) |> not @>
        verify <@ potential |> List.contains (Trait Ambidexterity) @>
        verify <@ actual |> List.contains (StatBonus(HP, 1)) |> not @>
        verify <@ potential |> List.contains (StatBonus(HP, 1)) @>
        }

    test "Verify Swashbuckler traits after a few clicks" {
        let ctx =
            DataCtx.fresh
            |> addKeys [
                ["Enhanced Parry"; "Rapier"]
                ["Weapon Master"; "OneWeapon"; "Rapier"]
                ]
            |> addData [
                ["Weapon Bond"], "Ancestral Rapier"
                ]
        let actual = Menus.swash |> dataBuilder ctx |> List.sort
        let expected =
            [   Trait CombatReflexes
                Trait (Luck Standard)
                Trait (EnhancedParry(1, Rapier))
                Trait (WeaponBond "Ancestral Rapier")
                ]
            |> List.sort
        Expect.equal "Traits" expected actual
        }
    ]
