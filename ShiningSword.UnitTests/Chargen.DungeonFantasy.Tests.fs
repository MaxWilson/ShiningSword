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

    test "Spot-check display names" {
        let stats = { Stats.freshFrom(11, 11, 11, 11) with Traits = Map.ofList [Ctor.Magery.name.Value, Magery 6] }
        let weakStats = { Stats.freshFrom(11, 11, 11, 11) with Traits = Map.ofList [Ctor.Magery.name.Value, Magery 2] }
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
        let traits0 = Menus.swash |> dataBuilder DataCtx.fresh |> List.sort
        let expected =
            [   Trait CombatReflexes
                Trait (Luck Standard)
                Trait (WeaponBond "")
                ]
            |> List.sort
        verify <@ expected = traits0 @>
        }

    test "Verify Swashbuckler traits after a few clicks" {
        let addKeys keys (ctx: DataCtx) =
            let keys = keys |> List.collect (List.rev >> List.prefixes)
            { ctx with queue = keys |> List.fold (fun q k -> q |> Map.add k "") ctx.queue }
        let addData keyVals (ctx: DataCtx) =
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
