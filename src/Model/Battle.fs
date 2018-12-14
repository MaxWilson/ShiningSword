module Model.Battle
open Model.Chargen
open Model.Names
open Common

open Model.Types

type Combatant = {
    name: Name
    attacks: Attack list
    }

let randomSex() = chooseRandom [|Male;Female|]

#nowarn "40" // recursive references in parse patterns are fine
module Parse =
    open Packrat
    let (|Roll|_|) = pack <| function
        | Int(n, Str "d" (Int(d, Str "+" (Int (plus, ctx))))) -> Some({ Roll.n = n; die = d; bonus = plus }, ctx)
        | Int(n, Str "d" (Int(d, ctx))) -> Some({ Roll.n = n; die = d; bonus = 0 }, ctx)
        | Str "d" (Int(d, Str "+" (Int (plus, ctx)))) -> Some({ Roll.n = 1; die = d; bonus = plus }, ctx)
        | Str "d" (Int(d, ctx)) -> Some({ Roll.n = 1; die = d; bonus = 0 }, ctx)
        | _ -> None
    let (|DamageType|_|) = pack <| function
        | Word(AnyCase("weapon"), ctx) -> Some(Weapon, ctx)
        | Word(AnyCase("fire"), ctx) -> Some(Fire, ctx)
        | Word(AnyCase("cold"), ctx) -> Some(Cold, ctx)
        | Word(AnyCase("poison"), ctx) -> Some(Poison, ctx)
        | _ -> None
    let (|Dmg|_|) = pack <| function
        | Roll(r, DamageType(damageType, ctx)) -> Some ((r, damageType), ctx)
        | Roll(r, ctx) -> Some ((r, Weapon), ctx)
        | _ -> None
    let (|Attack|_|) = pack <| function
        | Optional "+" (Int(toHit, Word(AnyCase("for"), Dmg(dmg, ctx)))) -> Some ({ tohit = toHit; damage = dmg}, ctx)
        | _ -> None
    let (|Name|_|) = pack <| function
        | Str "[male:" (OWS(CharsExcept (Set.ofSeq [']']) (nameList, Str "]" ctx))) -> Some((fun() -> chooseName Male nameList), ctx)
        | Str "[female:" (OWS(CharsExcept (Set.ofSeq [']']) (nameList, Str "]" ctx))) -> Some((fun() -> chooseName Male nameList), ctx)
        | Str "[" (CharsExcept (Set.ofSeq [']']) (nameList, Str "]" ctx)) -> Some((fun() -> chooseName (randomSex()) nameList), ctx)
        | Any(txt, ctx) -> Some(thunk txt, ctx)
        | _ -> None
    let (|Attacks|_|) =
        let rec (|AttackList|_|) = pack <| function
            | Attack(attack, AttackList(attacks, ctx)) -> Some(attack::attacks, ctx)
            | Attack(attack, ctx) -> Some([attack], ctx)
            | _ -> None
        function
        | Word(AnyCase("attacks"), Optional ":" (OWS (AttackList(attacklist, ctx)))) -> Some(attacklist, ctx)
        | _ -> None
    let (|Combatant|_|) = pack <| function
        | Name(name, (Attacks(attacks, ctx))) -> Some({ Combatant.name = name(); attacks = attacks }, ctx)
        | Name(name, ctx) -> Some({ Combatant.name = name(); attacks = [] }, ctx)
        | _ -> None
    let attack = parser (|Attack|_|)
    let name = parser (|Name|_|)
    let combatant = parser (|Combatant|_|)

// Parse.combatant "[male:Mordor]\nattacks: +4 for d12+3"
