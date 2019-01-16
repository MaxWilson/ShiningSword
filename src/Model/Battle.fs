module Model.Battle
open Model.Chargen
open Model.Names
open Common

open Model.Types
open Model.Operations

let randomSex() = chooseRandom [|Male;Female|]

let create = { Battle.map = Map.empty; combatants = Map.empty; stakes = None }
let add teamId (sb:StatBlock) (usages:Usages) (status:Status) (battle: Battle) =
    let nextId =
        if battle.combatants.IsEmpty then 1
        else 1 + (battle.combatants |> Map.toSeq |> Seq.maxBy fst |> fst)
    let rec freshPosition candidate =
        // look for a random empty square
        if battle.map.ContainsKey(candidate) then
            freshPosition (rand 20, rand 20) // random placement for now
        else
            candidate
    let newPosition = freshPosition (0,0)
    let combatant = { id = nextId; team = teamId; stats = sb; usages = usages; status = status; position = newPosition }
    { battle with combatants = Map.add nextId combatant battle.combatants; map = Map.add newPosition (Combatant nextId) battle.map }

let addFreshCombatant teamId statBlockTemplate battle =
    add teamId (statBlockTemplate()) Map.empty { conditions = [] } battle

let addExistingCharacter teamId (c:CharInfo) battle =
    add teamId (CharSheet.toStatBlock c.src) c.usages c.status battle

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
        | Str "[male:" (OWS(CharsExcept (Set.ofSeq [']']) (nameList, Str "]" ctx))) -> Some((fun() -> chooseFirstName Male nameList), ctx)
        | Str "[female:" (OWS(CharsExcept (Set.ofSeq [']']) (nameList, Str "]" ctx))) -> Some((fun() -> chooseFirstName Female nameList), ctx)
        | Str "[" (CharsExcept (Set.ofSeq [']']) (nameList, Str "]" ctx)) ->
            Some((fun() -> chooseFirstName (randomSex()) nameList), ctx)
        | Str "'" (CharsExcept (Set.ofSeq [''']) (name, Str "'" ctx)) -> Some((fun() -> name), ctx)
        | _ -> None
    let (|Attacks|_|) =
        let rec (|AttackList|_|) = pack <| function
            | Attack(attack, AttackList(attacks, ctx)) -> Some(attack::attacks, ctx)
            | Attack(attack, ctx) -> Some([attack], ctx)
            | _ -> None
        function
        | Word(AnyCase("attacks"), Optional ":" (OWS (AttackList(attacklist, ctx)))) -> Some(attacklist, ctx)
        | _ -> None
    let (|HitPoints|) =
        function
        | Word(AnyCase("hp"), Optional ":" (Int(hp, ctx))) -> (hp, ctx)
        | ctx -> (10, ctx)
    let (|ArmorClass|) =
        function
        | Word(AnyCase("ac"), Optional ":" (Int(ac, ctx))) -> (ac, ctx)
        | ctx -> (10, ctx)
    let (|TypeName|) =
        function
        | Words(typeName, ctx) -> (Some typeName, ctx)
        | ctx -> (None, ctx)
    let (|StatBlock|_|) = pack <| function
        | TypeName(typeName, Name(name, HitPoints(hp, ArmorClass(ac, (Attacks(attacks, ctx)))))) ->
            let sb() = {
                name = name()
                typeName = typeName
                sex = Male
                str = 10
                dex = 10
                con = 10
                int = 10
                wis = 10
                cha = 10
                hp = hp
                ac = ac
                xp = 0
                resistances = Set.empty
                immunities = Set.empty
                damageResistance = Map.empty
                conditionExemptions = Set.empty
                attacks = attacks
                features = []
                }
            Some(sb, ctx)
        | _ -> None
    let makeCombatantStub name attacks =
        let stats = {
            name = name
            typeName = None
            sex = Male
            str = 10
            dex = 10
            con = 10
            int = 10
            wis = 10
            cha = 10
            hp = 10
            ac = 10
            xp = 0
            resistances = Set.empty
            immunities = Set.empty
            damageResistance = Map.empty
            conditionExemptions = Set.empty
            attacks = attacks
            features = []
            }
        { id = 0; team = Blue; usages = Map.empty; status = { conditions = [] }; position = (0,0); stats = stats }
    let attack = parser (|Attack|_|)
    let name = parser (|Name|_|)
    let statblock = parser (|StatBlock|_|)

// Parse.statblock "Orc [male:Mordor]\nhp: 12 ac: 13 attacks: +4 for d12+3"()
// Parse.statblock "[male:Mordor] attacks: +4 for d12+3"()
