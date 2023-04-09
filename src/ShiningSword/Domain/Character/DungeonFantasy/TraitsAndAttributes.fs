[<AutoOpen>]
module Domain.Character.DungeonFantasy.TraitsAndAttributes
open Domain.Ribbit.Properties

type Race = Human | Catfolk | Dwarf | Elf | HalfElf | Gnome | HalfOgre | HalfOrc | Halfling | Coleopteran
type Profession = Barbarian | Bard | Cleric | Druid | HolyWarrior | Knight | MartialArtist | Scout | Swashbuckler | Thief | Wizard
type WeaponSkill =
    | AxeOrMace
    | Boxing | Brawling | Karate | Broadsword | Flail | Garrote | JitteOrSai
    | Knife | Kusari | MainGauche
    | Polearm | Rapier | Saber | Shield | Shortsword
    | Smallsword | Spear | Staff | Tonfa | TwoHandedAxeOrMace | TwoHandedFlail
    | TwoHandedSword | Whip
    | Blowpipe | Bolas
    | Bow | Crossbow | Lasso | Net | Sling | SpearThrower
    | ThrownAxeOrMace | ThrownDart | ThrownHarpoon | ThrownKnife | ThrownShuriken | ThrownSpear | ThrownStick | ThrowingArt
type AppearanceLevel = Attractive | Beautiful | VeryBeautiful
type ChummyLevel = Standard | Gregarious
type Creed = Outlaws | Gentlemans
type Duty = AdventuringCompanions
type LuckLevel = Standard | Extraordinary | Ridiculous
type ObsessionSubject = BecomeBestSwordsman
type Severity = Severe | Serious | Moderate | Mild
type VowSubject = UseOnlyWeaponOfChoice | NeverRefuseAChallengeToCombat | ChallengeEverySwordsmanToCombat | NeverWearArmor
type WeaponMasterFocus = All | Swords | FencingWeapons | TwoWeapon of WeaponSkill * WeaponSkill | OneWeapon of WeaponSkill
type Trait =
    | Ambidexterity
    | Appearance of AppearanceLevel
    | ArmorFamiliarity of int
    | Charisma of int
    | Chummy of ChummyLevel
    | CodeOfHonor of Creed
    | CombatReflexes
    | CompulsiveCarousing of Severity
    | CompulsiveGambling of Severity
    | CompulsiveSpending of Severity
    | Daredevil
    | EnhancedBlock of int
    | EnhancedDodge of int
    | EnhancedParry of int * WeaponSkill
    | EnhancedTimeSense
    | EveryOnesACritical
    | ExtraAttack of int
    | GreatVoid
    | Greed of Severity
    | HighPainThreshold
    | Impulsiveness of Severity
    | Jealousy of Severity
    | Lecherousness of Severity
    | Luck of LuckLevel
    | Magery of int
    | Obsession of ObsessionSubject * Severity
    | OneEye
    | Overconfidence of Severity
    | PerfectBalance
    | PowerInvestitureClerical of int
    | PowerInvestitureDruidic of int
    | RapierWit
    | SenseOfDuty of Duty
    | Serendipity of int
    | ShortAttentionSpan of Severity
    | SignatureGear of string
    | SpringingAttack
    | StrikingST of int
    | TrademarkMove of string
    | Trickster of Severity
    | Vow of VowSubject
    | WeaponBond of string
    | WeaponMaster of WeaponMasterFocus
    | Wounded

type Skill =
    | Weapon of WeaponSkill
    | Stealth
    | Camouflage
    | Observation

type Spell =
    | Blink
    | BlinkOther
    | DeathVision
    | Phantom
    | Telepathy

[<AutoOpen>]
module Stats =
    open type Create
    type Attributes = {
        ST: int Primary
        DX: int Primary
        IQ: int Primary
        HT: int Primary
        Will: int Secondary
        Per: int Secondary
        SM: int Primary
        HP: int Secondary
        FP: int Secondary
        Move: int Secondary
        SpeedTimesFour: int Secondary
        Dodge: int Secondary
        Traits: Map<string, Trait> // traitName -> Trait details
        }
    let freshFrom(st, dx, iq, ht) =
        {   ST = primary st
            DX = primary dx
            IQ = primary iq
            HT = primary ht
            Will = secondary()
            Per = secondary()
            SM = primary 0
            HP = secondary()
            FP = secondary()
            Move = secondary()
            SpeedTimesFour = secondary()
            Dodge = secondary()
            Traits = Map.empty
            }
    let fresh = freshFrom(10, 10, 10, 10)
    let ST char = Eval.eval char.ST
    let DX char = Eval.eval char.DX
    let IQ char = Eval.eval char.IQ
    let HT char = Eval.eval char.HT
    let Will char = Eval.evalAndCondense (IQ char, Some "IQ", char.Will)
    let Per char = Eval.evalAndCondense (IQ char, Some "IQ", char.Per)
    let SM char = Eval.eval char.SM
    let HP char = Eval.evalAndCondense(ST char, Some "ST", char.HP)
    let FP char = Eval.evalAndCondense(HT char, Some "HT", char.FP)
    let Speed char : float RValue =
        let ht = HT char
        let dx = DX char
        {   baseValue = ((Eval.sum ht + Eval.sum dx) |> float) / 4.
            description = Some $"(HT {ht |> Eval.sum} + DX {dx |> Eval.sum})/4"
            modifiers =
                char.SpeedTimesFour.modifiers |> List.map Eval.eval
                |> List.map (Tuple2.mapfst (float >> (flip (/) 4.)))
                }
    let Move char: int RValue =
        let speed = Speed char |> Eval.sum
        {   baseValue = speed |> int
            description = Some "Speed, rounded down"
            modifiers = char.Move.modifiers |> List.map Eval.eval
            }
    let Dodge char: int RValue =
        let speed = Speed char |> Eval.sum
        {   baseValue = (speed |> int) + 3
            description = Some "Speed +3, rounded down"
            modifiers = char.Dodge.modifiers |> List.map Eval.eval
            }

module Data =
    type StatAddress = ST | DX | IQ | HT | Will | Per | SM | HP | FP | Move | SpeedTimesFour | Dodge
    type MagicSource = Clerical | Druidic | Wizardly
    type Difficulty = Easy | Average | Hard | VeryHard
    type Chosen =
        | StatBonus of StatAddress * int
        | Trait of Trait
        | Skill of Skill  * level: int
        | Spell of Spell * MagicSource * level: int
    type SkillData = { stat: StatAddress; difficulty: Difficulty }
    type SpellData = { difficulty: Difficulty }
    let traitCost trait1 =
        let (|Severity|) = function
            | Severe -> float >> ((*)2.0) >> int
            | Serious -> float >> ((*)1.5) >> int
            | Moderate -> id
            | Mild -> float >> ((*)0.5) >> int
        match trait1 with
        | Ambidexterity -> 5
        | Appearance level ->
            match level with Attractive -> 4 | Beautiful -> 12 | VeryBeautiful -> 16
        | ArmorFamiliarity n -> n
        | Charisma n -> n * 5
        | Chummy level -> match level with ChummyLevel.Standard -> -5 | Gregarious -> -10
        | CodeOfHonor creed -> match creed with Outlaws -> -5 | Gentlemans -> -10
        | CombatReflexes -> 15
        | CompulsiveCarousing(Severity sev) -> sev -5
        | CompulsiveGambling(Severity sev) -> sev -5
        | CompulsiveSpending(Severity sev) -> sev -5
        | Daredevil -> 15
        | EnhancedBlock n -> n * 5
        | EnhancedDodge n -> n * 15
        | EnhancedParry(n, _) -> n * 5
        | EnhancedTimeSense -> 45
        | EveryOnesACritical -> 15
        | ExtraAttack n -> n * 25
        | GreatVoid -> 10
        | Greed(Severity sev) -> sev -15
        | HighPainThreshold -> 10
        | Impulsiveness(Severity sev) -> sev -10
        | Jealousy(Severity sev) -> sev -10
        | Lecherousness(Severity sev) -> sev -15
        | Luck level -> match level with Standard -> 15 | Extraordinary -> 30 | Ridiculous -> 60
        | Magery n -> 5 + 10 * n
        | Obsession(_, Severity sev) -> sev -10
        | OneEye -> -15
        | Overconfidence(Severity sev) -> sev -5
        | PerfectBalance -> 15
        | PowerInvestitureClerical n -> 10 * n
        | PowerInvestitureDruidic n -> 10 * n
        | RapierWit -> 5
        | SenseOfDuty duty -> match duty with AdventuringCompanions -> -5
        | Serendipity n -> n * 15
        | ShortAttentionSpan(Severity sev) -> sev -10
        | SignatureGear _ -> 1
        | SpringingAttack -> 10
        | StrikingST n -> n * 5
        | TrademarkMove _ -> 1
        | Trickster(Severity sev) -> sev -15
        | Vow subject -> match subject with UseOnlyWeaponOfChoice -> -5 | NeverRefuseAChallengeToCombat -> -10 | ChallengeEverySwordsmanToCombat -> -15 | NeverWearArmor -> -15
        | WeaponBond _ -> 1
        | WeaponMaster focus ->
            match focus with
            | All -> 45
            | Swords -> 35
            | FencingWeapons -> 30
            | TwoWeapon(w1, w2) -> 25
            | OneWeapon(w) -> 20
        | Wounded -> -5
    let skillData = memoize (function
        | Stealth -> { stat = DX; difficulty = Average }
        | Camouflage -> { stat = IQ; difficulty = Easy }
        | Weapon(Net) -> { stat = DX; difficulty = Hard }
        | Observation -> { stat = Per; difficulty = Average }
        | _ -> notImpl()
        )
    let spellData = memoize (function
        | Telepathy | Phantom | BlinkOther -> { difficulty = VeryHard }
        | _ -> { difficulty = Hard }
        )

    let cost = function
        | Trait tr -> traitCost tr
        | StatBonus((ST | HT), n) -> n * 10
        | StatBonus((DX | IQ), n) -> n * 20
        | StatBonus((Will | Per | Move | SpeedTimesFour), n) -> n * 5
        | StatBonus(HP, n) -> n * 2
        | StatBonus(FP, n) -> n * 3
        | StatBonus(Dodge, _) -> shouldntHappen "You're not supposed to buy Dodge as a stat, only as Enhanced Dodge. How did the player get this? It's a bug."
        | StatBonus(SM, _) -> shouldntHappen "You're not supposed to buy more SM as a stat. How did the player get this? It's a bug."
        | Skill(_, n) | Spell(_, _, n) ->
            if n > 2 then 4 * (n - 2)
            elif n = 2 then 2
            else 1

module Ctor =
    open Data
    let namedCtor(name, ctor, f) = namedCtor(name |> String.uncamel, ctor, f)
    let Appearance = namedCtor(nameof(Appearance), Appearance, function Appearance v -> Some v | _ -> None)
    let ArmorFamiliarity = namedCtor(nameof(ArmorFamiliarity), ArmorFamiliarity, function ArmorFamiliarity v -> Some v | _ -> None)
    let Chummy = namedCtor(nameof(Chummy), Chummy, function Chummy v -> Some v | _ -> None)
    let CodeOfHonor = namedCtor(nameof(CodeOfHonor), CodeOfHonor, function CodeOfHonor v -> Some v | _ -> None)
    let CompulsiveCarousing = namedCtor(nameof(CompulsiveCarousing), CompulsiveCarousing, function CompulsiveCarousing v -> Some v | _ -> None)
    let CompulsiveGambling = namedCtor(nameof(CompulsiveGambling), CompulsiveGambling, function CompulsiveGambling v -> Some v | _ -> None)
    let CompulsiveSpending = namedCtor(nameof(CompulsiveSpending), CompulsiveSpending, function CompulsiveSpending v -> Some v | _ -> None)
    let Charisma = namedCtor(nameof(Charisma), Charisma, function Charisma v -> Some v | _ -> None)
    let EnhancedBlock = namedCtor(nameof(EnhancedBlock), EnhancedBlock, function EnhancedBlock v -> Some v | _ -> None)
    let EnhancedDodge = namedCtor(nameof(EnhancedDodge), EnhancedDodge, function EnhancedDodge v -> Some v | _ -> None)
    let EnhancedParry = namedCtor(nameof(EnhancedParry), EnhancedParry, function EnhancedParry(name, skill) -> Some (name, skill) | _ -> None)
    let ExtraAttack = namedCtor(nameof(ExtraAttack), ExtraAttack, function ExtraAttack v -> Some v | _ -> None)
    let Greed = namedCtor(nameof(Greed), Greed, function Greed v -> Some v | _ -> None)
    let Impulsiveness = namedCtor(nameof(Impulsiveness), Impulsiveness, function Impulsiveness v -> Some v | _ -> None)
    let Jealousy = namedCtor(nameof(Jealousy), Jealousy, function Jealousy v -> Some v | _ -> None)
    let Lecherousness = namedCtor(nameof(Lecherousness), Lecherousness, function Lecherousness v -> Some v | _ -> None)
    let Luck = namedCtor(nameof(Luck), Luck, function Luck v -> Some v | _ -> None)
    let Magery = namedCtor(nameof(Magery), Magery, function Magery v -> Some v | _ -> None)
    let Obsession = namedCtor(nameof(Obsession), Obsession, function Obsession(obsession, severity) -> Some (obsession, severity) | _ -> None)
    let Overconfidence = namedCtor(nameof(Overconfidence), Overconfidence, function Overconfidence v -> Some v | _ -> None)
    let PowerInvestitureClerical = namedCtor(nameof(PowerInvestitureClerical), PowerInvestitureClerical, function PowerInvestitureClerical v -> Some v | _ -> None)
    let PowerInvestitureDruidic = namedCtor(nameof(PowerInvestitureDruidic), PowerInvestitureDruidic, function PowerInvestitureDruidic v -> Some v | _ -> None)
    let SenseOfDuty = namedCtor(nameof(SenseOfDuty), SenseOfDuty, function SenseOfDuty v -> Some v | _ -> None)
    let Serendipity = namedCtor(nameof(Serendipity), Serendipity, function Serendipity v -> Some v | _ -> None)
    let ShortAttentionSpan = namedCtor(nameof(ShortAttentionSpan), ShortAttentionSpan, function ShortAttentionSpan v -> Some v | _ -> None)
    let SignatureGear = namedCtor(nameof(SignatureGear), SignatureGear, function SignatureGear v -> Some v | _ -> None)
    let StrikingST = namedCtor(nameof(StrikingST), StrikingST, function StrikingST v -> Some v | _ -> None)
    let TrademarkMove = namedCtor(nameof(TrademarkMove), TrademarkMove, function TrademarkMove v -> Some v | _ -> None)
    let Trickster = namedCtor(nameof(Trickster), Trickster, function Trickster v -> Some v | _ -> None)
    let Vow = namedCtor(nameof(Vow), Vow, function Vow v -> Some v | _ -> None)
    let WeaponBond = namedCtor(nameof(WeaponBond), WeaponBond, function WeaponBond t -> Some t | _ -> None)
    let WeaponMaster = namedCtor(nameof(WeaponMaster), WeaponMaster, function WeaponMaster v -> Some v | _ -> None)

    let OneWeapon = namedCtor(nameof(OneWeapon), OneWeapon, function OneWeapon(v) -> Some (v) | _ -> None)
    let TwoWeapon = namedCtor(nameof(TwoWeapon), TwoWeapon, function TwoWeapon(weapon1, weapon2) -> Some (weapon1, weapon2) | _ -> None)

open Data
open Ctor
type Format() =
    static let boost char (source: MagicSource) =
        let lookup (ctor: Constructor<int, Trait>) =
            match char.Traits |> Map.tryFind ctor.name with
            | Some data -> ctor.extract(data)
            | _ -> None
        match source with
        | Clerical -> PowerInvestitureClerical
        | Druidic -> PowerInvestitureDruidic
        | Wizardly -> Magery
        |> lookup
        |> Option.defaultValue 0
    static let ofDifficulty level = function
        | Easy -> level - 1 // e.g. 1 point gives +0
        | Average -> level - 2 // e.g. 1 point gives +1
        | Hard -> level - 3
        | VeryHard -> level - 4
    static let traitName trait1 =
        let selfControl(baseName, severity) =
            let sev =
                match severity with
                | Mild -> "15 (Mild)"
                | Moderate -> "12 (Moderate)"
                | Serious -> "9 (Serious)"
                | Severe -> "6 (Severe)"
            $"{String.uncamel baseName}-{sev}"
        match trait1 with
        | EveryOnesACritical -> "Every One's A Critical"
        | Luck(level) ->
            match level with
            | Standard -> "Luck"
            | lvl -> $"{lvl} Luck"
        | EnhancedParry(n, weapon) ->
            $"Enhanced Parry {n} ({weapon})"
        | CompulsiveCarousing sev -> selfControl(nameof(CompulsiveCarousing), sev)
        | CompulsiveGambling sev -> selfControl(nameof(CompulsiveGambling), sev)
        | CompulsiveSpending sev -> selfControl(nameof(CompulsiveSpending), sev)
        | Greed sev -> selfControl(nameof(Greed), sev)
        | Impulsiveness sev -> selfControl(nameof(Impulsiveness), sev)
        | Jealousy sev -> selfControl(nameof(Jealousy), sev)
        | Lecherousness sev -> selfControl(nameof(Lecherousness), sev)
        | Obsession(subject,  sev) -> $"{selfControl(nameof(Obsession), sev)} ({subject.ToUncameledString()})"
        | Overconfidence sev -> selfControl(nameof(Overconfidence), sev)
        | ShortAttentionSpan sev -> selfControl(nameof(ShortAttentionSpan), sev)
        | Trickster sev -> selfControl(nameof(Trickster), sev)
        | StrikingST n -> $"{nameof(Trait.StrikingST) |> String.uncamel} %+d{n}"
        | Vow subject -> $"Vow ({subject.ToUncameledString()})"
        | WeaponMaster(v) ->
            match v with
            | OneWeapon w -> $"Weapon Master ({w})"
            | TwoWeapon(w1, w2) -> $"Weapon Master ({w1}, {w2})"
            | _ -> $"Weapon Master ({v})"
        | Chummy ChummyLevel.Standard -> "Chummy"
        | Chummy Gregarious -> "Gregarious"
        | SenseOfDuty duty -> $"Sense of Duty ({duty.ToUncameledString()})"
        | v -> v.ToString() |> String.uncamel
    static let skillName = function
        | Weapon(weapon) -> weapon.ToString() |> String.uncamel
        | v -> v.ToString() |> String.uncamel
    static let spellName name = String.uncamel (name.ToString())

    static member name (v: Trait) = traitName v
    static member name v =
        match v with
        | StatBonus(stat, n) -> $"{stat} %+d{n}"
        | Trait t -> Format.name t
        | Skill(name, level) ->
            let skill = Data.skillData name
            let relativeBonus = (ofDifficulty level skill.difficulty)
            $"{skillName name}/{skill.stat} %+d{relativeBonus}"
        | Spell(name, source, level) ->
            let relativeBonus = (ofDifficulty level (Data.spellData name).difficulty)
            $"{spellName name} (IQ %+d{relativeBonus})"
    static member value (char:Attributes) = function
        | StatBonus(stat, n) ->
            match stat with
            | SpeedTimesFour ->
                $"{stat} {Stats.Speed char |> Eval.sum} (%+d{n/4})"
            | _ ->
                let property =
                    match stat with
                        | ST -> Stats.ST
                        | DX -> Stats.DX
                        | IQ -> Stats.IQ
                        | HT -> Stats.HT
                        | Will -> Stats.Will
                        | Per -> Stats.Per
                        | SM -> Stats.SM
                        | HP -> Stats.HP
                        | FP -> Stats.FP
                        | Move -> Stats.Move
                        | SpeedTimesFour -> shouldntHappen()
                        | Dodge -> Stats.Will
                $"{stat} {(property char |> Eval.sum) + n} (%+d{n})"
        | Trait t -> Format.name t
        | Skill(name, level) ->
            let baseLevel =
                let property =
                    match (Data.skillData name).stat with
                        | ST -> Stats.ST
                        | DX -> Stats.DX
                        | IQ -> Stats.IQ
                        | HT -> Stats.HT
                        | Per -> Stats.Per
                        | Will -> Stats.Will
                        | otherwise -> shouldntHappen otherwise // skills are only supposed to be based off of the six main stats, and usually just DX/IQ
                property char |> Eval.sum
            let skill = Data.skillData name
            let relativeBonus = (ofDifficulty level skill.difficulty)
            $"{skillName name}-{baseLevel + relativeBonus} ({skill.stat} %+d{relativeBonus})"
        | Spell(name, source, level) ->
            let boost1, diff = boost char source, (ofDifficulty level (Data.spellData name).difficulty)
            let relativeBonus = boost char source + (ofDifficulty level (Data.spellData name).difficulty)
            $"{spellName name}-{(Stats.IQ char |> Eval.sum) + relativeBonus} (IQ %+d{relativeBonus})"
