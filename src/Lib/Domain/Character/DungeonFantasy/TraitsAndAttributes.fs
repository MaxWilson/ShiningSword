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
    | Chummy
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
    | Gregarious
    | HighPainThreshold
    | Impulsiveness of Severity
    | Jealousy of Severity
    | Lecherousness of Severity
    | Luck of LuckLevel
    | Obsession of ObsessionSubject * Severity
    | OneEye
    | Overconfidence of Severity
    | PerfectBalance
    | RapierWit
    | SenseOfDuty of Duty
    | Serendipity
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

module Ctor =
    let Appearance = ctor(Appearance, function Appearance v -> Some v | _ -> None)
    let ArmorFamiliarity = ctor(ArmorFamiliarity, function ArmorFamiliarity v -> Some v | _ -> None)
    let CodeOfHonor = ctor(CodeOfHonor, function CodeOfHonor v -> Some v | _ -> None)
    let CompulsiveCarousing = ctor(CompulsiveCarousing, function CompulsiveCarousing v -> Some v | _ -> None)
    let CompulsiveGambling = ctor(CompulsiveGambling, function CompulsiveGambling v -> Some v | _ -> None)
    let CompulsiveSpending = ctor(CompulsiveSpending, function CompulsiveSpending v -> Some v | _ -> None)
    let Charisma = ctor(Charisma, function Charisma v -> Some v | _ -> None)
    let EnhancedBlock = ctor(EnhancedBlock, function EnhancedBlock v -> Some v | _ -> None)
    let EnhancedDodge = ctor(EnhancedDodge, function EnhancedDodge v -> Some v | _ -> None)
    let EnhancedParry = ctor(EnhancedParry, function EnhancedParry(name, skill) -> Some (name, skill) | _ -> None)
    let ExtraAttack = ctor(ExtraAttack, function ExtraAttack v -> Some v | _ -> None)
    let Greed = ctor(Greed, function Greed v -> Some v | _ -> None)
    let Impulsiveness = ctor(Impulsiveness, function Impulsiveness v -> Some v | _ -> None)
    let Jealousy = ctor(Jealousy, function Jealousy v -> Some v | _ -> None)
    let Lecherousness = ctor(Lecherousness, function Lecherousness v -> Some v | _ -> None)
    let Luck = ctor(Luck, function Luck v -> Some v | _ -> None)
    let Obsession = ctor(Obsession, function Obsession(obsession, severity) -> Some (obsession, severity) | _ -> None)
    let Overconfidence = ctor(Overconfidence, function Overconfidence v -> Some v | _ -> None)
    let SenseOfDuty = ctor(SenseOfDuty, function SenseOfDuty v -> Some v | _ -> None)
    let ShortAttentionSpan = ctor(ShortAttentionSpan, function ShortAttentionSpan v -> Some v | _ -> None)
    let SignatureGear = ctor(SignatureGear, function SignatureGear v -> Some v | _ -> None)
    let StrikingST = ctor(StrikingST, function StrikingST v -> Some v | _ -> None)
    let TrademarkMove = ctor(TrademarkMove, function TrademarkMove v -> Some v | _ -> None)
    let Trickster = ctor(Trickster, function Trickster v -> Some v | _ -> None)
    let Vow = ctor(Vow, function Vow v -> Some v | _ -> None)
    let WeaponBond = ctor(WeaponBond, function WeaponBond t -> Some t | _ -> None)
    let WeaponMaster = ctor(WeaponMaster, function WeaponMaster v -> Some v | _ -> None)

    let OneWeapon = ctor(OneWeapon, function OneWeapon(v) -> Some (v) | _ -> None)
    let TwoWeapon = ctor(TwoWeapon, function TwoWeapon(weapon1, weapon2) -> Some (weapon1, weapon2) | _ -> None)

type Skill =
    | Weapon of WeaponSkill
    | Stealth
    | Camouflage
    | Observation

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
