[<AutoOpen>]
module Domain.Character.DungeonFantasy.TraitsAndAttributes
open Domain.Ribbit.Properties

type Race = Human | Catfolk | Dwarf | Elf | HalfElf | Gnome | HalfOgre | HalfOrc | Halfling | Coleopteran
type Profession = Barbarian | Bard | Cleric | Druid | HolyWarrior | Knight | MartialArtist | Scout | Swashbuckler | Thief | Wizard
type WeaponType = Unarmed | Rapier | Broadsword | Bow | MainGauche | Knife | Spear | Polearm | Staff
type Trait =
    | HighPainThreshold
    | CombatReflexes
    | EnhancedParry of WeaponType

type Skill =
    | Weapon of WeaponType
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
