module UI.PriestSpells
open Feliz

#nowarn "40" // Disabling the recursive initialization warning for this file because the parser is recursive, but isn't doing anything weird like calling ctor args during construction.

module Data =
    let spheres = """
    All: Bless 1, Combine 1, Detect Evil 1, Purify Food & Drink 1, Atonement 5
    Animal: Animal Friendship 1, Invisibility to Animals 1, Locate Animals or Plants 1, Charm Person or Mammal 2, Messenger 2,
        Snake Charm 2, Speak With Animals 2, Hold Animal 3, Summon Insects 3, Animal Summoning I 4, Call Woodland Beings 4,
        Giant Insect 4, Repel Insects 4, Animal Growth 5, Animal Summoning II 5, Animal Summoning III 6, Anti-Animal Shell 6, Creeping Doom 7
    Astral: Plane Shift 5, Astral Spell 7
    Charm: Command 1, Remove Fear 1, Enthrall 2, Hold Person 2, Cloak of Bravery 4, Free Action 4, Imbue With Spell Ability 4, Quest 5, Confusion 7, Exaction 7
    Combat: Magical Stone 1, Shillelagh 1, Chant 2, Spiritual Hammer 2, Prayer 3, Flame Strike 5, Insect Plague 5, Holy Word 7
    Creation: Create Food & Water 3, Animate Object 6, Balde Barrier 6, Heroes' Feast 6, Wall of Thorns 6, Changestaff 7, Chariot of Sustarre 7
    Divination: Detect Magic 1, Detect Poison 1, Detect Snares & Pits 1, Locate Animals or Plants 1, Augury 2, Detect Charm 2, Find Traps 2,
        Know Alignment 2, Speak With Animals 2, Locate Object 3, Speak With Dead 3, Detect Lie 4, Divination 4, Reflecting Pool 4, Tongues 4, Commune 5,
        Commune With Nature 5, Magic Font 5, True Seeing 5, Find the Path 6, Speak With Monsters 6
    Elemental: Create Water 1, Dust Devil 2, Fire Trap 2, Flame Blade 2, Heat Metal 2, Produce Flame 2, Flame Walk 3, Meld Into Stone 3, Protection From Fire 3,
        Pyrotechnics 3, Stone Shape 3, Water Breathing 3, Water Walk 3, Lower Water 4, Produce Fire 4, Air Walk 5, Commune With Nature 5, Spike Stones 5, Transmute Rock to Mud 5,
        Wall of Fire 5, Conjure Fire Elemental 6, Fire Seeds 6, Part Water 6, Stone Tell 6, Transmute Water to Dust 6, Animate Rock 7, Chariot of Sustarre 7, Conjure Earth Elemental 7,
        Earthquake 7, Fire Storm 7, Transmute Metal to Wood 7, Wind Walk 7
    Guardian: Silence 2, Wyvern Watch 2, Glyph of Warding 3, Blade Barrier 6, Symbol 7
    Healing: Cure Light Wounds 1, Slow Poison 2, Cure Serious Wounds 4, Neutralize Poison 4, Cure Critical Wounds 5, Heal 6
    Necromantic: Invisibility to Undead 1, Aid 2, Animate Dead 3, Cure Blindness or Deafness 3, Cure Disease 3, Feign Death 3, Negative Plane Protection 3, Raise Dead 5,
        Regenerate 7, Reincarnate 7, Restoration 7, Resurrection 7
    Plant: Entangle 1, Pass Without Trace 1, Shillelagh 1, Barksin 2, Goodberry 2, Trip 2, Warp Wood 2, Plant Growth 3, Snare 3, Spike Growth 3, Tree 3, Hallucinatory Forest 4,
        Hold Plant 4, Plant Door 4, Speak With Plants 4, Sticks to Snakes 4, Anti-Plant Shell 5, Pass Plant 5, Liveoak 6, Transport Via Plants 6, Turn Wood 6, Wall of Thorns 6, Changestaff 7
    Protection: Endure Cold 1, Endure Heat 1, Protection From Evil 1, Sanctuary 1, Barkskin 2, Resist Fire 2, Resist Cold 2, Withdraw 2, Dispel Magic 3, Magical Vestment 3,
        Negative Plane Protection 3, Protection From Fire 3, Remove Curse 3, Remove Paralysis 3, Protection From Evil 10' Radius 4, Protection From Lightning 4, Repel Insects 4,
        Spell Immunity 4, Anti-Plant Shell 5, Dispel Evil 5, Anti-Animal Shell 6
    Summoning: Abjure 4, Animal Summoning I 4, Call Woodland Beings 4, Animal Summoning II 5, Dispel Evil 5, Aerial Servant 6, Animal Summoning III 6, Animate Objects 6, Conjure Animals 6,
        Wall of Thorns 6, Weather Summoning 6, Word of Recall 6, Conjure Earth Elemental 7, Creeping Doom 7, Exaction 7, Gate 7, Succor 7
    Sun: Light 1, Continual Light 3, Starshine 3, Moonbeam 5, Rainbow 5, Sunray 7
    Weather: Faerie Fire 1, Obscurement 2, Call Lightning 3, Control Temperature 10' Radius 4, Protection From Lightning 4, Control Winds 5, Rainbow 5, Weather Summoning 6, Control Weather 7
    """
    module Parser =
        // #load @"c:\code\rpg\src\Core\Common.fs"
        // #load @"c:\code\rpg\src\Core\CQRS.fs"
        // #load @"c:\code\rpg\src\Core\Coroutine.fs"
        // #load @"c:\code\rpg\src\Core\Packrat.fs"
        open Packrat
        let (|SpellLevel|_|) = function
            | OWS(Int(n, ((Char((',' | '\n' | '\r'), _) | End) as rest))) -> Some(n, rest)
            | _ -> None
        let (|NameChunk|_|) =
            let chars = alphanumeric + Set.ofList ['\''; ','; '&';] // e.g. Protection From Evil, 10' Radius has a comma and an apostrophe; some spells have &
            function
            | SpellLevel _ -> None // spell level takes priority over NameChunk; don't mistake a " 1" for " 10' Radius"
            | OWS(Chars chars (v, OWS rest)) -> Some(v, rest)
            | _ -> None
        let rec (|Names|_|) = pack <| function
            | NameChunk(lhs, Names(rhs, rest)) -> Some(lhs + " " + rhs, rest)
            | NameChunk(v, rest) -> Some(v, rest)
            | _ -> None
        let rec (|Spell|_|) = function
            | Names(name, SpellLevel(level, rest)) -> Some((name, level), rest)
            | _ -> None
        let rec (|Spells|_|) = pack <| function
            | Spell(lhs, Str "," (Spells(rhs, rest))) -> Some(lhs :: rhs, rest)
            | Spell(v, rest) -> Some([v], rest)
            | _ -> None

        let rec (|Sphere|_|) = function
            | Names(sphereName, Str ":" (Spells(spells, rest))) -> Some((sphereName, spells), rest)
            | _ -> None
        let partial (|Recognizer|_|) txt = match ParseArgs.Init txt with | Recognizer(v, _) -> v
        let partialR (|Recognizer|_|) txt = match ParseArgs.Init txt with | Recognizer(v, (input, pos)) -> v, input.input.Substring pos

        partialR (|SpellLevel|_|) """5
    Fireball 3"""
        partial (|Sphere|_|) spheres

module Impl =
    type Model = { filter: string }
    type Msg = NoOp
    let init() = { filter = "" }
    let update msg model = model
open Impl

[<ReactComponent>]
let View() =
    let model, dispatch = React.useElmishSimple init update
    Html.div [
        Html.h1 "Priest Spells"
        ]