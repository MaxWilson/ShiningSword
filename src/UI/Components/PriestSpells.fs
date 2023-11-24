module UI.PriestSpells
open Feliz

module Data =
    let spells = """
    All: Bless 1, Combine 1, Detect Evil 1, Purify Food & Drink 1, Atonement 5
    Animal: Animal Friendship 1, Invisibility to Animals 1, Locate Animals or Plants 1, Charm Person or Mammal 2, Messenger 2,
        Snake Charm 2, Speak With Animals 2, Hold Animal 3, Summon Insects 3, Animal Summoning I 4, Call Woodland Beings 4,
        Giant Insect 4, Repel Insects 4, Animal Growth 5, Animal Summoning II 5, Animal Summoning III 6, Anti-Animal Shell 6, Creeping Doom 7
    Astral: Plane Shift 5, Astral Spell 7
    Charm: Command 1, Remove Fear 1, Enthrall 2, Hold Person 2, Cloak of Bravery 4, Free Action 4, Imbue With Spell Ability 4, Quest 5, Confusion 7, Exaction 7
    Combat: Magical Stone 1, Shillelagh 1, Chant 2, Spiritual Hammer 2, Prayer 3, Flame Strike 5, Insect Plague 5, Holy Word 7
    Creation: Create Food & Water 3, Animate Object 6, Balde Barrier 6, Heroes' Feast 6, Wall of Thorns 6, Changestaff 7, Chariot of Sustarre 7
    Divination: Detect Magic 1, Detect Poison 1, Detect Snares & Pits 1, Locate Animals or Plants 1, Augury 2, Detect Charm 2, Find Traps 2,
        Know Alignment 2, Speak With Animals 2, Locate Object 3, Speak With Dead 3, Detect Lie 4
    """

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