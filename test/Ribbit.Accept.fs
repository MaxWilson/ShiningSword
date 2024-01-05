module Ribbit.Accept
open Expecto
open Swensen.Unquote

let bardsTaleSimple = """
number properties: HP, SP, AC, XP, GP
text properties: Name
property race: Elf, Dwarf, Human, Hobbit, Half-orc, Gnome, Half-elf
flags: Dead, Poisoned, Stoned, Insane
derived flag: OK when not Dead | Stoned | Insane

roll AttackRoll: roll-at-least on 1d20
// todo: define my current weapon, damage roll, damage type, ST, damage for ST, takeDamage

action attack (target) [requires turn when OK]:
    attack: AttackRoll vs target's AC
    if attack succeeds
        damage: my current weapon's mode's damage roll for my ST
        let injury = target.takeDamage(damage, my current weapon's mode's damage type)
        if target is Dead
            log "<Name> kills <target's Name>!" [attack, damage, injury, target.HP] // would auto-log all variables in scope, i.e. attack, damage, injury, but we'll be explicit here so we can include target's new HP
        else
            log "<Name> hits <target's Name> for <injury>" [+target.HP] // another way of specifying that we want to log target.HP even though it's not a variable
    else
        log "<Name> misses <target's Name>"
"""