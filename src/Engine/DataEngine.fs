(* The DataEngine consists of a domain-specific language (DSL) called Ribbit for RPG data manipulation, a parser
for turning text written in Ribbit into executable programs, and an interpreter for executing those programs, together with
F# interfaces for implementing the language host facilities like saving and loading data asynchronously.

Ribbit is inspired by Haskell and Prolog. You can think of it as performing a series of substitutions on the original
program until the result converges. Whitespace is significant. Ribbit is intended to be used in interactive mode within
a REPL loop, referencing chunks of pre-written code embodying e.g. the 5E ruleset with a given DM's houserules.

A simple Hello World program in Ribbit:

declare number hp, dmg default 0
define status = when dmg >= hp then dead when dmg > 0 then wounded else alive
hp = 10
status

This should evaluate to 'alive'. A slightly more complex program including actions might look like this:

module 5E =
  declare number hp, dmg default 0, ac, tohit
  declare weaponDamage
  define status = when dmg >= hp then dead when dmg > 0 then wounded else alive
  whenever hit:
    let dmg = roll me.weaponDamage
    change target.dmg add dmg
  whenever hit is crit:
    dmg = roll me.weaponDamage * 2
  whenever hit is crit and me has extra crit dice:
    dmg = roll (me.weaponDamage * 2 + me.extra crit dice)
  whenever attack target:
    let attackRoll = roll d20
    when attackRoll = 20 then hit (crit)
    when attackRoll + me.toHit >= target.AC then hit
    else miss

add bob, john
bob.hp = 10, ac = 12, tohit = 4, weaponDamage = 2d8+4
john.hp = 10

bob attack john

At the end of this program, if rolls are [d20 => 18, 2d8+4 => 12], john.status should evaluate to "dead".

*)

module DataEngine

