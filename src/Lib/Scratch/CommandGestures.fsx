

// This script is purely for messing around with graph visualization.
#I __SOURCE_DIRECTORY__
#I ".."
#load @"Optics.fs"
#load @"Common.fs"

// This is a proof-of-concept for something that can build commands, gdb-style.

(*
Name = any name in ctx
NewName = any string
addGesture = Gesture [Terminal add; name => NewName]
addGesture.Validate "add Bob" = true
addGesture
Bob hits Beholder for 20

For discrete things, should provide some kind of enumeration of possible choices. E.g. for verbs/affordances like "attack"/"dodge"/"cast" and spells like "Fireball".
For other things like NewName, anything is valid, so just need to be able to generate and validate.
Target is sort of in-between. Might be able to enumerate targets, but maybe not positions.

Maybe this is all overgeneralized and wrongheaded. In order to work with gestures, we don't need to be able to reproduce every CFG in arbitrary order.
We only need to be able to add stuff to the end while still maintaining a valid command state. E.g. we don't need to be able to say
hits
Bob
Harry
27
for

We only need to be able to say
Harry
hits
Bob
for
27

*)

// Atomic Gesture ~== part of speech.
type AtomicGesture = | Name | Verb | Position | Amount

type IMadlib =
    abstract SuggestRefinement: AtomicGesture * string
    abstract ValidateRefinement: AtomicGesture * string -> bool
    abstract Refine: AtomicGesture * string -> IMadlib option
    abstract CommandText: string option

type CommandGesture() =
    interface IMadlib with
        member this.SuggestRefinement: AtomicGesture * string = notImpl()
        member this.Refine(arg1: AtomicGesture, arg2: string): IMadlib option = notImpl()
        member this.ValidateRefinement(arg1: AtomicGesture, arg2: string): bool = notImpl()
        member this.CommandText = notImpl()

// Should correspond to active patterns used in parsing. Used to compose complete CommandGestures.
// Inspired by CFGs. But how do we give semantics to the grammar? Is that generic?
// Also need to be able to refer to the pieces of it, in order to provide Mad-lib context to the user.
type CommandFragment =
    | Terminal of AtomicGesture
    | Nonterminal of CommandFragment list
