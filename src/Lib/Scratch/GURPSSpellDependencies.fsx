
// in order to compare spell prereqs, we copy and paste from the respective PDF tables into strings, and then write a parser over each string.
#I __SOURCE_DIRECTORY__
#I ".."
#I "..\Core"
#load @"Optics.fs"
#load @"Common.fs"
#load @"Packrat.fs"
#load "GURPSSpellTextTable.fsx"
open Packrat
open DFData

let (|TitleWord|_|) = function
    // lookahead: a title word will have whitespace after it, otherwise it might be a college instead
    | OWS(Chars alphanumeric (name, ctx & WS(_, _))) -> Some(name, ctx)
    | _ -> None
let collegeChars = alphanumeric + Set.ofList ['.';'&']
let (|College|_|) = function
    | OWS(Chars collegeChars (name, ctx)) -> Some(name, ctx)
    | _ -> None
let (|EndOfLine|_|) = function
    | (Char (('\n' | '\r'), ctx)) -> Some(ctx)
    | End as ctx -> Some(ctx)
    | _ -> None
let rec (|Prereq|_|) = pack <| function
    // an individual prereq, like "at least three necromancy spells"
    // Need to use lookahead to make sure not to eat the page number,
    // e.g. "Destroy Spirits Necro. C: PI3, at least three necromancy spells 59" should return "at least three necromancy spells" as prereq #2 but stop short of the 59+EOL
    | OWS(Int(_, EndOfLine _)) -> None // excluded case: NOT if it would eat the page number
    | OWS(Word(word, Prereq(_, ((arg, ix) as ctx))) & (_, startIx)) -> Some(arg.input.Substring(startIx, ix - startIx).Trim(), ctx) // recursive case: a word and the rest of the prereq
    | OWS(Word(word, ctx) & (_, startIx)) -> Some(word, ctx) // basis case: a word that does not end with a page number
    | _ -> None
let rec (|PrereqChain|_|) = pack <| function
    | OWS(Char(lead, _) & Prereq(word, OWSStr "," (PrereqChain(words, ctx)))) when System.Char.IsLetter lead -> Some(word::words, ctx)
    | OWS(Char(lead, _) & Prereq(word, ctx)) when System.Char.IsLetter lead -> Some([word], ctx)
    | _ -> None
let rec (|ClassPrereqs|_|) = pack <| function
    | OWS(Str "C:" (PrereqChain(prereqs, ctx))) -> Some(prereqs, ctx)
    | OWS(Str "W*:" (PrereqChain(prereqs, ctx))) -> Some(prereqs, ctx)
    | OWS(Str "W:" (PrereqChain(prereqs, ctx))) -> Some(prereqs, ctx)
    | OWS(Str "D:" (PrereqChain(prereqs, ctx))) -> Some(prereqs, ctx)
    | _ -> None
let rec (|Prereqs|_|) = pack <| function
    | ClassPrereqs(prereqs, OWSStr "•" (Prereqs(more, ctx))) -> Some(prereqs::more, ctx)
    | ClassPrereqs(prereqs, ctx) -> Some([prereqs], ctx)
    | _ -> None
#nowarn "40" // we're not doing anything weird like calling a passed-in function in a ctor
let (|Title|_|) = function
    | TitleWord(word1, TitleWord(word2, ctx)) -> Some(String.join " " [word1; word2], ctx)
    | TitleWord(word1, ctx) -> Some(word1, ctx)
    | _ -> None
type Spell = { name: string; college: string; prereqs: string list list; page: int }
let rec (|Spell|_|) = pack <| function
    // specific should come before general, but in this case having a shorter title is (I think) more specific because of the EndOfLine qualifier. Or is it?
    | TitleWord(pt1, Spell(spell, ctx)) -> Some({ spell with name = String.join " " [pt1;spell.name] }, ctx)
    | TitleWord(name, College(college, Prereqs(prereqs, Int(pg, ctx)))) ->
        Some({ name = name; college = college; prereqs = prereqs; page = pg }, ctx)
    | _ -> None

let parseDf txt =
    match ParseArgs.Init(txt) with
    | Spell(spell, End) -> Ok spell
    | Spell(spell, Any(rest, _)) ->
        Error (sprintf "%A" spell, rest)
    | _ -> Error ($"Not a spell", txt)

let eachLine f (input:string) =
    [for l in input.Split("\n") do
        yield f (l.Trim())
        ]

"Affect Spirits Necro. C: PI3 59
Agonize Body W: M2, Sensitize 20
Spirits Necro. C: PI3 59
Destroy Spirits Necro. C: PI3, at least three necromancy spells 59
" |> eachLine parseDf

dfSpells |> eachLine parseDf |> List.choose (function Error(msg, rest) -> (msg, rest) |> Some | _ -> None)
