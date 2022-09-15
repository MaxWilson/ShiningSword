
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

#nowarn "40" // we're not doing anything weird like calling a passed-in function in a ctor
let (|TitleWord|_|) = function
    // lookahead: a title word will have whitespace after it, otherwise it might be a college instead
    | OWS(Chars alphanumeric (name, (OWSStr "(VH)" (ctx & WS(_, _))))) -> Some(name, ctx)
    | OWS(Chars alphanumeric (name, ctx & WS(_, _))) -> Some(name, ctx)
    | _ -> None
let collegeChars = alphanumeric + Set.ofList ['.';'&']
let rec (|College|_|) = pack <| function
    | OWS(Chars collegeChars (name, OWSStr "/" (College(more, ctx)))) -> Some($"{name}/{more}", ctx)
    | OWS(Chars collegeChars (name, ctx)) -> Some(name, ctx)
    | _ -> None
let (|EndOfLine|_|) = function
    | (Char (('\n' | '\r'), ctx)) -> Some(ctx)
    | End as ctx -> Some(ctx)
    | _ -> None
let prereqChars = alphanumeric + Set.ofList ['/']
let (|PrereqWord|_|) = function // stuff like W1/BT1 is allowed
    | OWS(Chars prereqChars (v, OWS rest)) -> Some(v, rest)
    | _ -> None
let rec (|Prereq|_|) = pack <| function
    // an individual prereq, like "at least three necromancy spells"
    // Need to use lookahead to make sure not to eat the page number,
    // e.g. "Destroy Spirits Necro. C: PI3, at least three necromancy spells 59" should return "at least three necromancy spells" as prereq #2 but stop short of the 59+EOL
    | OWS(Int(_, EndOfLine _)) -> None // excluded case: NOT if it would eat the page number
    | OWS(PrereqWord(word, Prereq(_, ((arg, ix) as ctx))) & (_, startIx)) -> Some(arg.input.Substring(startIx, ix - startIx).Trim(), ctx) // recursive case: a word and the rest of the prereq
    | OWS(PrereqWord(word, ctx) & (_, startIx)) -> Some(word, ctx) // basis case: a word that does not end with a page number
    | _ -> None
let rec (|PrereqChain|_|) = pack <| function
    | OWS(Char(lead, _) & Prereq(word, OWSStr "," (PrereqChain(words, ctx)))) -> Some(word::words, ctx)
    | OWS(Char(lead, _) & Prereq(word, ctx)) -> Some([word], ctx)
    | _ -> None
let rec (|ClassPrereqs|_|) = pack <| function
    | OWS(Str "C:" (PrereqChain(prereqs, ctx))) -> Some("C: "::prereqs, ctx)
    | OWS(Str "W*:" (PrereqChain(prereqs, ctx))) -> Some("W*: "::prereqs, ctx)
    | OWS(Str "W:" (PrereqChain(prereqs, ctx))) -> Some("W: "::prereqs, ctx)
    | OWS(Str "D:" (PrereqChain(prereqs, ctx))) -> Some("D: "::prereqs, ctx)
    | _ -> None
let rec (|Prereqs|_|) = pack <| function
    | ClassPrereqs(prereqs, OWSStr "•" (Prereqs(more, ctx))) -> Some(prereqs::more, ctx)
    | ClassPrereqs(prereqs, ctx) -> Some([prereqs], ctx)
    | _ -> None
let (|Title|_|) = function
    | TitleWord(word1, TitleWord(word2, ctx)) -> Some(String.join " " [word1; word2], ctx)
    | TitleWord(word1, OWSStr "(VH)" ctx) -> Some(word1 + " (VH)", ctx)
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
    | Spell(spell, End) -> Ok (spell, txt)
    | Spell(spell, Any(rest, _)) ->
        Error (sprintf "%A...%s" spell rest, txt)
    | _ -> Error ($"Not a spell", txt)

let eachLine f (input:string) =
    let mutable accum = ""
    [for l in input.Split("\n") |> Array.rev do
        let line = l.Trim() + " " + accum
        match f line with
        | Ok spell as r ->
            accum <- ""
            yield r
        | Error err ->
            if line.Length < 100 then
                accum <- line // arbitrarily choose 100 chars as the point at which it's obviously not all one spell
            else
                printfn "Error: %s" line
                accum <- ""
        ] |> List.rev

match ParseArgs.Init "Vigil (VH) Mind C: PI4 56" with
| Spell(spell, _) ->
    sprintf "%A" spell
| TitleWord(name, College(college, Prereqs(prereqs, Int(pg, ctx)))) ->
    sprintf "%A" { name = name; college = college; prereqs = prereqs; page = pg }
| TitleWord(name, College(college, Prereqs(prereqs, _))) ->
    sprintf "%A" { name = name; college = college; prereqs = prereqs; page = 99 }
| TitleWord(name, College(college, _)) ->
    sprintf "%A" { name = name; college = college; prereqs = []; page = 99 }
| TitleWord(name, _) ->
    sprintf "%A" { name = name; college = "None"; prereqs = []; page = 99 }


"""Water Jet Water W: Shape Water 71
Water Vision Know./
Water
D: PI3 • W*: Shape
Water
71
Weaken M&B W: Find Weakness 50
Weaken Will Mind W*: M1/BT1,
Foolishness
56
Weather Dome P&W/
Weather
D: PI2 65
Windstorm Air D: PI2 • W: Shape
Air
17
Wisdom Mind C: PI3 • W*: 6 Mind
Control spells
56
Wither Limb Body W: M2, Paralyze
Limb
23
Wither Plant Plant D: PI4 63
Wizard Eye Know. W*: Apportation,
Keen Vision
45
""" |> eachLine parseDf

#load "GURPSSpellTextTable.fsx"
open DFData

dfSpells |> eachLine parseDf
|> ignore
|> agglomerate
|> Array.choose (function Error(msg, rest) -> (msg, rest) |> Some | _ -> None)