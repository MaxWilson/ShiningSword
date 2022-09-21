
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

type MagicType = Clerical | Wizardly | Druidic
type Prerequisite = { heading: MagicType * string; items: string list }
    with static member Create(magicType, primaryLabel, items) = { heading = (magicType, primaryLabel); items = items }
type Spell = { name: string; college: string; prereqs: Prerequisite list; page: int }

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

#nowarn "40" // we're not doing anything weird like calling a passed-in function in a ctor
module DF =
    let titlewordChars = alphanumeric + Set.ofList ['-';'’'] // No-Smell is a valid spell name, ditto Monk's Banquet
    let (|TitleWord|_|) = function
        // lookahead: a title word will have whitespace after it, otherwise it might be a college instead
        | OWS(Chars titlewordChars (name, (OWSStr "(VH)" (ctx & WS(_, _))))) -> Some(name, ctx)
        | OWS(Chars titlewordChars (name, ctx & WS(_, _))) -> Some(name, ctx)
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
    let prereqChars = alphanumeric + Set.ofList ['/';'+';'-';'&';'.';'“';'”']
    let (|PrereqWord|_|) = function // stuff like W1/BT1, IQ 13+, or 6 L&D spells is allowed
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
        | OWS(Str "C:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Clerical, "C", prereqs), ctx)
        | OWS(Str "W*:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Wizardly, "W*", prereqs), ctx)
        | OWS(Str "W:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Wizardly, "W", prereqs), ctx)
        | OWS(Str "D:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Druidic, "D", prereqs), ctx)
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
    let rec (|Spell|_|) = pack <| function
        // specific should come before general, but in this case having a shorter title is (I think) more specific because of the EndOfLine qualifier. Or is it?
        | TitleWord(pt1, Spell(spell, ctx)) -> Some({ spell with name = String.join " " [pt1;spell.name] }, ctx)
        | TitleWord(name, College(college, Prereqs(prereqs, Int(pg, ctx)))) ->
            Some({ name = name; college = college; prereqs = prereqs; page = pg }, ctx)
        | _ -> None

    let parse txt =
        match ParseArgs.Init(txt) with
        | Spell(spell, End) -> Ok (spell, txt)
        | Spell(spell, Any(rest, _)) ->
            Error (sprintf "%A...%s" spell rest, txt)
        | _ -> Error ($"Not a spell", txt)

module Magic =
    let titlewordChars = alphanumeric + Set.ofList ['-';'’'] // No-Smell is a valid spell name, ditto Monk's Banquet
    let (|TitleWord|_|) = function
        // lookahead: a title word will have whitespace after it, otherwise it might be a college instead
        | OWS(Chars titlewordChars (name, (OWSStr "(VH)" (ctx & WS(_, _))))) -> Some(name, ctx)
        | OWS(Chars titlewordChars (name, ctx & WS(_, _))) -> Some(name, ctx)
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
    let prereqChars = alphanumeric + Set.ofList ['/';'+';'-';'&';'.';'“';'”']
    let (|PrereqWord|_|) = function // stuff like W1/BT1, IQ 13+, or 6 L&D spells is allowed
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
        | OWS(Str "C:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Clerical, "C", prereqs), ctx)
        | OWS(Str "W*:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Wizardly, "W*", prereqs), ctx)
        | OWS(Str "W:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Wizardly, "W", prereqs), ctx)
        | OWS(Str "D:" (PrereqChain(prereqs, ctx))) -> Some(Prerequisite.Create(Druidic, "D", prereqs), ctx)
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
    let rec (|Spell|_|) = pack <| function
        // specific should come before general, but in this case having a shorter title is (I think) more specific because of the EndOfLine qualifier. Or is it?
        | TitleWord(pt1, Spell(spell, ctx)) -> Some({ spell with name = String.join " " [pt1;spell.name] }, ctx)
        | TitleWord(name, College(college, Prereqs(prereqs, Int(pg, ctx)))) ->
            Some({ name = name; college = college; prereqs = prereqs; page = pg }, ctx)
        | _ -> None

    let parse txt =
        match ParseArgs.Init(txt) with
        | Spell(spell, End) -> Ok (spell, txt)
        | Spell(spell, Any(rest, _)) ->
            Error (sprintf "%A...%s" spell rest, txt)
        | _ -> Error ($"Not a spell", txt)

let magicSample = """
    113 Continual Mage Regular Lt-Dk Varies Varies 1 sec. Mage Light, Continual Light 6
    Light
    114 Continual Sunlight Area Lt-Dk Varies 3 1 sec. Sunlight 6
    """
for spell, _txt in magicSample |> eachLine Magic.parse |> List.map (Result.toOption >> Option.get) do
    printfn $"""{spell.name} {spell.prereqs |> List.map (fun p -> p.items |> String.join ", " |> sprintf "%A: [%s]" p.heading) |> String.join " or "}"""

#load "GURPSSpellTextTable.fsx"
open DFData

#r "nuget: TextCopy"
let showSuspects =
    let out = System.Text.StringBuilder()
    let rec recur prior = function
        | line::rest ->
            if (line:string).Length < 50 then
                out.AppendLine(sprintf "%s\n%s\n" prior line) |> ignore
            recur line rest
        | [] -> ()
    fun txt -> recur "" txt; out.ToString() |> TextCopy.ClipboardService.SetText; out.ToString() |> printfn "%s"
magic.Split("\n") |> List.ofArray |> showSuspects

for spell, _txt in dfSpells |> eachLine DF.parse |> List.map (Result.toOption >> Option.get) do
    printfn $"""{spell.name} {spell.prereqs |> List.map (fun p -> p.items |> String.join ", " |> sprintf "%A: [%s]" p.heading) |> String.join " or "}"""

let writer = System.Text.StringBuilder()
let mutable dupes = Set.empty
let append txt =
    if dupes.Contains txt |> not then
        writer.AppendLine txt |> ignore
        dupes <- dupes |> Set.add txt
// format for graphviz
for spell, _txt in dfSpells |> eachLine parseDf |> List.map (Result.toOption >> Option.get) do
    for group in spell.prereqs do
        let grouptxt = group.items |> String.join ", " |> sprintf "[%s]"
        (sprintf "    %A -> %A [color=red]" spell.name grouptxt) |> append
        for prereq in group.items do
            (sprintf "    %A -> %A [color=blue]" grouptxt prereq) |> append
TextCopy.Clipboard().SetText ($"
digraph G {{
{writer.ToString()}
    start->PI1
    PI1->PI2
    PI2->PI3
    start [shape=Mdiamond];
}}
")
