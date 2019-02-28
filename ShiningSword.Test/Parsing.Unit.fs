module ShiningSword.Test.Packrat.Unit

open Xunit
open Packrat

#nowarn "40" // ignore warnings about recursive active patterns via pack. It's fine in this case.
[<Fact(DisplayName="[Packrat] make sure that pack can define left-recursive grammars")>]
let VerifyLeftRecursion() =
  let (|Next|Empty|) ((ctx, pos): ParseInput) =
    if pos < ctx.input.Length then Next(ctx.input.[pos], (ctx, pos+1))
    else Empty
  // define an intermediate production "E" to make recursion indirect
  let rec (|CompoundExpression|_|) = pack (function
      | E(v, Next('+', Next('x', next))) -> Some(v+1, next)
      | Next('x', next) -> Some(1, next)
      | _ -> None)
  and (|E|_|) = pack (function
      | CompoundExpression(v, next) -> Some(v, next)
      | _ -> None)
  // It's a CompoundExpression, and it's also an E
  match ParseArgs.Init "x+x" with
  | CompoundExpression(v, Empty) -> Assert.Equal(2, v)
  | _ -> failwith "Could not parse"
  match ParseArgs.Init "x+x" with
  | E(v, Empty) -> Assert.Equal(2, v)
  | _ -> failwith "Could not parse"
  match ParseArgs.Init "x+x+x+x" with
  | E(4, Empty) -> ()
  | _ -> failwith "Could not parse"

[<Fact(DisplayName="[Packrat] demonstrate basic usage with grammar for converting yesno to binary-ish")>]
let BasicUsage() =
  let (|YesNo|_|) = pack <| function
    | Str "yes" rest -> Some(1I, rest)
    | Str "no" rest -> Some(0I, rest)
    | _ -> None
  let rec (|YesNos|_|) = pack <| function
    | YesNos(v1, OWS( YesNo(v2, rest) )) -> Some(v1*10I + v2, rest)
    | YesNo(v, rest) -> Some(v, rest)
    | _ -> None
  // It's a CompoundExpression, and it's also an E
  match ParseArgs.Init "yes no no yesYesNOnoYES" with
  | YesNos(v, End) -> Assert.Equal(10011001I, v)
  | _ -> failwith "Could not parse"
  match ParseArgs.Init " yes " with
  | YesNos(_) -> failwith "expected parse failure"
  | _ -> ()

[<Fact(DisplayName = "[Packrat] another test to ensure that left recursion is recognized")>]
let VerifyLeftRecursion2() =
  let rec (|Rolls|_|) = pack <| function
    | Rolls(vs, OWS( Char('+', Roll(v, rest) ))) -> Some(vs@[v], rest)
    | Roll(v, rest) -> Some([v], rest)
    | _ -> None
  and (|Roll|_|) = pack <| function
    | Rolls(lhs, OWS(Char('-', OWS(Int(rhs, rest))))) -> Some (List.sum lhs - float rhs, rest)
    | Rolls(rolls, rest) -> Some(List.sum rolls, rest)
    | Int(v, rest) -> Some(float v, rest)
    | Word("pi", rest) -> Some(3.14, rest)
    | _ -> None
  match ParseArgs.Init "pi-1" with
  | Roll(v, End) -> Assert.Equal(2.14, v)
  | _ -> failwith "Could not parse"

[<Fact(DisplayName = "[Packrat] check performance of non-recursive pack for boundedness")>]
let BoundsCheck() =
  let mutable counter = 0
  let (|C|_|) = pack <| fun input ->
    counter <- counter + 1
    match input with
    | Str "x" rest -> Some((), rest)
    | _ -> None
  let rec (|B|_|) = pack <| function
    | Root (_, Str "z" rest) -> Some((), rest)
    | A (_, Str "z" rest) -> Some((), rest)
    | C rest -> Some(rest)
    | C (_, C rest) -> Some(rest)
    | C (_, C (_, C rest)) -> Some(rest)
    | _ -> None
  and (|A|_|) = pack <| function
    | Root (_, Str "z" rest) -> Some((), rest)
    | B rest -> Some(rest)
    | B (_, C rest) -> Some(rest)
    | B rest -> Some(rest)
    | _ -> None
  and (|Root|_|) = pack <| function
    | C (_, Str "z" rest) -> Some((), rest)
    | C (_, Str "z" rest) -> Some((), rest)
    | A rest -> Some(rest)
    | A (_, C rest) -> Some(rest)
    | A rest -> Some(rest)
    | Str "y" rest -> Some((), rest)
    | _ -> None
  match ParseArgs.Init "xxy" with
  | A (_, A (_, Root(_, End))) -> Assert.True(counter <= 6) // Should evaluate C no more than twice in each position
  | _ -> failwith "Parse failure"

[<Fact(DisplayName = "[Packrat] show usage of additional context in ParseArgs for grammars that depend on outside context")>]
let VerifyContextSensitiveGrammarUsage() =
  let (|People|_|) = ExternalContextOf<string seq>
  let rec (|Person|_|) = pack <| function
    | Word(word, rest) & People people when Seq.contains word people -> Some(word, rest)
    | _ -> None
  let parseBase people v =
    match ParseArgs.Init(v, people) with
    | Person(v, rest) -> sprintf "Hello, %s" v
    | Word(v, rest) -> sprintf "Look, it's a %s" v
    | _ -> failwith "Parse failure"
  let parse = parseBase ["Ann"; "Bob"]
  Assert.Equal("Hello, Bob", parse "Bob")
  Assert.Equal("Look, it's a Eraser", parse "Eraser")
  Assert.Equal("Hello, Ann", parse "Ann")
  Assert.Equal("Look, it's a Bob", parseBase ([]:string list) "Bob")

[<Theory(DisplayName = "[Packrat] Verify non-trivial helper function LongestSubstringWhere")>]
[<InlineData("zzabcdzzzzefg", "zz", 12, "abcdzzzz")>]
[<InlineData("zzabcdzzzzefg", "zz", 9, "abcdzzzz")>]
[<InlineData("zzabcdzzzzefg", "zz", 8, "abcdzzzz")>]
[<InlineData("zzabcdzzzzefg", "zz", 6, "abcdzz")>]
[<InlineData("zzabcdzzzzefg", "zz", 5, "abcdz")>]
[<InlineData("zzabcdzzzzefg", "zz", 4, "No match")>]
[<InlineData("zzabcdz", "zz", 5, "abcdz")>]
[<InlineData("abcdz", "", 5, "abcdz")>]
let VerifyLongestSubstringWhere(input, prefix, maxLen, expected) =
    let pred = (fun (s:string) -> s.EndsWith("z"))
    let actual =
        match ParseArgs.Init input with // initial zz to make sure LongestSubstringWhere is testing the matched substring and not the whole string
        | Str prefix (LongestSubstringWhere pred maxLen (v, ctx)) -> v
        | _ -> "No match"
    Assert.Equal(expected, actual)
