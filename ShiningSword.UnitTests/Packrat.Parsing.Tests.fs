module Packrat.Parsing.Tests
open Common
open Packrat
open Expecto

#nowarn "40" // we're not planning on doing any unsafe things during initialization, like evaluating the functions that rely on the object we're busy constructing

// Helper to give better error messages on parse fail
let parseFail ((args: ParseArgs), _) = Tests.failtestf "Could not parse %s" args.input

[<Tests>]
let tests = testList "packrat.unit" [
    testCase "Make sure that pack can define left-recursive grammars" <| fun _ ->
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
        | CompoundExpression(v, Empty) -> Expect.equal v 2 "There are two xs so we the data structure should derive 2 as the answer"
        | v -> parseFail v
        match ParseArgs.Init "x+x" with
        | E(v, Empty) -> Expect.equal v 2 "Failed to generate correct data structure"
        | v -> parseFail v
        match ParseArgs.Init "x+x+x+x" with
        | E(4, Empty) -> ()
        | v -> parseFail v

    testCase "Demonstrate basic usage with grammar for converting yesno to binary-ish" <| fun _ ->
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
        | YesNos(v, End) -> Expect.equal v 10011001I "Yes/no should translate to binary digits 1/0 respectively"
        | v -> parseFail v
        match ParseArgs.Init " yes " with
        | YesNos(_) -> Tests.failtest "Expected a parse failure due to leading whitespace, which the grammar forbids"
        | _ -> () // expected

    testCase "Another test to ensure that left recursion is recognized" <| fun _ ->
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
        | Roll(v, End) -> Expect.equal v 2.14 "Miscalculated pi - 1"
        | v -> parseFail v

    testCase "Check performance of non-recursive pack for boundedness" <| fun _ ->
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
        | A (_, A (_, Root(_, End))) -> Expect.isTrue (counter <= 6) "Should evaluate C no more than twice in each position"
        | v -> parseFail v

    testCase "Show usage of additional context in ParseArgs for grammars that depend on outside context" <| fun _ ->
        let (|People|_|) = ExternalContextOf<string seq>
        let rec (|Person|_|) = pack <| function
            | Word(word, rest) & People people when Seq.contains word people -> Some(word, rest)
            | _ -> None
        let parseBase people v =
            match ParseArgs.Init(v, people) with
            | Person(v, rest) -> sprintf "Hello, %s" v
            | Word(v, rest) -> sprintf "Look, it's a %s" v
            | v -> parseFail v
        let parse = parseBase ["Ann"; "Bob"]
        Expect.equal (parse "Bob") "Hello, Bob" "Should recognize that Bob is a person"
        Expect.equal (parse "Eraser") "Look, it's a Eraser" "Should recognize that Eraser is an object"
        Expect.equal (parse "Ann") "Hello, Ann" "Should recognize Ann as a person"
        Expect.equal (parseBase ([]:string list) "Bob") "Look, it's a Bob" "Ability to recognize Bob as a person depends on Bob being in the list of people it's trained on"
    testCase "Verify non-trivial helper function LongestSubstringWhere" <| fun _ ->
            let testParameters = [
                "zzabcdzzzzefg", "zz", 12, "abcdzzzz"
                "zzabcdzzzzefg", "zz", 9, "abcdzzzz"
                "zzabcdzzzzefg", "zz", 8, "abcdzzzz"
                "zzabcdzzzzefg", "zz", 6, "abcdzz"
                "zzabcdzzzzefg", "zz", 5, "abcdz"
                "zzabcdzzzzefg", "zz", 4, "No match"
                "zzabcdz", "zz", 5, "abcdz"
                "abcdz", "", 5, "abcdz"
                ]
            let t = Expecto.Tests.test
            flip List.iteri testParameters <| fun i (input, prefix, maxLen, expected) ->
                let pred = (fun (s:string) -> s.EndsWith("z"))
                let actual =
                        match ParseArgs.Init input with // initial zz to make sure LongestSubstringWhere is testing the matched substring and not the whole string
                        | Str prefix (LongestSubstringWhere pred maxLen (v, ctx)) -> v
                        | _ -> "No match"
                Expect.equal actual expected "Incorrect data structure generated"
    ]
