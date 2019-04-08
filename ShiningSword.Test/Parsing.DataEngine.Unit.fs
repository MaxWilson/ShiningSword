module Parsing.DataEngine.Unit
open Common
open Xunit

[<Fact(DisplayName = "Embedded roll failures should still parse as regular log entries")>]
let EmbeddedRollFailures() =
    match Packrat.parser DataEngine.Parse.(|LogWithEmbeddedExpressions|_|) "test [(d20+5)?10]" with
    | DataEngine.Command.Log([msg]) -> Assert.Equal(DataEngine.Functions.Expression.text "test [(d20+5)?10]", msg)
    | v -> matchfail v
