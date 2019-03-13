module Model.Functions.Unit
open Common
open Common.Hierarchy
open Model.Types
open Model.Functions
open Xunit

[<Fact>]
let VerifyLogExtract() =
    let log = Log.empty |> Log.log "El opens the door" |> Log.logDetailed (Nested("El sees 6 orcs", [Leaf("El sees [d6] orcs"); Nested("d6: 6", [Leaf("d6"); Leaf("6")])]))
    Assert.Equal("El opens the door\nEl sees 6 orcs", log |> Log.getEntriesAsText |> Log.page None |> String.join "\n")
    let indentify = Common.Hierarchy.mapReduce (fun parents v -> String.replicate parents.Length "  " + v) (fun v children -> String.join "\n" (v::children))
    Assert.Equal("El opens the door\nEl sees 6 orcs\n  El sees [d6] orcs\n  d6: 6", log |> Log.extractEntries |> Log.truncateDetail 1 |> Log.page None |> List.map indentify |> String.join "\n")

