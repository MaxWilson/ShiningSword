module Model.Operations
open Model.Types

// executes action declarations in listed order
let execute (r:Roster) (d: Declarations) : Roster =
    r
