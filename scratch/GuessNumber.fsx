#load "SketchUI.fsx"
open SketchUI

let init = ()
let update msg model = model
let view model =
    "Hello, world! Notice that there's no dispatch argument"
let send: unit -> unit = connect init update view

send()