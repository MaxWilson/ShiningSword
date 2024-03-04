#if INTERACTIVE
#load "../src/Core/Common.fs"
#endif

let connect init update view =
    let mutable current = init()
    let dispatch msg =
        current <- update msg current
        printfn "\n%s" (view current)
    dispatch
