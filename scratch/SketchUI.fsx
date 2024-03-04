#if INTERACTIVE
#load "../src/Core/Common.fs"
#endif

let connect init update view =
    let state = ref (init())
    let dispatch msg =
        state.Value <- update msg state.Value
        printfn "\n%s" (view state.Value)
    dispatch, state

let reconnect (state: _ ref) update view =
    printfn "\n[Reconnecting...]\n%s" (view state.Value)
    let dispatch msg =
        state.Value <- update msg state.Value
        printfn "\n%s" (view state.Value)
    dispatch, state
