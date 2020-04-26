module rec Optics

type 't LensValue = Return of 't | Ignore
type 't LensInput = 't -> 't LensValue // in practice will either be a reader or a writer, analogous to Id or Const
type LensOutput<'state> = 'state -> LensValue<'state>
type PrismOutput<'state> = 'state -> LensValue<'state>
type Lens<'state, 'value> = Lens of ('value LensInput -> LensOutput<'state>)
    with
    static member (=>)(outerLens: Lens<_,_>, innerLens: Lens<_,_>) =
        Operations.compose(outerLens, innerLens)
    static member (=>)(outerLens: unit -> Lens<_,_>, innerLens: Lens<_,_>) =
        Operations.compose(outerLens(), innerLens)
    static member (=>)(outerLens: Lens<_,_>, innerLens: unit -> Lens<_,_>) =
        Operations.compose(outerLens, innerLens())
    static member (=>)(outerLens: unit -> Lens<_,_>, innerLens: unit -> Lens<_,_>) =
        Operations.compose(outerLens(), innerLens())

type Prism<'state, 'value> = Prism of (('value option) LensInput -> PrismOutput<'state>)

[<AbstractClass; Sealed>]
type Operations =
    static member compose(Lens outerLens, Lens innerLens): Lens<_,_> =
        Lens(innerLens >> outerLens)
    static member read(Lens lens: Lens<'state, 'value>): 'state -> 'value =
        fun state ->
            let mutable retval = Unchecked.defaultof<_>
            lens (fun v -> retval <- v; Ignore) state |> ignore
            retval
    static member read(Prism prism: Prism<'state, 'value>): 'state -> 'value option =
        fun state ->
            let mutable retval = None
            prism (fun v -> retval <- v; Ignore) state |> ignore
            retval
    static member over (Lens l: Lens<'state,'value>) : ('value -> 'value) -> 'state -> 'state =
        fun (f : 'value -> 'value) (state:'state) ->
            match l (f >> Return) state with
            | Return v -> v
            | Ignore -> shouldntHappen()
    static member over (Prism prism: Prism<'state,'value>) : ('value -> 'value) -> 'state -> 'state =
        fun (f : 'value -> 'value) (state:'state) ->
            match prism (Option.map f >> Return) state with
            | Return v -> v
            | Ignore -> shouldntHappen()
    static member write (lens: Lens<'state,'value>) : 'value -> 'state -> 'state =
        fun (value:'value) (state: 'state) ->
            Operations.over lens (fun _ -> value) state
    static member write (Prism _ as prism: Prism<'state,'value>) : 'value -> 'state -> 'state =
        fun (value:'value) (state: 'state) ->
            Operations.over prism (fun _ -> value) state
    // convenience overloads for parametric-polymorphic lenses
    static member read(lens: unit -> Lens<'state, 'value>): 'state -> 'value =
        Operations.read(lens())
    static member read(prism: unit -> Prism<'state, 'value>): 'state -> 'value option =
        Operations.read(prism())
    static member over(lens: unit -> Lens<'state, 'value>) =
        Operations.over(lens())
    static member over(prism: unit -> Prism<'state, 'value>) =
        Operations.over(prism())
    static member write(lens: unit -> Lens<'state, 'value>) =
        Operations.write(lens())
    static member write(prism: unit -> Prism<'state, 'value>) =
        Operations.write(prism())

let inline lens (get: 'state -> 'value) (set: 'value -> 'state -> 'state) : Lens<_,_> =
    let lens : Lens<'state, 'value> =
        fun (f:LensInput<_>) s ->
            match (get s |> f : LensValue<_>) with
            | Return v -> Return (set v s)
            | Ignore -> Ignore
        |> Lens
    lens

let inline prism (get: 'state -> 'value option) (set: 'value -> 'state -> 'state) : Prism<_,_> =
    let prism : Prism<'state, 'value> =
        fun (f:LensInput<_>) s ->
            match get s |> f with
            | Return (Some v) -> Return (set v s)
            | Return None -> Return s
            | Ignore -> Ignore
        |> Prism
    prism
