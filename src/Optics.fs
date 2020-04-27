module Optics

type 't OpticResult = Update of 't | Ignore
type 't OpticInput = 't -> 't OpticResult // in practice will either be a reader or a writer, analogous to Id or Const
type OpticOutput<'state> = 'state -> OpticResult<'state>
type Lens<'state, 'value> = Lens of ('value OpticInput -> OpticOutput<'state>)
    with
    member this.d = match this with Lens(l) -> l
    static member (=>)(outer: Lens<_,_>, inner: Lens<_,_>) =
        Lens(outer.d << inner.d)
    static member (=>)(outer: unit -> Lens<_,_>, inner: Lens<_,_>) =
        Lens(outer().d << inner.d)
    static member (=>)(outer: Lens<_,_>, inner: unit -> Lens<_,_>) =
        Lens(outer.d << inner().d)
    static member (=>)(outer: Lens<_,_>, inner: Prism<_,_>) : Lens<_,_> =
        notImpl()
    static member (=>)(outer: unit -> Lens<_,_>, inner: Prism<_,_>) : Lens<_,_> =
        notImpl()
    static member (=>)(outer: Lens<_,_>, inner: unit -> Prism<_,_>) : Lens<_,_> =
        notImpl()
    static member create (get: 'state -> 'value) (set: 'value -> 'state -> 'state) : Lens<'state, 'value> =
        fun (f:OpticInput<_>) s ->
            match (get s |> f : OpticResult<_>) with
            | Update v -> Update (set v s)
            | Ignore -> Ignore
        |> Lens


and Prism<'state, 'value> = Prism of (('value option) OpticInput -> OpticOutput<'state>)
    with
    member this.d = match this with Prism(l) -> l
    static member (=>)(outer: Prism<_,_>, inner: Prism<_,_>) =
        Prism(outer.d << inner.d)
    static member (=>)(outer: unit -> Prism<_,_>, inner: Prism<_,_>) =
        Prism(outer().d << inner.d)
    static member (=>)(outer: Prism<_,_>, inner: unit -> Prism<_,_>) =
        Prism(outer.d << inner().d)
    static member (=>)(outer: Prism<_,_>, inner: Lens<_,_>) : Prism<_,_> =
        notImpl()
    static member (=>)(outer: unit -> Prism<_,_>, inner: Lens<_,_>) : Prism<_,_> =
        notImpl()
    static member (=>)(outer: Prism<_,_>, inner: unit -> Lens<_,_>) : Prism<_,_> =
        notImpl()
    static member create (get: 'state -> 'value option) (set: 'value -> 'state -> 'state) : Prism<'state, 'value> =
        fun (f:OpticInput<_>) s ->
            match get s |> f with
            | Update (Some v) -> Update (set v s)
            | Update None -> Ignore
            | Ignore -> Ignore
        |> Prism

[<AbstractClass; Sealed>]
type Operations =
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
            match l (f >> Update) state with
            | Update v -> v
            | Ignore -> shouldntHappen()
    static member over (Prism prism: Prism<'state,'value>) : ('value -> 'value) -> 'state -> 'state =
        fun (f : 'value -> 'value) (state:'state) ->
            match prism (Option.map f >> Update) state with
            | Update v -> v
            | Ignore -> state
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
    Lens.create get set

let inline prism (get: 'state -> 'value option) (set: 'value -> 'state -> 'state) : Prism<_,_> =
    Prism.create get set
