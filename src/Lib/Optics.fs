module Optics

type 't OpticResult = Update of 't | Ignore
    with
    static member map f = function Ignore -> Ignore | Update(v) -> Update(f v)
type 't OpticInput = 't -> 't OpticResult // in practice will either be a reader or a writer, analogous to Id or Const
type OpticOutput<'state> = 'state -> OpticResult<'state>
type Lens<'state, 'value> = Lens of ('value OpticInput -> OpticOutput<'state>)
    with
    member this.d : 'value OpticInput -> OpticOutput<'state> = match this with Lens(l) -> l
    static member (?=>)(outer: Lens<_,_>, inner: Lens<_,_>) =
        //let model1 = (fun f -> inner.d f |> outer.d)
        //let model2 = (fun f -> outer.d (fun st -> inner.d f st))
        //let model3 = (fun f -> outer.d (fun st -> inner.d f st))
        Prism(fun f -> 
                (function
                    | Some data -> inner.d f data |> function Update v -> Update (Some v) | _ -> Ignore
                    | None -> Ignore)
                |> outer.d)    
    static member (=>)(outer: Lens<_,_>, inner: Lens<_,_>) =
        Lens(outer.d << inner.d)
    static member (=>)(outer: unit -> Lens<_,_>, inner: Lens<_,_>) =
        Lens(outer().d << inner.d)
    static member (=>)(outer: Lens<_,_>, inner: unit -> Lens<_,_>) =
        Lens(outer.d << inner().d)
    static member (=>)(outer: Lens<_,_>, inner: Prism<_,_>) : Prism<_,_> =
        Prism(outer.d << inner.d)
    static member (=>)(outer: unit -> Lens<_,_>, inner: Prism<_,_>) : Prism<_,_> =
        Prism(outer().d << inner.d)
    static member (=>)(outer: Lens<_,_>, inner: unit -> Prism<_,_>) : Prism<_,_> =
        Prism(outer.d << inner().d)
    static member create (get: 'state -> 'value) (set: 'value -> 'state -> 'state) : Lens<'state, 'value> =
        fun (f:OpticInput<_>) s ->
            match (get s |> f : OpticResult<_>) with
            | Update v -> Update (set v s)
            | Ignore -> Ignore
        |> Lens

and Prism<'state, 'value> = Prism of ('value OpticInput -> OpticOutput<'state>)
    with
    member this.d : 'value OpticInput -> OpticOutput<'state> = match this with Prism(l: 'value OpticInput -> OpticOutput<'state>) -> l
    static member (=>)(outer: Prism<_,_>, inner: Prism<_,_>) =
        Prism(outer.d << inner.d)
    static member (=>)(outer: unit -> Prism<_,_>, inner: Prism<_,_>) =
        Prism(outer().d << inner.d)
    static member (=>)(outer: Prism<_,_>, inner: unit -> Prism<_,_>) =
        Prism(outer.d << inner().d)
    static member (=>)(outer: Prism<'state,'value>, inner: Lens<'value,'innerValue>) : Prism<'state,'innerValue> =
        Prism(outer.d << inner.d)
    static member (=>)(outer: unit -> Prism<_,_>, inner: Lens<_,_>) : Prism<_,_> =
        Prism(outer().d << inner.d)
    static member (=>)(outer: Prism<_,_>, inner: unit -> Lens<_,_>) : Prism<_,_> =
        Prism(outer.d << inner().d)
    static member create (get: 'state -> 'value option) (set: 'value -> 'state -> 'state) : Prism<'state, 'value> =
        fun (f:OpticInput<'value>) s ->
            match get s with
            | None -> Ignore
            | Some v ->
                match f v with
                | Update v -> Update (set v s)
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
            prism (fun v -> retval <- Some v; Ignore) state |> ignore
            retval
    static member over (Lens l: Lens<'state,'value>) : ('value -> 'value) -> 'state -> 'state =
        fun (f : 'value -> 'value) (state:'state) ->
            match l (f >> Update) state with
            | Update v -> v
            | Ignore -> failwith "Shouldn't ever happen: over failed to return a value"
    static member over (Prism prism: Prism<'state,'value>) : ('value -> 'value) -> 'state -> 'state =
        fun (f : 'value -> 'value) (state:'state) ->
            match prism (f >> Update) state with
            | Update v -> v
            | Ignore -> state
    static member write (lens: Lens<'state,'value>) : 'value -> 'state -> 'state =
        fun (value:'value) (state: 'state) ->
            Operations.over lens (fun _ -> value) state
    static member write (Prism _ as prism: Prism<'state,'value>) : 'value -> 'state -> 'state =
        fun (value:'value) (state: 'state) ->
            Operations.over prism (fun _ -> value) state
    // convenience overload for a common pattern: write to a lens option
    static member writeSome (lens: Lens<'state,'value option>) : 'value -> 'state -> 'state =
        fun (value:'value) (state: 'state) ->
            Operations.over lens (fun _ -> Some value) state
    static member writeSome (prism: Prism<'state,'value option>) : 'value -> 'state -> 'state =
        fun (value:'value) (state: 'state) ->
            Operations.over prism (fun _ -> Some value) state
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
    static member writeSome(lens: unit -> Lens<'state, 'value option>) =
        Operations.writeSome(lens())

let inline lens (get: 'state -> 'value) (set: 'value -> 'state -> 'state) : Lens<_,_> =
    Lens.create get set

let inline prism (get: 'state -> 'value option) (set: 'value -> 'state -> 'state) : Prism<_,_> =
    Prism.create get set

module Tuple2 =
    let fst_() = lens fst (fun v st -> v, snd st)
    let snd_() = lens snd (fun v st -> fst st, v)

module Option =
    let some_() =
        prism id (fun v d -> Some v) 
    /// some__ is for if you know you're guaranteed to have some_ because you've already checked it
    let some__() =
        lens Option.get (fun v d -> Some v) 

module List =
    let nth_ n =
        prism (fun (l: _ list) -> if n >= l.Length then None else List.item n l |> Some) (fun v d -> d |> List.mapi (fun i x -> if i = n then v else x))
    /// some__ is for if you know you're guaranteed to have nth_ because you've already checked it
    let nth__ n =
        lens (List.item n) (fun v d -> d |> List.mapi (fun i x -> if i = n then v else x))

