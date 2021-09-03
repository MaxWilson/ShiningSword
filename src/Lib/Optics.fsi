module Optics
type 't OpticResult =
  | Update of 't
  | Ignore
  with
    static member map : f:('a -> 'b) -> ('a OpticResult -> 'b OpticResult)
  end
type 't OpticInput = 't -> 't OpticResult
type OpticOutput<'state> = 'state -> 'state OpticResult
type Lens<'state,'value> =
  | Lens of ('value OpticInput -> OpticOutput<'state>)
  with
    static member
      ( => ) : outer:Lens<'a,'b> * inner:(unit -> Prism<'b,'c>) -> Prism<'a,'c>
    static member
      ( => ) : outer:(unit -> Lens<'a,'b>) * inner:Prism<'b,'c> -> Prism<'a,'c>
    static member
      ( => ) : outer:Lens<'a,'b> * inner:Prism<'b,'c> -> Prism<'a,'c>
    static member
      ( => ) : outer:Lens<'a,'b> * inner:(unit -> Lens<'b,'c>) -> Lens<'a,'c>
    static member
      ( => ) : outer:(unit -> Lens<'a,'b>) * inner:Lens<'b,'c> -> Lens<'a,'c>
    static member ( => ) : outer:Lens<'a,'b> * inner:Lens<'b,'c> -> Lens<'a,'c>
    static member
      ( ?=> ) : outer:Lens<'a,'b option> * inner:Lens<'b,'c> -> Prism<'a,'c>
    static member
      create : get:('state -> 'value) ->
                 set:('value -> 'state -> 'state) -> Lens<'state,'value>
    member d : ('value OpticInput -> OpticOutput<'state>)
  end
and Prism<'state,'value> =
  | Prism of ('value OpticInput -> OpticOutput<'state>)
  with
    static member
      ( => ) : outer:Prism<'a,'b> * inner:(unit -> Lens<'b,'c>) -> Prism<'a,'c>
    static member
      ( => ) : outer:(unit -> Prism<'a,'b>) * inner:Lens<'b,'c> -> Prism<'a,'c>
    static member
      ( => ) : outer:Prism<'state,'value> * inner:Lens<'value,'innerValue> ->
                 Prism<'state,'innerValue>
    static member
      ( => ) : outer:Prism<'a,'b> * inner:(unit -> Prism<'b,'c>) -> Prism<'a,'c>
    static member
      ( => ) : outer:(unit -> Prism<'a,'b>) * inner:Prism<'b,'c> -> Prism<'a,'c>
    static member
      ( => ) : outer:Prism<'a,'b> * inner:Prism<'b,'c> -> Prism<'a,'c>
    static member
      create : get:('state -> 'value option) ->
                 set:('value -> 'state -> 'state) -> Prism<'state,'value>
    member d : ('value OpticInput -> OpticOutput<'state>)
  end
[<AbstractClassAttribute (); SealedAttribute ()>]
type Operations =
  class
    static member
      over : prism:(unit -> Prism<'state,'value>) ->
               (('value -> 'value) -> 'state -> 'state)
    static member
      over : lens:(unit -> Lens<'state,'value>) ->
               (('value -> 'value) -> 'state -> 'state)
    static member
      over : Prism<'state,'value> -> (('value -> 'value) -> 'state -> 'state)
    static member
      over : Lens<'state,'value> -> (('value -> 'value) -> 'state -> 'state)
    static member
      read : prism:(unit -> Prism<'state,'value>) -> ('state -> 'value option)
    static member
      read : lens:(unit -> Lens<'state,'value>) -> ('state -> 'value)
    static member read : Prism<'state,'value> -> ('state -> 'value option)
    static member read : Lens<'state,'value> -> ('state -> 'value)
    static member
      write : prism:(unit -> Prism<'state,'value>) ->
                ('value -> 'state -> 'state)
    static member
      write : lens:(unit -> Lens<'state,'value>) -> ('value -> 'state -> 'state)
    static member
      write : prism:Prism<'state,'value> -> ('value -> 'state -> 'state)
    static member
      write : lens:Lens<'state,'value> -> ('value -> 'state -> 'state)
    static member
      writeSome : lens:(unit -> Lens<'state,'value option>) ->
                    ('value -> 'state -> 'state)
    static member
      writeSome : prism:Prism<'state,'value option> ->
                    ('value -> 'state -> 'state)
    static member
      writeSome : lens:Lens<'state,'value option> ->
                    ('value -> 'state -> 'state)
  end
val inline lens :
  get:('state -> 'value) ->
    set:('value -> 'state -> 'state) -> Lens<'state,'value>
val inline prism :
  get:('state -> 'value option) ->
    set:('value -> 'state -> 'state) -> Prism<'state,'value>
module Tuple2 = begin
  val fst_ : unit -> Lens<('a * 'b),'a>
  val snd_ : unit -> Lens<('a * 'b),'b>
end
module Option = begin
  val some_ : unit -> Prism<'a option,'a>
  /// some__ is for if you know you're guaranteed to have some_ because you've already checked it
  val some__ : unit -> Lens<'a option,'a>
end
module List = begin
  val nth_ : n:int -> Prism<'a list,'a>
  /// some__ is for if you know you're guaranteed to have nth_ because you've already checked it
  val nth__ : n:int -> Lens<'a list,'a>
end

