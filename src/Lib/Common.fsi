[<AutoOpen>]
module Common
val flip : f:('a -> 'b -> 'c) -> x:'b -> y:'a -> 'c
val random : System.Random
val rand : x:int -> int
val thunk : v:'a -> 'b -> 'a
val thunk1 : f:('a -> 'b) -> arg:'a -> 'c -> 'b
val thunk2 : f:('a -> 'b -> 'c) -> arg1:'a -> arg2:'b -> 'd -> 'c
val thunk3 :
  f:('a -> 'b -> 'c -> 'd) -> arg1:'a -> arg2:'b -> arg3:'c -> 'e -> 'd
val ignore1 : f:(unit -> 'a) -> 'b -> 'a
val matchfail : v:'a -> 'b
/// Placeholder while we're doing type-focused development, before implementation
val notImpl : unit -> 'a
val shouldntHappen : 'a -> 'b
val emptyString : string
val betweenInclusive : a:'a -> b:'a -> n:'a -> bool when 'a : comparison
/// invoke f without requiring parens
val inv : f:(unit -> 'a) -> 'a
val chooseRandom : options:seq<'a> -> 'a
val iter : data:byref<'a> -> f:('a -> 'a) -> 'a
/// iter and ignore the result
val iteri : data:byref<'a> -> f:('a -> 'a) -> unit
val shuffleCopy : ('a [] -> 'a [])
module String = begin
  val oxfordJoin : _arg1:string list -> string
  val join : delimiter:string -> strings:seq<string> -> string
  val equalsIgnoreCase : lhs:string -> rhs:string -> bool
  val firstWord : input:string -> string
  val trim : s:string -> string
end
module List = begin
  val join : delimiter:'a -> lst:'a list -> 'a list
  val ofOption : _arg1:'a option -> 'a list
  val every : f:('a -> bool) -> ('a list -> bool)
  val tryMapFold :
    f:('a -> 'b -> Result<'a,'c>) -> state:'a -> lst:'b list -> Result<'a,'c>
end
module Map = begin
  val keys : m:Map<'a,'b> -> seq<'a> when 'a : comparison
  val values : m:Map<'a,'b> -> seq<'b> when 'a : comparison
  val addForce :
    key:'a ->
      f:(Map<'b,'c> -> Map<'b,'c>) -> m:Map<'a,Map<'b,'c>> -> Map<'a,Map<'b,'c>>
      when 'a : comparison and 'b : comparison and 'c : equality
  val findForce :
    key:'a -> m:Map<'a,Map<'b,'c>> -> Map<'b,'c>
      when 'a : comparison and 'b : comparison
end
type IdGenerator =
  | NextId of int
  with
    static member
      newId : idGenerator_:Optics.Lens<'m,IdGenerator> -> model:'m -> int * 'm
    static member fresh : IdGenerator
  end
module Queue = begin
  type 't d = 't list
  val append : item:'a -> queue:'a list -> 'a list
  val empty : 'a list
  val read : queue:'a d -> 'a d
end
type Ops =
  class
    static member add : item:'a * data:'a Queue.d -> 'a list
    static member
      add : key:'a * value:'b * data:Map<'a,'b> -> Map<'a,'b>
              when 'a : comparison
    static member addTo : data:'a Queue.d -> ('a -> 'a list)
    static member
      addTo : data:Map<'a,'b> -> ('a -> 'b -> Map<'a,'b>) when 'a : comparison
  end

