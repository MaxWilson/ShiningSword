/// A module for dynamically building interactive wizards that minimize the number of unnecessary questions they ask.
/// Key concepts: 
///   Setting<T>:will eventually yield a T when user finishes answering all the questions.
///   Render<output>: typically Render<ReactElement>, used to format output for user to look at/interact with to update wizardState.
///   WizardState: the current choices that have been made by the user, in the form of Choice hashcodes -> index mapping.
module AutoWizard
type 't LifecycleStage =
  | Unset
  | Set
  | Complete of 't
  with
    member map : f:('t -> 'a) -> 'a LifecycleStage
  end
type Setting<'t> =
  interface
    abstract member
      Match : IPatternMatch<'t> -> 't LifecycleStage * 'output list
  end
and IPatternMatch<'t> =
  interface
    abstract member
      App1 : Setting<('s -> 't)> ->
               Setting<'s> -> 't LifecycleStage * 'output list
    abstract member
      App2 : Setting<('s1 * 's2 -> 't)> ->
               Setting<'s1> -> Setting<'s2> -> 't LifecycleStage * 'output list
    abstract member
      App3 : Setting<('s1 * 's2 * 's3 -> 't)> ->
               Setting<'s1> ->
                 Setting<'s2> ->
                   Setting<'s3> -> 't LifecycleStage * 'output list
    abstract member
      Choice : Setting<'t> list -> 't LifecycleStage * 'output list
    abstract member
      ChoiceDistinctN : Setting<'input> list ->
                          int -> 't LifecycleStage * 'output list
    abstract member Const : 't -> 't LifecycleStage * 'output list
  end
type Render<'appState,'output> =
  interface
    abstract member
      RenderChoice : state:unit LifecycleStage ->
                       options:'t1 list ->
                         lens:Optics.Lens<'appState,ChoiceState option> ->
                           'output list
    abstract member
      RenderChoiceDistinctN : state:unit LifecycleStage ->
                                options:'t1 list ->
                                  n:int ->
                                    lens:Optics.Lens<'appState,
                                                     ChoiceState option> ->
                                      'output list
  end
and ChoiceKey =
  | Choice of hash: int
  | Multichoice of hash: int * n: int
and ChoiceState =
  | ChoiceIndex of int
  | MultichoiceIndex of int list
val compose :
  render:('t LifecycleStage -> 'a) ->
    children:'a list -> input:'t LifecycleStage -> 't LifecycleStage * 'a list
type SettingConst<'t> =
  class
    interface Setting<'t>
    new : v:'t * ?label:string -> SettingConst<'t>
    override ToString : unit -> string
  end
type SettingChoice<'t> =
  class
    interface Setting<'t>
    new : values:Setting<'t> list -> SettingChoice<'t>
    override ToString : unit -> string
  end
type SettingChoiceDistinctN<'t> =
  class
    interface Setting<'t list>
    new : values:Setting<'t> list * n:int -> SettingChoiceDistinctN<'t>
    override ToString : unit -> string
  end
type SettingCtor<'t,'s> =
  class
    interface Setting<'t>
    new : label:string * ctor:Setting<('s -> 't)> * arg:Setting<'s> ->
            SettingCtor<'t,'s>
    override ToString : unit -> string
  end
type SettingCtor2<'t,'s1,'s2> =
  class
    interface Setting<'t>
    new : label:string * ctor:Setting<('s1 * 's2 -> 't)> * arg1:Setting<'s1> *
          arg2:Setting<'s2> -> SettingCtor2<'t,'s1,'s2>
    override ToString : unit -> string
  end
type SettingCtor3<'t,'s1,'s2,'s3> =
  class
    interface Setting<'t>
    new : label:string * ctor:Setting<('s1 * 's2 * 's3 -> 't)> *
          arg1:Setting<'s1> * arg2:Setting<'s2> * arg3:Setting<'s3> ->
            SettingCtor3<'t,'s1,'s2,'s3>
    override ToString : unit -> string
  end
val c : v:'a -> Setting<'a>
val alias : name:string -> v:'a -> Setting<'a>
val choose : options:Setting<'a> list -> Setting<'a>
val chooseDistinct : n:int -> options:Setting<'a> list -> Setting<'a list>
val ctor : label:string * f:Setting<('a -> 'b)> * arg:Setting<'a> -> Setting<'b>
val ctor2 :
  label:string * f:Setting<('a * 'b -> 'c)> * arg1:Setting<'a> *
  arg2:Setting<'b> -> Setting<'c>
val ctor3 :
  label:string * f:Setting<('a * 'b * 'c -> 'd)> * arg1:Setting<'a> *
  arg2:Setting<'b> * arg3:Setting<'c> -> Setting<'d>
val both : arg1:Setting<'a> * arg2:Setting<'b> -> Setting<'a * 'b>
val pmatch :
  pattern:IPatternMatch<'t> -> x:Setting<'t> -> 't LifecycleStage * 'a list
val pattern<'t, 'appState, 'out> :
  getLens:(ChoiceKey -> Optics.Lens<'appState,ChoiceState option>) ->
    render:Render<'appState,'out> -> state:'appState -> IPatternMatch<'t>
val eval :
  setting:Setting<'t> *
  getLens:(ChoiceKey -> Optics.Lens<'appState,ChoiceState option>) *
  render:Render<'appState,'output> * state:'appState ->
    't LifecycleStage * 'output list

