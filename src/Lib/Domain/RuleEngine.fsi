namespace Domain
  module RuleEngine = begin
    module Logic = begin
      val continueWith :
        f:('a -> 'b -> 'b * Model.Ribbit.HOASLogicOutput<'b,'c,'d>) ->
          logic:Model.Ribbit.HOASLogic<'b,'c,'a> ->
            ('b -> 'b * Model.Ribbit.HOASLogicOutput<'b,'c,'d>)
      val tryRead :
        id:Model.Ribbit.RowKey ->
          prop:Model.Ribbit.Prop<obj,'t> ->
            state:Model.Ribbit.State -> 't option
      val defineAffordance : name:'a -> props:'b -> logic:'c -> state:'d -> 'd
      val demand :
        Model.Ribbit.RowKey * Model.Ribbit.PropertyName ->
          logic:Model.Ribbit.Logic<unit> ->
            state:Model.Ribbit.State -> Model.Ribbit.State
      val addToQueue :
        logic:Model.Ribbit.Logic<unit> ->
          state:Model.Ribbit.State -> Model.Ribbit.State
      val fulfill :
        id:Model.Ribbit.RowKey * prop:Model.Ribbit.Prop<obj,'t> ->
          value:'t -> state:Model.Ribbit.State -> Model.Ribbit.State
      val andLog :
        id:Model.Ribbit.RowKey ->
          logic:Model.Ribbit.HOASLogic<Model.Ribbit.State,'a,string> ->
            (Model.Ribbit.State ->
               Model.Ribbit.State *
               Model.Ribbit.HOASLogicOutput<Model.Ribbit.State,'a,unit>)
      val processLogic :
        Model.Ribbit.State *
        Model.Ribbit.HOASLogicOutput<Model.Ribbit.State,
                                     (Model.Ribbit.RowKey *
                                      Model.Ribbit.PropertyName) option,unit> ->
          Model.Ribbit.State
      val spawn :
        logic:Model.Ribbit.Logic<string> ->
          state:Model.Ribbit.State -> Model.Ribbit.State
      val triggerAffordance : name:'a -> propValues:'b -> state:'c -> 'c
      val untilFixedPoint : state:Model.Ribbit.State -> Model.Ribbit.State
      module Builder = begin
        type Builder<'state,'demand> =
          class
            new : unit -> Builder<'state,'demand>
            member
              Bind : logic:Model.Ribbit.HOASLogic<'state,'demand,'t> *
                     rhs:('t -> Model.Ribbit.HOASLogic<'state,'demand,'r>) ->
                       Model.Ribbit.HOASLogic<'state,'demand,'r>
            member
              Return : x:'a ->
                         ('b -> 'b * Model.Ribbit.HOASLogicOutput<'c,'d,'a>)
            member ReturnFrom : logic:'a -> 'a
            member
              Run : x:Model.Ribbit.HOASLogic<'a,'b,'c> ->
                      Model.Ribbit.Logic<'a,'b,'c>
          end
        val read :
          id:Model.Ribbit.RowKey ->
            prop:Model.Ribbit.Prop<obj,'a> ->
              state:Model.Ribbit.State ->
                Model.Ribbit.State *
                Model.Ribbit.HOASLogicOutput<Model.Ribbit.State,
                                             (Model.Ribbit.RowKey *
                                              Model.Ribbit.PropertyName) option,
                                             'a>
        val logic : Builder<Model.Ribbit.State,Model.Ribbit.Demand>
      end
    end
  end

