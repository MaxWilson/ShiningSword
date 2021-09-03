namespace Domain
  module Model = begin
    module Character = begin
      type Stat =
        | Str
        | Dex
        | Con
        | Int
        | Wis
        | Cha
      [<Myriadic.Generator.LensAttribute ()>]
      type Stats =
        { str: int
          dex: int
          con: int
          int: int
          wis: int
          cha: int }
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Sex =
        | Male
        | Female
        | Neither
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Feat =
        | Sharpshooter
        | CrossbowExpert
        | HeavyArmorMaster
        | GreatWeaponMaster
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Skill =
        | Athletics
        | Stealth
        | Perception
        | Insight
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type ElfRace =
        | High
        | Wood
        | Drow
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type DwarfRace =
        | Mountain
        | Hill
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type HumanType =
        | Standard
        | Variant of Skill * Feat * (Stat * Stat)
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Race =
        | Human of HumanType
        | Elf of ElfRace
        | Dwarf of DwarfRace
        | Halforc
        | Goblin
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Class =
        | Barbarian
        | Fighter
        | Monk
        | Rogue
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type FightingStyle =
        | Dueling
        | Archery
        | Defense
        | GreatWeaponFighting
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Subclass =
        | Champion
        | EldritchKnight
        | Samurai
        | Zealot
        | Swashbuckler
        | FourElements
      type ASIChoice =
        | ASI of Stat * Stat
        | Feat of Feat
      [<Myriad.Plugins.Generator.DuCasesAttribute ("dus")>]
      type ClassAbility =
        | ASIChoice of ASIChoice
        | FightingStyle of FightingStyle
        | ExtraAttack of int
        | SecondWind of int
        | Indomitable of int
        | Subclass of Subclass
      [<Myriadic.Generator.LensAttribute ()>]
      type CharacterSheet =
        { stats: Stats
          unmodifiedStats: Stats
          name: string
          sex: Sex
          race: Race
          xp: int
          allocatedLevels: Class list
          subclasses: Map<Class,Subclass>
          classAbilities: ClassAbility list }
    end
    [<Myriadic.Generator.LensAttribute ()>]
    type StatBlock =
      { stats: Character.Stats
        hp: int
        ac: int }
    [<Myriadic.Generator.LensAttribute ()>]
    type CharSheet =
      { statBlock: StatBlock
        xp: int
        yearOfBirth: int
        sex: Character.Sex }
    type StatSource =
      | StatBlock of StatBlock
      | CharSheet of CharSheet
    [<Myriadic.Generator.LensAttribute ()>]
    type Creature =
      { name: string
        stats: StatSource }
    module Draft = begin
      [<Myriadic.Generator.LensAttribute ()>]
      type DraftSheet =
        { unmodifiedStats: Character.Stats
          explicitName: string option
          autoName: string
          sex: AutoWizard.Setting<Character.Sex>
          race: AutoWizard.Setting<Character.Race>
          xp: int
          allocatedLevels: Character.Class list
          subclasses: Map<Character.Class,Character.Subclass>
          classAbilities: AutoWizard.Setting<Character.ClassAbility> list }
        with
          member name : string
        end
      [<Myriad.Plugins.Generator.DuCasesAttribute ("fields")>]
      type Trait =
        | Race of Character.Race
        | Class of Character.Class * Character.Subclass option * int
        | Feat of Character.Feat
        | ASI of Character.Stat * int
        | Skill of Character.Skill
    end
    module Ribbit = begin
      type RowKey = int
      type PropertyName = string
      type DataKey = RowKey * PropertyName
      type Logic<'state,'demand,'result> =
        | HOAS of HOASLogic<'state,'demand,'result>
        | FOAS
      and HOASLogic<'state,'demand,'result> =
        'state -> 'state * HOASLogicOutput<'state,'demand,'result>
      and HOASLogicOutput<'state,'demand,'result> =
        | Ready of 'result
        | Awaiting of
          demand: 'demand * followup: HOASLogic<'state,'demand,'result>
      type TypeImplementationBase<'sharedType> =
        interface
          /// Returns true if value satisfies type
          abstract member isSatisfied : 'sharedType -> bool
          abstract member dataType : System.Type
        end
      type TypeImplementation<'sharedType,'t> =
        class
          interface TypeImplementationBase<'sharedType>
          new : impl:{| extract: ('sharedType -> 't option);
                        parse: (string -> 't option) |} ->
                  TypeImplementation<'sharedType,'t>
          member extract : v:'sharedType -> 't option
          member parse : v:string -> 't option
        end
      type Property =
        class
          new : name:PropertyName * dataType:System.Type -> Property
          member dataType : System.Type
          member name : PropertyName
        end
      type ParameterDefinition =
        { name: string
          dataType: TypeImplementationBase<obj>
          defaultValue: obj option
          property: Property }
      [<Myriadic.Generator.LensAttribute ()>]
      type State =
        { ids: Common.IdGenerator
          properties: Map<string,Property>
          eventDefinitions: Map<string,EventDefinition>
          data: Map<DataKey,obj>
          settled: Map<RowKey,string>
          outstandingQueries: Map<DataKey,Logic<unit> list>
          workQueue: Logic<unit> Common.Queue.d
          log: RowKey Common.Queue.d }
        with
          static member fresh : State
        end
      and Demand = DataKey option
      and Logic<'t> = Logic<State,Demand,'t>
      and EventDefinition =
        { isAffordance: bool
          parameters: ParameterDefinition
          body: Logic<string> }
      type Prop<'sharedType,'t> =
        class
          inherit Property
          new : name:PropertyName * type':System.Type *
                impl:TypeImplementation<'sharedType,'t> -> Prop<'sharedType,'t>
          member extract : data:'sharedType -> 't option
          member
            extract : rowId:RowKey *
                      data:Map<(RowKey * PropertyName),'sharedType> -> 't option
          member
            isFulfilled : rowId:RowKey *
                          data:Map<(RowKey * PropertyName),'sharedType> -> bool
          member parse : stringInput:string -> 't option
        end
      val intProp : name:PropertyName -> Prop<obj,int>
      val stringProp : name:PropertyName -> Prop<obj,string>
      val rowKeyProp : name:PropertyName -> Prop<obj,RowKey>
    end
  end

