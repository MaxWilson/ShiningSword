namespace AutoGen
  module Sex = begin
    val toString : x:Domain.Model.Character.Sex -> string
    val fromString : x:string -> Domain.Model.Character.Sex option
    val toTag : x:Domain.Model.Character.Sex -> int
    val isMale : x:Domain.Model.Character.Sex -> bool
    val isFemale : x:Domain.Model.Character.Sex -> bool
    val isNeither : x:Domain.Model.Character.Sex -> bool
  end
namespace AutoGen
  module Feat = begin
    val toString : x:Domain.Model.Character.Feat -> string
    val fromString : x:string -> Domain.Model.Character.Feat option
    val toTag : x:Domain.Model.Character.Feat -> int
    val isSharpshooter : x:Domain.Model.Character.Feat -> bool
    val isCrossbowExpert : x:Domain.Model.Character.Feat -> bool
    val isHeavyArmorMaster : x:Domain.Model.Character.Feat -> bool
    val isGreatWeaponMaster : x:Domain.Model.Character.Feat -> bool
  end
namespace AutoGen
  module Skill = begin
    val toString : x:Domain.Model.Character.Skill -> string
    val fromString : x:string -> Domain.Model.Character.Skill option
    val toTag : x:Domain.Model.Character.Skill -> int
    val isAthletics : x:Domain.Model.Character.Skill -> bool
    val isStealth : x:Domain.Model.Character.Skill -> bool
    val isPerception : x:Domain.Model.Character.Skill -> bool
    val isInsight : x:Domain.Model.Character.Skill -> bool
  end
namespace AutoGen
  module ElfRace = begin
    val toString : x:Domain.Model.Character.ElfRace -> string
    val fromString : x:string -> Domain.Model.Character.ElfRace option
    val toTag : x:Domain.Model.Character.ElfRace -> int
    val isHigh : x:Domain.Model.Character.ElfRace -> bool
    val isWood : x:Domain.Model.Character.ElfRace -> bool
    val isDrow : x:Domain.Model.Character.ElfRace -> bool
  end
namespace AutoGen
  module DwarfRace = begin
    val toString : x:Domain.Model.Character.DwarfRace -> string
    val fromString : x:string -> Domain.Model.Character.DwarfRace option
    val toTag : x:Domain.Model.Character.DwarfRace -> int
    val isMountain : x:Domain.Model.Character.DwarfRace -> bool
    val isHill : x:Domain.Model.Character.DwarfRace -> bool
  end
namespace AutoGen
  module HumanType = begin
    val toString : x:Domain.Model.Character.HumanType -> string
    val fromString : x:string -> Domain.Model.Character.HumanType option
    val toTag : x:Domain.Model.Character.HumanType -> int
    val isStandard : x:Domain.Model.Character.HumanType -> bool
    val isVariant : x:Domain.Model.Character.HumanType -> bool
  end
namespace AutoGen
  module Race = begin
    val toString : x:Domain.Model.Character.Race -> string
    val fromString : x:string -> Domain.Model.Character.Race option
    val toTag : x:Domain.Model.Character.Race -> int
    val isHuman : x:Domain.Model.Character.Race -> bool
    val isElf : x:Domain.Model.Character.Race -> bool
    val isDwarf : x:Domain.Model.Character.Race -> bool
    val isHalforc : x:Domain.Model.Character.Race -> bool
    val isGoblin : x:Domain.Model.Character.Race -> bool
  end
namespace AutoGen
  module Class = begin
    val toString : x:Domain.Model.Character.Class -> string
    val fromString : x:string -> Domain.Model.Character.Class option
    val toTag : x:Domain.Model.Character.Class -> int
    val isBarbarian : x:Domain.Model.Character.Class -> bool
    val isFighter : x:Domain.Model.Character.Class -> bool
    val isMonk : x:Domain.Model.Character.Class -> bool
    val isRogue : x:Domain.Model.Character.Class -> bool
  end
namespace AutoGen
  module FightingStyle = begin
    val toString : x:Domain.Model.Character.FightingStyle -> string
    val fromString : x:string -> Domain.Model.Character.FightingStyle option
    val toTag : x:Domain.Model.Character.FightingStyle -> int
    val isDueling : x:Domain.Model.Character.FightingStyle -> bool
    val isArchery : x:Domain.Model.Character.FightingStyle -> bool
    val isDefense : x:Domain.Model.Character.FightingStyle -> bool
    val isGreatWeaponFighting : x:Domain.Model.Character.FightingStyle -> bool
  end
namespace AutoGen
  module Subclass = begin
    val toString : x:Domain.Model.Character.Subclass -> string
    val fromString : x:string -> Domain.Model.Character.Subclass option
    val toTag : x:Domain.Model.Character.Subclass -> int
    val isChampion : x:Domain.Model.Character.Subclass -> bool
    val isEldritchKnight : x:Domain.Model.Character.Subclass -> bool
    val isSamurai : x:Domain.Model.Character.Subclass -> bool
    val isZealot : x:Domain.Model.Character.Subclass -> bool
    val isSwashbuckler : x:Domain.Model.Character.Subclass -> bool
    val isFourElements : x:Domain.Model.Character.Subclass -> bool
  end
namespace AutoGen
  module ClassAbility = begin
    val toString : x:Domain.Model.Character.ClassAbility -> string
    val fromString : x:string -> 'a option
    val toTag : x:Domain.Model.Character.ClassAbility -> int
    val isASIChoice : x:Domain.Model.Character.ClassAbility -> bool
    val isFightingStyle : x:Domain.Model.Character.ClassAbility -> bool
    val isExtraAttack : x:Domain.Model.Character.ClassAbility -> bool
    val isSecondWind : x:Domain.Model.Character.ClassAbility -> bool
    val isIndomitable : x:Domain.Model.Character.ClassAbility -> bool
    val isSubclass : x:Domain.Model.Character.ClassAbility -> bool
  end
namespace AutoGen
  module Trait = begin
    val toString : x:Domain.Model.Draft.Trait -> string
    val fromString : x:string -> 'a option
    val toTag : x:Domain.Model.Draft.Trait -> int
    val isRace : x:Domain.Model.Draft.Trait -> bool
    val isClass : x:Domain.Model.Draft.Trait -> bool
    val isFeat : x:Domain.Model.Draft.Trait -> bool
    val isASI : x:Domain.Model.Draft.Trait -> bool
    val isSkill : x:Domain.Model.Draft.Trait -> bool
  end
namespace AutoGen
  module Stats = begin
    val str_ : Optics.Lens<Domain.Model.Character.Stats,int>
    val dex_ : Optics.Lens<Domain.Model.Character.Stats,int>
    val con_ : Optics.Lens<Domain.Model.Character.Stats,int>
    val int_ : Optics.Lens<Domain.Model.Character.Stats,int>
    val wis_ : Optics.Lens<Domain.Model.Character.Stats,int>
    val cha_ : Optics.Lens<Domain.Model.Character.Stats,int>
  end
  module CharacterSheet = begin
    val stats_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Domain.Model.Character.Stats>
    val unmodifiedStats_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Domain.Model.Character.Stats>
    val name_ : Optics.Lens<Domain.Model.Character.CharacterSheet,string>
    val sex_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Domain.Model.Character.Sex>
    val race_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Domain.Model.Character.Race>
    val xp_ : Optics.Lens<Domain.Model.Character.CharacterSheet,int>
    val allocatedLevels_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Domain.Model.Character.Class list>
    val subclasses_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Map<Domain.Model.Character.Class,
                      Domain.Model.Character.Subclass>>
    val classAbilities_ :
      Optics.Lens<Domain.Model.Character.CharacterSheet,
                  Domain.Model.Character.ClassAbility list>
  end
  module StatBlock = begin
    val stats_ :
      Optics.Lens<Domain.Model.StatBlock,Domain.Model.Character.Stats>
    val hp_ : Optics.Lens<Domain.Model.StatBlock,int>
    val ac_ : Optics.Lens<Domain.Model.StatBlock,int>
  end
  module CharSheet = begin
    val statBlock_ : Optics.Lens<Domain.Model.CharSheet,Domain.Model.StatBlock>
    val xp_ : Optics.Lens<Domain.Model.CharSheet,int>
    val yearOfBirth_ : Optics.Lens<Domain.Model.CharSheet,int>
    val sex_ : Optics.Lens<Domain.Model.CharSheet,Domain.Model.Character.Sex>
  end
  module Creature = begin
    val name_ : Optics.Lens<Domain.Model.Creature,string>
    val stats_ : Optics.Lens<Domain.Model.Creature,Domain.Model.StatSource>
  end
  module DraftSheet = begin
    val unmodifiedStats_ :
      Optics.Lens<Domain.Model.Draft.DraftSheet,Domain.Model.Character.Stats>
    val explicitName_ : Optics.Lens<Domain.Model.Draft.DraftSheet,string option>
    val autoName_ : Optics.Lens<Domain.Model.Draft.DraftSheet,string>
    val sex_ :
      Optics.Lens<Domain.Model.Draft.DraftSheet,
                  AutoWizard.Setting<Domain.Model.Character.Sex>>
    val race_ :
      Optics.Lens<Domain.Model.Draft.DraftSheet,
                  AutoWizard.Setting<Domain.Model.Character.Race>>
    val xp_ : Optics.Lens<Domain.Model.Draft.DraftSheet,int>
    val allocatedLevels_ :
      Optics.Lens<Domain.Model.Draft.DraftSheet,
                  Domain.Model.Character.Class list>
    val subclasses_ :
      Optics.Lens<Domain.Model.Draft.DraftSheet,
                  Map<Domain.Model.Character.Class,
                      Domain.Model.Character.Subclass>>
    val classAbilities_ :
      Optics.Lens<Domain.Model.Draft.DraftSheet,
                  AutoWizard.Setting<Domain.Model.Character.ClassAbility> list>
  end
  module State = begin
    val ids_ : Optics.Lens<Domain.Model.Ribbit.State,Common.IdGenerator>
    val properties_ :
      Optics.Lens<Domain.Model.Ribbit.State,
                  Map<string,Domain.Model.Ribbit.Property>>
    val eventDefinitions_ :
      Optics.Lens<Domain.Model.Ribbit.State,
                  Map<string,Domain.Model.Ribbit.EventDefinition>>
    val data_ :
      Optics.Lens<Domain.Model.Ribbit.State,Map<Domain.Model.Ribbit.DataKey,obj>>
    val settled_ :
      Optics.Lens<Domain.Model.Ribbit.State,
                  Map<Domain.Model.Ribbit.RowKey,string>>
    val outstandingQueries_ :
      Optics.Lens<Domain.Model.Ribbit.State,
                  Map<Domain.Model.Ribbit.DataKey,
                      Domain.Model.Ribbit.Logic<unit> list>>
    val workQueue_ :
      Optics.Lens<Domain.Model.Ribbit.State,
                  Domain.Model.Ribbit.Logic<unit> Common.Queue.d>
    val log_ :
      Optics.Lens<Domain.Model.Ribbit.State,
                  Domain.Model.Ribbit.RowKey Common.Queue.d>
  end

