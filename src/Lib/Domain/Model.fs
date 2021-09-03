namespace Domain
module Model =
    #if INTERACTIVE
    module Generator =
        type LensAttribute() =
            inherit System.Attribute()
        type DuCasesAttribute() =
            inherit System.Attribute()
    #else
    open Myriadic
    open Myriad.Plugins
    #endif
    open AutoWizard

    module Character =
        type Stat = Str | Dex | Con | Int | Wis | Cha

        [<Generator.Lens>]
        type Stats = {
            str: int
            dex: int
            con: int
            int: int
            wis: int
            cha: int
            }
        [<Generator.DuCases "fields">]
        type Sex = Male | Female | Neither
        [<Generator.DuCases "fields">]
        type Feat = Sharpshooter | CrossbowExpert | HeavyArmorMaster | GreatWeaponMaster
        [<Generator.DuCases "fields">]
        type Skill = Athletics | Stealth | Perception | Insight
        [<Generator.DuCases "fields">]
        type ElfRace = High | Wood | Drow
        [<Generator.DuCases "fields">]
        type DwarfRace = Mountain | Hill
        [<Generator.DuCases "fields">]
        type HumanType = Standard | Variant of Skill * Feat * (Stat * Stat)
        [<Generator.DuCases "fields">]
        type Race = Human of HumanType | Elf of ElfRace | Dwarf of DwarfRace | Halforc | Goblin

        [<Generator.DuCases "fields">]
        type Class = Barbarian | Fighter | Monk | Rogue
        [<Generator.DuCases "fields">]
        type FightingStyle = Dueling | Archery | Defense | GreatWeaponFighting
        [<Generator.DuCases "fields">]
        type Subclass =
            | Champion
            | EldritchKnight
            | Samurai
            | Zealot
            | Swashbuckler
            | FourElements

        type ASIChoice = ASI of Stat * Stat | Feat of Feat

        [<Generator.DuCases "dus">]
        type ClassAbility =
            | ASIChoice of ASIChoice
            | FightingStyle of FightingStyle
            | ExtraAttack of int
            | SecondWind of int
            | Indomitable of int
            | Subclass of Subclass

        [<Generator.Lens>]
        type CharacterSheet = {
            stats: Stats
            unmodifiedStats: Stats
            name: string
            sex: Sex
            race: Race
            xp: int
            allocatedLevels: Class list // advancement priorities, e.g. [Fighter; Fighter; Fighter; Fighter; Rogue; Fighter; Rogue]
            subclasses: Map<Class, Subclass>
            classAbilities: ClassAbility list
            }


    open Character
    open Common

    [<Generator.Lens>]
    type StatBlock = {
        stats: Stats
        hp: int
        ac: int
        }

    [<Generator.Lens>]
    type CharSheet = {
        statBlock: StatBlock
        xp: int
        yearOfBirth: int
        sex: Sex
        }

    type StatSource = StatBlock of StatBlock | CharSheet of CharSheet

    [<Generator.Lens>]
    type Creature = {
        name: string
        stats: StatSource
        }

    module Draft =
        [<Generator.Lens>]
        type DraftSheet = {
            unmodifiedStats: Stats
            explicitName: string option
            autoName: string
            sex: Setting<Sex>
            race: Setting<Race>
            xp: int
            allocatedLevels: Class list // advancement priorities, e.g. [Fighter; Fighter; Fighter; Fighter; Rogue; Fighter; Rogue]
            subclasses: Map<Class, Subclass>
            classAbilities: Setting<ClassAbility> list
            }
            with
            member this.name = defaultArg this.explicitName this.autoName
        [<Generator.DuCases "fields">]
        type Trait =
            | Race of Race
            | Class of Class * Subclass option * int
            | Feat of Feat
            | ASI of Stat * int
            | Skill of Skill

    module Ribbit =
        
        type AgentId = int
        type EventId = int
        type RuntimeValue = String of string | Number of int | Boolean of bool | Id of int | Undefined
        type Name = string
        type PropertyName = string
        type VariableReference =
            | DataRef of agent: AgentId * property: PropertyName
            | LocalRef of name: Name
            | EventRef of event:EventId
            | IndirectDataRef of agentIdLocalRef: Name * property: PropertyName
            | IndirectEventRef of eventIdLocalRef: Name
        
        type Expression =
            | Const of RuntimeValue
            | BinaryOp of Expression * Expression * BinaryOperator
            | Dereference of VariableReference
            | Roll of n:int * dSize: int * plus: int
            | StartEvent of eventName: Name * args: (Name * Expression) list
        
        and BinaryOperator =
            | Plus
            | Minus
            | Times
            | Divide
            | Equals
            | AtLeast
            | AtMost
        
        type Statement =
            | Return of Expression
            | Assign of VariableReference * Expression
            | Sequence of Statement list
            | If of test: Expression * andThen: Statement * orElse: Statement option
        type CurrentExpressionValue = Result<RuntimeValue, VariableReference list>
        
        type ExecutionContext =
            {
            workQueue: EventId list // LIFO queue for ease of implementation but it probably doesn't matter what the order is
            currentEvent: EventId option
            }
            with
            static member fresh = { workQueue = []; currentEvent = None }
            static member forEvent eventId = { ExecutionContext.fresh with currentEvent = Some eventId }
        
        type DereferenceF<'state> = ExecutionContext -> VariableReference -> 'state -> CurrentExpressionValue
        type DeferF<'state> = EventId -> Statement list -> VariableReference list -> 'state -> 'state
        
        type CompleteF<'state> = EventId -> RuntimeValue -> 'state -> 'state
        type SupplyF<'state> = ExecutionContext -> VariableReference -> RuntimeValue -> 'state -> ('state * EventId list)
        type ResumeF<'state> = EventId -> 'state -> Statement list
        type RunF<'state> = EventId -> 'state -> 'state
        type StartByNameF<'state> = Name -> (Name * RuntimeValue) list -> 'state -> EventId * 'state

        type Api<'state> = {
                dereference: DereferenceF<'state>
                defer: DeferF<'state>
                resume: ResumeF<'state>
                supply: SupplyF<'state>
                start: StartByNameF<'state>
            }
        type RibbitRuntimeException (msg: string) =
            inherit System.Exception(msg)

        type Scope = {
            properties: Map<Name, RuntimeValue>
            }
        type Event = EventResult of RuntimeValue | EventState of EventState
        and EventState = { scope: Scope; instructionStack: Statement list }
        and EventDefinition = { name: Name; mandatoryParams: PropertyName list; instructions: Statement list }
        
        type Game = {
            roster: Map<string, AgentId list>
            rosterReverse: Map<AgentId, string>
            data: Map<AgentId, Scope>
            eventDefinitions: Map<Name, EventDefinition>
            events: Map<EventId, Event>
            nextAgentId: AgentId // an id generator
            nextEventId: EventId // an id generator
            waitingEvents: Map<VariableReference, Set<EventId>> // to support execution chaining
            dataDependencies: (AgentId*PropertyName) list // for display in UI. Local variables and event results can't be input by the user so don't need to go in this list.
            }

