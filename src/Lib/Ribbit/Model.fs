namespace Domain
module Model =
    #if INTERACTIVE
    open System
    [<RequireQualifiedAccess>]
    module Generator =
        /// Instructs to generate lenses for each property of the record
        type LensAttribute(wrapperName : string) =
            inherit System.Attribute()
            let mutable _wrapperName = wrapperName
            member this.WrapperName = _wrapperName
            new () = LensAttribute(null : string)
            new (``type``: Type) = LensAttribute(``type``.Name)
        type DuCasesAttribute(wrapperName : string) =
            inherit System.Attribute()
            let mutable _wrapperName = wrapperName
            member this.WrapperName = _wrapperName
            new () = DuCasesAttribute(null : string)
            new (``type``: Type) = DuCasesAttribute(``type``.Name)
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

    module Ribbit0 =

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

        type Command =
        | Define of eventName: Name * instructions: Statement list
        | Execute of Statement list
        | Supply of ref:VariableReference * value:RuntimeValue
        | AddToRoster of Name

    type Metacommand =
        | Ribbit of Ribbit0.Command

    module Ribbit =
        open System.Collections.Generic
        type EventId = EventId of int
        type CreatureId = CreatureId of int
        type PropertyName = PropertyName of string
        type Roll =
            | StaticBonus of n:int
            | Roll of n:int * d:int * rest: Roll option
            with
            static member create(n,d) = Roll(n,d,None)
            static member create(n,d,plus) = Roll(n,d,Some(StaticBonus plus))
            member this.eval() =
                match this with
                | StaticBonus n -> n
                | Roll(n,d,rest) ->
                    let v = [for _ in 1..n -> rand d] |> List.sum
                    match rest with
                    | None -> v
                    | Some r -> v + r.eval()
        type RuntimeValue =
            | Text of string
            | Number of int
            | Boolean of bool
            | Id of int
            | Random of Roll
            | Resource of int // n times per XYZ. Related to number but more specialized, has additional operations like consume and convert.
            | Undefined
        type Scope<'t> = Dictionary<'t, RuntimeValue>

        type Address =
            | EventProperty of EventId * PropertyName
            | CreatureProperty of CreatureId * PropertyName

        type InnerState =
            {
                mutable events: ResizeArray<Scope<string> option>
                mutable creatureData: ResizeArray<Scope<string> option>
                mutable properties: Dictionary<string, Property>
                }
            with static member fresh = { events = ResizeArray<_>(); creatureData = ResizeArray<_>(); properties = Dictionary<_,_>() }

        and ResolveAddressToValue = Address -> InnerState -> (RuntimeValue * Address list) LookupResult

        // you give it a way to read from state, and get back a list of addresses read and either a result or a list of addresses you're still awaiting
        and DeriveOrCreateValue = (ResolveAddressToValue -> (RuntimeValue * Address list) LookupResult)

        and 't LookupResult =
            | Yield of 't
            | Await of Address list
        and FallbackBehavior =
            | AskUser
            | Generate of DeriveOrCreateValue
            | Derive of DeriveOrCreateValue
        and Property = {
            name: PropertyName
            runtimeTypeCheck: RuntimeValue -> bool
            fallbackBehavior: FallbackBehavior
            }

        module Prop =
            let isNumber = function Number _ -> true | _ -> false
            let isText = function Text _ -> true | _ -> false
            let isBool = function Boolean _ -> true | _ -> false
            let isId = function Id _ -> true | _ -> false
            let isRandom = function Random _ -> true | _ -> false
            let isResource = function Resource _ -> true | _ -> false
            let prop checker name =
                {
                    name = PropertyName name
                    runtimeTypeCheck = checker
                    fallbackBehavior = AskUser
                    }
            let propWithDefault checker name defaultValue =
                {
                    name = PropertyName name
                    runtimeTypeCheck = checker
                    fallbackBehavior = Generate (fun _ -> Yield(defaultValue, []))
                    }
        open Prop
        let numberProp = prop isNumber
        let textProp = prop isText
        let resourceProp = prop isResource
        let prototypeProp = prop isId "Prototype"
        type Msg =
            | Set of Address * RuntimeValue
        type State = Stateful.State<InnerState, Msg>
        let update msg (state: InnerState) =
            let reserve (collection: ResizeArray<_>) (ix: int) =
                if ix >= collection.Count then
                    collection.AddRange(Seq.init (1 + ix - collection.Count) (thunk None))
            match msg with
            | Set(EventProperty(EventId evid, PropertyName pid), v) ->
                reserve state.events evid
                let scope =
                    match state.events[evid] with
                    | None ->
                        let scope = Scope<string>()
                        state.events[evid] <- Some scope
                        scope
                    | Some scope -> scope
                scope[pid] <- v
            | Set(CreatureProperty(CreatureId cid, PropertyName pid), v) ->
                reserve state.events cid
                let scope =
                    match state.creatureData[cid] with
                    | None ->
                        let scope = Scope<string>()
                        state.creatureData[cid] <- Some scope
                        scope
                    | Some scope -> scope
                scope[pid] <- v
            state

        let read address (innerState:InnerState) =
            let deref = function
                | EventProperty(EventId evid, PropertyName pid) ->
                    if evid < innerState.events.Count then
                        match innerState.events[evid] with
                        | None -> None
                        | Some scope ->
                            match scope.TryGetValue pid with
                            | true, v -> Some v
                            | false, _ -> None
                    else
                        None
                | CreatureProperty(CreatureId cid, PropertyName pid) ->
                    if cid < innerState.creatureData.Count then
                        match innerState.creatureData[cid] with
                        | None -> None
                        | Some scope ->
                            match scope.TryGetValue pid with
                            | true, v -> Some v
                            | false, _ -> None
                    else
                        None
            match deref address with
            | Some v -> Some v
            | None ->
                let rec searchInParent childAddress =
                    let parent =
                        match childAddress with
                        | EventProperty(evid, _) -> deref (EventProperty(evid, prototypeProp.name))
                        | CreatureProperty(cid, _) -> deref (CreatureProperty(cid, prototypeProp.name))
                    match parent with
                    | None -> None
                    | Some (Id parentId) ->
                        let parentProp =
                            match childAddress with
                            | EventProperty(_, prop) -> EventProperty(EventId parentId, prop)
                            | CreatureProperty(_, prop) -> CreatureProperty(CreatureId parentId, prop)
                        match deref parentProp with
                        | Some v -> Some v
                        | None -> searchInParent parentProp
                    | Some _ -> shouldntHappen()
                searchInParent address
        let readOrCreateAndRead address (innerState:InnerState) =
            // this version of read will do something if read fails: ask the user, generate new values, etc.
            notImpl()

        let hpP = { numberProp "HP" with fallbackBehavior = Generate(notImpl) }
        let x = InnerState.fresh |> update (Set(EventProperty(EventId 1, hpP.name), Number 42))
        x.events[1].Value.["HP"]
