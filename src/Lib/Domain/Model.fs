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
        type RowKey = int
        type PropertyName = string
        type DataKey = RowKey * PropertyName
        type Logic<'state, 'demand, 'result> =
            | HOAS of HOASLogic<'state, 'demand, 'result>
            | FOAS
        and HOASLogic<'state, 'demand, 'result> = ('state -> 'state * HOASLogicOutput<'state, 'demand, 'result>)
        and HOASLogicOutput<'state, 'demand, 'result> = Ready of 'result | Awaiting of demand:'demand * followup:HOASLogic<'state, 'demand, 'result>

        type TypeImplementationBase<'sharedType> =
            /// Returns true if value satisfies type
            abstract isSatisfied: 'sharedType -> bool
            abstract dataType: System.Type

        type TypeImplementation<'sharedType, 't>
            (impl:
                {| extract: 'sharedType -> 't option;
                parse: string -> 't option |}) =
            member _.extract v = impl.extract v
            member _.parse v = impl.parse v
            interface TypeImplementationBase<'sharedType> with
                member _.isSatisfied v = impl.extract v |> Option.isSome
                member _.dataType = typeof<'t>

        type Property(name, dataType) =
            member _.dataType = dataType
            member _.name = name

        type ParameterDefinition = {        
            name: string
            dataType: TypeImplementationBase<obj>
            defaultValue: obj option
            property: Property // Where to actually store the data. Is this really the right way to pass in parameters?
            }

        [<Generator.Lens>]
        type State = {
            ids: IdGenerator
            properties: Map<string, Property>
            eventDefinitions: Map<string, EventDefinition>
            data: Map<DataKey, obj>
            settled: Map<RowKey, string>
            outstandingQueries: Map<DataKey, Logic<unit> list>
            workQueue: Logic<unit> Queue.d
            log: RowKey Queue.d
            }
            with
            static member fresh = {
                ids = IdGenerator.fresh
                properties = Map.empty
                eventDefinitions = Map.empty
                outstandingQueries = Map.empty
                data = Map.empty
                settled = Map.empty
                workQueue = Queue.empty
                log = Queue.empty }
        and Demand = DataKey option
        and Logic<'t> = Logic<State, Demand, 't>
        and EventDefinition = {
            isAffordance: bool
            parameters: ParameterDefinition
            body: Logic<string>
            }

        type Prop<'sharedType, 't>(name: PropertyName, type', impl: TypeImplementation<'sharedType, 't>) =
            inherit Property(name, type')
            member _.extract data = impl.extract data
            member _.parse stringInput = impl.parse stringInput
            member _.extract (rowId: RowKey, data) =
                match data |> Map.tryFind (rowId, name) with
                | Some(data) -> impl.extract data
                | None -> None
            member this.isFulfilled(rowId: RowKey, data) =
                this.extract(rowId, data).IsSome
        let intProp name =
            let impl =
                TypeImplementation<obj, int>(
                    {|  extract = function :?int as x -> Some x | _ -> None
                        parse = fun str -> match System.Int32.TryParse str with true, v -> Some v | _ -> None
                        |})
            Prop<obj, int>(name, typeof<int>, impl)
        let stringProp name =
            let impl =
                TypeImplementation<obj, string>(
                    {|  extract = function :?string as x -> Some x | _ -> None
                        parse = Some
                        |})
            Prop<obj, string>(name, typeof<int>, impl)
        let rowKeyProp name =
            let impl =
                TypeImplementation<obj, RowKey>(
                    {|  extract = function :?int as x -> Some x | _ -> None
                        parse = fun str -> match System.Int32.TryParse str with true, v -> Some v | _ -> None
                        |})
            Prop<obj, RowKey>(name, typeof<RowKey>, impl)

