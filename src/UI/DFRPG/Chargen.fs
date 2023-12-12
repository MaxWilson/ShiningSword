module UI.DFRPG.Chargen
open Feliz
open Fable.React
type Stuff = Foo | Bar
type Weapon = Sword | Bow
type Trait = WeaponMaster of Weapon | CombatReflexes | Skill of string * bonus:int

type Multimap<'key, 'value when 'key:comparison and 'value: comparison> = Map<'key, Set<'value>>
type OrderedMultimap<'key, 'value when 'key:comparison and 'value: comparison> = Map<'key, List<'value>>
type OfferKey = string // should include a guid
let newKey (prefix:string) = $"{prefix}-{System.Guid.NewGuid()}"
type SideEffects = unit
// payload will probably be a character in a given ruleset
type 'payload PendingChange = PendingChange of OfferKey * ('payload -> 'payload)

let offerRoot = newKey "root" // not persisted but that's okay

type 'payload OfferOutput = {
    stableState: 'payload // TODO: used for cost calculations
    queuedChanges: 'payload PendingChange list // TODO: used for cost calculations
    notifyChanged: 'payload OfferOutput -> unit
    mutable pickedOffers: OfferKey Set
    mutable dataAugment: Map<OfferKey, int> // some traits have levels
    mutable children: OrderedMultimap<OfferKey, OfferKey>
    mutable parents: Map<OfferKey, OfferKey>
    mutable uiBuilder: Map<OfferKey, ReactElement list -> ReactElement>
    mutable choices: Set<OfferKey>
    }
    with
    static member fresh payload pending = { stableState = payload; queuedChanges = pending; notifyChanged = ignore; pickedOffers = Set.empty; dataAugment = Map.empty; children = Map.empty; parents = Map.empty; uiBuilder = Map.empty; choices = Set.empty }
    member this.notify() = this.notifyChanged this
    member this.toReactElements() =
        // do a post-order traversal of the offer tree, gathering up the UI elements wherever they exist
        let root = offerRoot
        let rec recur (key:OfferKey) =
            match this.children |> Map.tryFind key with
            | Some (children: OfferKey list) ->
                if not (this.uiBuilder.ContainsKey key) then
                    // if we're not picked, we don't need to render anything
                    shouldntHappen $"if there's no uiBuilder for {key} then there's no visuals and there shouldn't be any children either"
                let combine = this.uiBuilder[key] // if there's no uiBuilder then there's no visuals and there shouldn't be any children either
                let uis = children |> List.map recur
                let ui = combine uis
                ui
            | None ->
                match this.uiBuilder |> Map.tryFind key with
                | Some builder -> builder []
                | None -> Html.div $"no uiBuilder for {key}"
        recur root

// for scope-like properties as opposed to preorder output, e.g. whether we're within a "grant all these things" block
type OfferScope = {
    autogrant: bool
    remainingBudget: int option
    parent: OfferKey
    }
type 'payload Offer = OfferScope * 'payload OfferOutput -> SideEffects

type Skill = { // stub, doesn't even have attribute
    name: string
    difficulty: int
    }

type DFRPGCharacter = { // stub
    traits: Trait Set
    }
    with static member fresh = { traits = Set.empty }

let checkbox (txt: string) (id: string) selected onChange = class' "control" Html.div [
    Html.input [
        prop.type' "checkbox"
        prop.isChecked selected
        prop.onCheckedChange onChange
        prop.id id
        ]
    Html.label [
        prop.htmlFor id
        prop.text txt
        ]
    ]

type API = {
    offering: string -> unit // description -> () with a checkbox
    unconditional: string -> unit // description -> () but no checkbox, only text e.g. "choose 20 from"
    }

let recur key (scope, output) offer =
    offer ({ scope with parent = key }, output)

let run (offers: _ Offer list) (state: DFRPGCharacter OfferOutput) notify : _ OfferOutput =
    let root = offerRoot
    let output = { OfferOutput<_>.fresh state.stableState state.queuedChanges with pickedOffers = state.pickedOffers; notifyChanged = notify }
    let scope = { autogrant = false; remainingBudget = None; parent = root }
    for offer in offers do
        recur root (scope, output) offer
    output.uiBuilder <- output.uiBuilder |> Map.add root (function
        | [child] -> child
        | children -> Html.div [prop.children children]
        )
    output

type style = Feliz.style
type OfferConfiguration = {
    label: string option
    key: string option // let multiple options share the same if they should share state, e.g. Sword! Broadsword-20 and Sword-and-Shield! Broadsword-19 + Shield-15 might want both broadswords to keep the same state when toggling between Sword! vs. Sword-and-Sheild!
    }
let blank = { label = None; key = None }

type Op() =
    static let offerLogic =
        fun (key:OfferKey) (scope: OfferScope, output: 'payload OfferOutput) innerLogic ->
            let selected = output.pickedOffers.Contains key

            // we could be in a state of notPicked, refining, or picked. When to show what? Depends on parent state, but do we expect parent to have already filtered us out?
            // Three possibilities:
            // 1. Parent not selected. In this case we won't even get here, don't need to worry about it.
            // 2. Parent selected but we're not selected. In this case we need to enable selection, unless it would put us over budget, and then we just show it as unselectable.
            // 3. Parent selected and we're also selected. In this case we need to enable deselection, unless it's "free", and then we just show it as perma-selected and un-deselectable.

            // It is the child's responsibility to set up parent/child relationships
            let child = key in (
                output.parents <- output.parents |> Map.add child scope.parent
                output.children <- output.children |> Map.change scope.parent (Option.orElse (Some []) >> Option.map (flip List.append [child]))
                )

            let uiCheckbox selected (txt: string) =
                let id = $"chk_{key}"
                let onChange v =
                    if v then output.pickedOffers <- output.pickedOffers |> Set.add key else output.pickedOffers <- output.pickedOffers |> Set.remove key
                    output.notify()
                output.choices <- output.choices |> Set.add key
                output.uiBuilder <- output.uiBuilder |> Map.add key (function
                    | [] -> checkbox txt id selected onChange
                    | children when txt = "" -> Html.div [prop.children children]
                    | children -> Html.div [prop.children [checkbox txt id selected onChange; Html.ul children]]
                    )
            let uiDiv (txt: string) =
                output.uiBuilder <- output.uiBuilder |> Map.add key (function
                    | [] -> Html.div txt
                    | [child] when txt = "" -> child
                    | children when txt = "" -> Html.div children
                    | children -> React.fragment [Html.div txt; Html.ul children]
                    )

            let api: API = {
                offering = uiCheckbox selected
                unconditional = uiDiv
                }
            innerLogic selected api

    static member label (txt:string) = { blank with label = Some txt }
    static member skill(config: OfferConfiguration, name:string, bonus: int): DFRPGCharacter Offer =
        let key = defaultArg config.key <| newKey $"{name} %+d{bonus}"
        fun ((scope, output) as args) ->
            offerLogic key args <| fun selected (ui:API) ->
                ui.offering (defaultArg config.label $"{name} %+d{bonus}")
    static member skill(name: string, bonus: int) = Op.skill(blank, name, bonus)

    static member skill(config: OfferConfiguration, name:string, bonusRange: int list): DFRPGCharacter Offer =
        let key = defaultArg config.key <| newKey $"{name} {bonusRange}"
        fun ((scope, output) as args) ->
            offerLogic key args <| fun selected (ui:API) ->
                ui.offering (defaultArg config.label $"{name} %+d{bonusRange[0]} to %+d{bonusRange |> List.last}")
    static member skill(name, bonusRange: int list) = Op.skill(blank, name, bonusRange)

    static member either(config: OfferConfiguration, choices: (DFRPGCharacter Offer) list): DFRPGCharacter Offer =
        let key = defaultArg config.key <| newKey $"one-of-{choices.Length}"
        fun ((scope, output) as args) ->
            offerLogic key args <| fun selected (ui:API) ->
                // recur if selected or no need for selection
                if selected || scope.autogrant || config.label.IsNone then
                    choices |> List.iter (recur key args)
                match config.label with
                | Some label ->
                    ui.offering (label + if selected then $"Choose one of {choices.Length}:" else "")
                | None ->
                    ui.unconditional $"Choose one of {choices.Length}:"
    static member either(choices: (DFRPGCharacter Offer) list) = Op.either(blank, choices)

    static member  and'(config: OfferConfiguration, choices: DFRPGCharacter Offer list): DFRPGCharacter Offer =
        let key = defaultArg config.key <| newKey $"one-of-{choices.Length}"
        fun ((scope, output) as args) ->
            offerLogic key args <| fun selected (ui:API) ->
                // recur if selected or no need for selection
                if selected || scope.autogrant || config.label.IsNone then
                    choices |> List.iter (recur key args)
                match config.label with
                | Some label ->
                    ui.offering label
                | None ->
                    ui.unconditional $"Choose one of {choices.Length}:"
    static member  and'(choices: DFRPGCharacter Offer list) = Op.and'(blank, choices)

    static member budgeted(config: OfferConfiguration, budget, offers: DFRPGCharacter Offer list): DFRPGCharacter Offer =
        let key = defaultArg config.key <| newKey $"budget-{budget}"
        fun ((scope, output) as args) ->
            offerLogic key args <| fun selected (ui:API) ->
                // selected doesn't matter in this case: there's no checkbox, only a div or ul
                offers |> List.iter (recur key args)
                ui.unconditional (defaultArg config.label $"Choose [{budget}] from:")
    static member budgeted(budget, offers: DFRPGCharacter Offer list) = Op.budgeted(blank, budget, offers)
open type Op

let swash = [
    skill("Climbing", 1)
    skill("Stealth", [1..3])
    budgeted(20, [
        skill("Acrobatics", 2)
        skill("Acrobatics", [1..3])
        let mainWeapons = ["Rapier"; "Broadsword"; "Polearm"; "Two-handed sword"] |> List.map (fun name -> name, newKey name)
        let weaponsAt (bonus: int) = mainWeapons |> List.map (fun (name, key) -> skill({ blank with key = Some key }, name, bonus))
        either [
            either(label "Sword!", weaponsAt +5)
            and'(label "Sword and Dagger", [either(weaponsAt +4); skill("Main-gauche", +1)])
            and'(label "Sword and Shield", [either(weaponsAt +4); skill("Shield", +2)])
            ]
        either [
            skill("Fast-draw (Sword)", +2)
            and'(label "both", [skill("Fast-draw (Sword)", +1); skill("Fast-draw (Dagger)", +1)])
            ]
        ])
    ]

type Msg = RefreshedOutput of DFRPGCharacter OfferOutput
type Model = {
    currentOutput: DFRPGCharacter OfferOutput option
    }

let init _ = { currentOutput = None }
let update msg model =
    match msg with
    | RefreshedOutput output -> { model with currentOutput = Some output }
