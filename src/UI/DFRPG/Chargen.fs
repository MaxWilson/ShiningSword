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
    mutable pickedOffers: OfferKey Set
    mutable dataAugment: Map<OfferKey, int> // some traits have levels
    mutable children: OrderedMultimap<OfferKey, OfferKey>
    mutable parents: Map<OfferKey, OfferKey>
    mutable uiBuilder: Map<OfferKey, ReactElement list -> ReactElement>
    }
    with
    static member fresh payload pending = { stableState = payload; queuedChanges = pending; pickedOffers = Set.empty; dataAugment = Map.empty; children = Map.empty; parents = Map.empty; uiBuilder = Map.empty }
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

let checkbox (txt: string) (id: string) selected onChange = Html.div [
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
    label: string -> unit // description -> () but no checkbox, only text e.g. "choose 20 from"
    }

let offerLogic =
    fun (key:OfferKey) innerLogic (scope: OfferScope, output: 'payload OfferOutput) ->
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
            let onChange v = if v then output.pickedOffers <- output.pickedOffers |> Set.add key else output.pickedOffers <- output.pickedOffers |> Set.remove key
            output.uiBuilder <- output.uiBuilder |> Map.add key (function
                | [] -> checkbox txt id selected onChange
                | children when txt = "" -> Html.div [prop.children children]
                | children -> Html.div [prop.children (checkbox txt id selected onChange::children)]
                )
        let uiLabel (txt: string) =
            output.uiBuilder <- output.uiBuilder |> Map.add key (function
                | children -> Html.div [prop.children ((Html.text txt)::children)]
                )

        let api: API = {
            offering = uiCheckbox selected
            label = uiLabel
            }
        innerLogic selected api

let recur key (scope, output) offer =
    offer ({ scope with parent = key }, output)

let run (offers: _ Offer list) state pending : _ OfferOutput =
    let root = offerRoot
    let output = OfferOutput<_>.fresh state pending
    let scope = { autogrant = false; remainingBudget = None; parent = root }
    for offer in offers do
        recur root (scope, output) offer
    output.uiBuilder <- output.uiBuilder |> Map.add root (function
        | [child] -> child
        | children -> Html.div [prop.children children]
        )
    output

let skill(name, bonus): DFRPGCharacter Offer =
    let key = newKey $"{name} %+d{bonus}"
    fun ((scope, output) as args) ->
        let innerLogic selected (ui:API) =
            ui.offering $"{name} {bonus}"
        offerLogic key innerLogic args
let skillRange(name, bonusRange: int list): DFRPGCharacter Offer =
    let key = newKey $"{name} {bonusRange}"
    fun ((scope, output) as args) ->
        let innerLogic selected (ui:API) =
            ui.offering $"{name} {bonusRange[0]}"
        offerLogic key innerLogic args
let either(choices: DFRPGCharacter Offer list): DFRPGCharacter Offer =
    let key = newKey $"one-of-{choices.Length}"
    fun ((scope, output) as args) ->
        let innerLogic selected (ui:API) =
            if selected then
                choices |> List.iter (recur key args)
            ui.label "Choose one of:"
        offerLogic key innerLogic args
type style = Feliz.style
let budgeted(budget, offers: DFRPGCharacter Offer list): DFRPGCharacter Offer =
    let key = newKey $"budget-{budget}"
    fun ((scope, output) as args) ->
        let innerLogic selected (ui:API) =
            if selected then
                offers |> List.iter (recur key args)
            if not selected then
                output.pickedOffers <- output.pickedOffers |> Set.add key
            output.uiBuilder <- output.uiBuilder |> Map.add key (function
                | children -> Html.div [prop.children (Html.div $"Choose [{budget}] from:"::children)])
        offerLogic key innerLogic args

let swash() = [
    skill("climbing", 1)
    skillRange("stealth", [1..3])
    budgeted(20, [
        skill("Acrobatics", 2)
        skillRange("Acrobatics", [1..3])
        either([
            skill("Rapier", 20)
            skill("Broadsword", 20)
            ])
        ])
    ]

type Model = {
    template: DFRPGCharacter Offer list
    }

let init _ = { template = swash() }
let update msg1 model = model
