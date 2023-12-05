module UI.DFRPG.Chargen
open Feliz
open Fable.React
type Stuff = Foo | Bar
type Weapon = Sword | Bow
type Trait = WeaponMaster of Weapon | CombatReflexes | Skill of string * bonus:int

type Multimap<'key, 'value when 'key:comparison and 'value: comparison> = Map<'key, Set<'value>>
type OfferKey = System.Guid
let newKey() = System.Guid.NewGuid()
type SideEffects = unit
// payload will probably be a character in a given ruleset
type 'payload PendingChange = PendingChange of OfferKey * ('payload -> 'payload)

type 'payload OfferOutput = {
    stableState: 'payload // TODO: used for cost calculations
    queuedChanges: 'payload PendingChange // TODO: used for cost calculations
    mutable pickedOffers: OfferKey Set
    mutable dataAugment: Map<OfferKey, int> // some traits have levels
    mutable children: Multimap<OfferKey, OfferKey>
    mutable parents: Map<OfferKey, OfferKey>
    mutable uiBuilder: Map<OfferKey, ReactElement list -> ReactElement>
    }
    with static member root = System.Guid.NewGuid() // not persisted but that's okay

// for scope-like properties as opposed to preorder output, e.g. whether we're within a "grant all these things" block
type OfferScope = {
    autogrant: bool
    remainingBudget: int option
    }
type 'payload Offer = OfferScope -> 'payload OfferOutput -> SideEffects

type Skill = { // stub, doesn't even have attribute
    name: string
    difficulty: int
    }

type DFRPGCharacter = { // stub
    traits: Trait Set
    }

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

let offerLogic =
    fun (key:OfferKey) innerLogic (output: 'payload OfferOutput) ->
        // we could be in a state of notPicked, refining, or picked. When to show what? Depends on parent state, but do we expect parent to have already filtered us out?
        // Three possibilities:
        // 1. Parent not selected. In this case we won't even get here, don't need to worry about it.
        // 2. Parent selected but we're not selected. In this case we need to enable selection, unless it would put us over budget, and then we just show it as unselectable.
        // 3. Parent selected and we're also selected. In this case we need to enable deselection, unless it's "free", and then we just show it as perma-selected and un-deselectable.
        let ui selected (txt: string) =
            let id = $"chk_{key}"
            let onChange v = if v then output.pickedOffers <- output.pickedOffers |> Set.add key else output.pickedOffers <- output.pickedOffers |> Set.remove key
            output.uiBuilder <- output.uiBuilder |> Map.add key (function
                | [] -> checkbox txt id selected onChange
                | children when txt = "" -> Html.div [prop.children children]
                | children -> Html.div [prop.children (checkbox txt id selected onChange::children)])
        if output.pickedOffers.Contains(key) then
            innerLogic true (ui true)
        else innerLogic false (ui false)

let skill(name, bonus): DFRPGCharacter Offer =
    let key = newKey()
    fun scope output ->
        let innerLogic selected ui =
            ui $"{name} {bonus}"
        output |> offerLogic key innerLogic
let either(choices: DFRPGCharacter Offer list): DFRPGCharacter Offer =
    let key = newKey()
    fun scope output ->
        let innerLogic selected ui =
            for choice in choices do
                choice scope output
            ui "" // todo: maybe refactor this to ui.either
        output |> offerLogic key innerLogic
let budgeted(budget, offers: DFRPGCharacter Offer list): DFRPGCharacter Offer =
    let key = newKey()
    fun scope output ->
        let innerLogic selected ui =
            for offer in offers do
                offer scope output
            output.uiBuilder <- output.uiBuilder |> Map.add key (function
                | children -> Html.div [prop.children (Html.div "Choose [{budget}] from:"::children)])
        output |> offerLogic key innerLogic

let swash() = [

    budgeted(20, [
        skill("Acrobatics", 2)
        skill("Acrobatics", [1..3])
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
