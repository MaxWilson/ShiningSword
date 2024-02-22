# DFRPG Chargen Design Doc

## Requirements

Supports all DFRPG Adventurers templates
Skills, advantages, disadvantages, quirks
Random attribute generation option
Doesn't double-charge for things that show up in multiple sections, like disadvantages or quirk buys

```mermaid
stateDiagram-v2
    state Chargen {
      [*] --> Constraints
      Constraints --> RollAttributes
      RollAttributes --> ChooseProfession
      RollAttributes --> ChooseRace
      ChooseRace --> RollAttributes
      ChooseProfession --> CompleteTemplate
      ChooseProfession --> AddQuirks
      CompleteTemplate --> BuyEquipment
      BuyEquipment --> Save
      AddQuirks --> Save
      Constraints: Set race/nation/roll method constraints (optional)
      RollAttributes: Roll Attributes (optional)
      ChooseRace: Choose Race (optional)
      ChooseProfession: Choose Profession (radio buttons, collapsible, remember preference, allows random)
      CompleteTemplate: Complete template (250 point budget)
      AddQuirks: Add quirks (optional, raises budget)
      BuyEquipment: Buy equipment
      Save: Save character
      Save --> [*]
      }
    state Gameplay {
      [*] --> KillMonsters
      Explore --> KillMonsters
      Explore --> Rewards
      Explore --> Traps
      Explore --> NPCs
      NPCs --> Explore
      Traps --> Rewards
      KillMonsters --> Rewards
      Rewards --> Explore
      Explore --> [*]
      Rewards: Get treasure, gain XP
      KillMonsters: Kill monsters (or parlay, etc.)
      }
    state "Advancement" as Advancement {
      Save2: Save character2
      [*] --> AddToTemplate
      AddToTemplate --> BuyMoreEquipment
      BuyMoreEquipment --> SaveAgain
      SaveAgain --> [*]
      AddToTemplate: Add more traits (above initial 250 point limit)
      SaveAgain: Save changes
      BuyMoreEquipment: Spend cash, buy more stuff
      }
    SaveOnExit: Save before exiting?
    state if_SaveGame <<choice>>
    [*] --> Chargen
    Chargen --> Gameplay
    Gameplay --> SaveOnExit
    SaveOnExit --> if_SaveGame
    if_SaveGame --> [*] : Quit without saving
    if_SaveGame --> SaveAndQuit : Save
    SaveAndQuit --> [*]
    Gameplay --> Advancement
    Advancement --> Gameplay
```
