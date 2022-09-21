
module DFData

let dfSpells = """
Affect Spirits Necro. C: PI3 59
Agonize Body W: M2, Sensitize 20
Air Jet Air W: Shape Air 15
Alertness (VH) Mind W*: 2 Keen Sense
spells
53
Ambidexterity Body W: Grace 20
Analyze Magic Know. W*: Identify Spell 42
Animal Control Animal D: PI2 17
Apportation Move. W: M1 56
Arboreal Immurement
Plant D: PI5 61
Armor P&W C: PI1 • W: Shield 63
Astral Block Necro. C: PI4 • W: Repel
Spirits, Summon
Spirit 59
Astral Vision (VH) Know./
Necro.
C: PI3 • W*: Sense
Spirit, See
Invisible
42
Aura Know. C: PI1 • W*: Detect
Magic
42
Awaken Healing C: PI2 36
Balance Body W: Grace 20
Banish Necro. C: PI4 • W: M1,
1 spell from
10 colleges
59
Beast Link Animal D: PI2 18
Beast Possession Animal D: PI4 18
Beast-Rouser Animal D: PI1 18
Beast Seeker Animal D: PI2 18
Beast-Soother Animal D: PI1 18
Beast Speech Animal D: PI2 18
Beast Summoning Animal D: PI3 18
Blackout L&D W: Darkness 46
Bladeturning P&W W: Shield 63
Bless Meta C: PI5 50
Blink Gate/Move. W: M3, IQ 13+,
1 spell from
10 colleges
56
Blink Other (VH) Gate/Move. W: Blink 56
Blur L&D W: Darkness 46
Borrow Language C&E W*: Lend Language 24
Borrow Skill C&E W*: Lend Skill 24
Bravery Mind C: PI1 • W*: Fear 53
Breathe Water Air/Water C: PI3 • D: PI3
• W: Create Air,
Destroy Water
68
Bright Vision L&D W: Keen Vision or
5 L&D spells
46
Burning Touch Fire W: M2, 6 Fire spells
including Heat
29
Charm Mind W*: M1/BT1,
Loyalty, 7 other
Mind Control
spells
53
Cleansing Healing C: PI1 36
Climbing Body W: M0 20
Clumsiness Body W: Spasm 20
Cold Fire W: Heat 29
Colors L&D W: Light 46
Command Mind C: PI2 • W*: M2/
BT2, Forgetfulness
53
Command Spirit Necro. C: PI3 • W: Summon
Spirit, Turn Spirit
60
Compel Truth C&E C: PI2 • W*: M2/
BT2, Truthsayer
24
Complex Illusion Illusion W: Simple Illusion,
Sound
40
Conceal Plant D: PI3 61
Concussion Air/Sound W*: Shape Air,
Thunderclap
15
Continual Light L&D C: PI2 • W: Light 46
Control Gate Gate W: M3, Seek Gate 34
Control Illusion Illusion W: Perfect Illusion 40
Control Person C&E W*: Soul Rider or
Telepathy
24
Cook Food W: Test Food, Create
Fire
32
Coolness P&W/
Water
C: PI1 • D: PI1
• W: Cold
68
Copy M&B W: 5 M&B spells
including
Restore, no
Illiteracy
48
Counterspell Meta W: M1 51
Create Air Air W: Purify Air 16
Create Animal Animal D: PI4 19
Create Earth Earth W: Earth to Stone 27
Create Fire Fire W: Ignite Fire or
Seek Fire
29
Create Food Food C: PI3 • W: Cook,
Seek Food
32
Create Plant Plant D: PI3 61
Create Water Water C: PI2 • W: Purify
Water
68
Cure Disease Healing C: PI3 • D: PI2 36
Curse Meta C: PI5 51
Dark Vision L&D W: Infravision or
Night Vision
46
Darkness L&D W: Continual Light 46
Daze Mind W*: Foolishness 54
Death Vision Necro. W: M1 60
Deathtouch Body W: Wither Limb 20
Debility Body W: M0 20
Decay Food W: Test Food 32
Deflect Energy Fire W: M1, Shape Fire 29
Deflect Missile Move./P&W W: Apportation 56
Dehydrate Water W: 5 Water spells
including Destroy
Water
68
Delayed Message Sound W*: M1/BT1, Sense
Life, Voices
66
Destroy Air Air W: Create Air 16
Destroy Water Water W: Create Water 68
Detect Magic Know. C: PI1 • D: PI1
• W*: M1/BT1
43
Detect Poison Healing/
P&W
C: PI1 • D: PI1 36
Dispel Illusion Illusion W: Control Illusion 40
Dispel Magic Meta C: PI4 • D: PI4
• W: Counterspell, any 12
other spells
51
Dispel Possession C&E C: PI3 24
Divert Teleport
(VH)
Gate/Move. W: M3, Trace
Teleport
34
Drunkenness Mind W*: Foolishness,
Clumsiness
54
Dull Sense Mind W*: M0/BT1 54
Dullness (VH) Mind W*: 2 Dull Sense
spells
54
Earth to Air Air/Earth W: Create Air, Shape
Earth
16
Earth to Stone Earth W: M1, Shape Earth 27
Earth Vision Earth/
Know.
D: PI3 • W*: Shape
Earth
27
Earthquake Earth C: PI5 • D: PI6 27
Entombment Earth D: PI5 • W: M2,
5 Earth spells
27
Entrap Spirit Necro. C: PI5 • W: M1,
7 Necromantic
spells including
Turn Spirit
60
Essential Food
(VH)
Food C: PI4 • W: 6 Food
spells including
Create Food
32
Ethereal Body
(VH)
Move. W: M3, 6 Movement
spells
57
Explosive Fireball Fire W: Fireball 29
Explosive Lightning Air/Weather
W: Lightning 71
Extinguish Fire Fire D: PI1 • W: Ignite
Fire
30
Far-Feeling Know. W*: M1/BT1 43
Far-Hearing Know./
Sound
W*: M1/BT1,
4 Sound spells
66
Far-Tasting Food/
Know.
W*: M1, Seek Food 33
Fascinate Mind W*: Daze 54
Fasten M&B W: Knot 48
Fear Mind W*: Sense Emotion 54
Final Rest Healing/
Necro.
C: PI1 36
Find Direction Know. D: PI1
• W*: M1/BT1
43
Find Weakness M&B W: 1 Air, 1 Earth,
1 Fire, 1 Water
spell
49
Fire Cloud Fire W: Fireball, Shape
Air
30
Fireball Fire W: M1, Create Fire,
Shape Fire
30
Fireproof Fire D: PI2 • W: Extinguish Fire
30
Flame Jet Fire W: Create Fire,
Shape Fire
30
Flaming Missiles Fire W: Flaming Weapon 30
Flaming Weapon Fire C: PI3 • W: M2, Heat 31
Flesh to Stone Earth W: Earth to Stone 27
Flight (VH) Move. W: M2, Levitation 57
Fog Water/
Weather
D: PI2 71
Foolishness Mind W*: M0/BT1, IQ 12+ 54
Forest Warning Plant D: PI3 61
Forgetfulness Mind W*: M1/BT1,
Foolishness
54
Frailty Body W: Lend Energy 21
Freeze Water D: PI3 • W: Shape
Water
68
Frostbite Water D: PI4 • W: Cold,
Freeze
69
Garble Sound W*: Voices 66
Geyser (VH) Water D: PI6 69
Gift of Letters
(VH)
C&E C: PI4 • W*: Borrow
Language, 3
written languages
24
Gift of Tongues
(VH)
C&E C: PI4 • W*: Borrow
Language, 3
spoken languages
24
Glass Wall Know. W*: 5 Knowledge
spells or Earth
Vision
43
Glow L&D C: PI2
• W: Continual
Light
46
Glue Move. W: Haste 57
Grace Body W: Clumsiness 21
Grease Move. W: Haste 57
Great Haste (VH) Move. W: M1, IQ 12+,
Haste
57
Great Healing
(VH)
Healing C: PI3 37
Great Voice Sound C: PI2
• W*: Thunderclap, Voices
66
Great Ward Meta W: M2, Ward 51
Hail Water/
Weather
D: PI4 71
Haste Move. W: M0 57
Hawk Vision L&D D: PI1 • W: Keen
Vision or 5 L&D
spells
47
Healing Slumber Healing C: PI2 37
Heat Fire W: Create Fire,
Shape Fire
31
Hide L&D W: Blur or
Forgetfulness
47
Hide Emotion C&E W*: Sense Emotion 24
Hide Path Plant D: PI2 62
Hide Thoughts C&E C: PI2 • W*: Truthsayer or Hide
Emotion
25
Hinder Body/
Move.
W: Clumsiness or
Haste
21
History Know. W*: Trace 43
Hold Breath Body W: M1, Vigor 21
Hush Sound W*: Silence 66
Hybrid Control
(VH)
Animal D: PI3 19
Ice Dagger Water W: Ice Sphere or
Water Jet
69
Ice Sphere Water W: Shape Water 69
Icy Missiles Water W: Icy Weapon 69
Icy Weapon Water W: Create Water 69
Identify Plant Plant D: PI1 62
Identify Spell Know. W*: Detect Magic 43
Ignite Fire Fire W: M0 31
Illusion Disguise Illusion W: Simple Illusion 40
Illusion Shell Illusion W: Simple Illusion 41
Independence Illusion W: Simple Illusion 41
Infravision L&D W: Keen Vision or
5 L&D spells
47
Initiative Illusion W: Independence,
Wisdom
41
Instant Neutralize Poison (VH)
Healing C: PI4 • D: PI3 37
Invisibility L&D W: 6 L&D spells
including Blur
47
Iron Arm P&W W: Resist Pain,
DX 11+
64
Itch Body W: M0 21
Keen Hearing Sound W*: M0/BT1 54
Keen Sense Mind W*: M0/BT1 54
Knot M&B W: Stiffen 49
Know Illusion Illusion W: Simple Illusion 41
Know Location Know. D: PI2 • W*: M1/
BT1, Tell Position
43
Lend Energy Healing/
Meta
C: PI1 • W: M1 37
Lend Language C&E W*: 3 C&E spells 25
Lend Skill C&E W*: Mind-Sending,
IQ 11+
25
Lend Vitality Healing C: PI1 37
Levitation Move. W: Apportation 57
Light L&D C: PI1 • W: M0 47
Light Jet L&D C: PI2 • W: Continual Light
47
Light Tread Move. D: PI2 • W: Apportation, Shape Earth
57
Lighten Burden Move. W: Apportation 58
Lightning Air/
Weather
D: PI4 • W: M1, 6 Air
spells
71
Lightning Missiles Air/
Weather
W: Lightning
Weapon
72
Lightning Weapon Air/
Weather
W: M2, Lightning 72
Lockmaster Move. W: Either M2,
Apportation or
Locksmith
58
Locksmith Move. W: Apportation 58
Loyalty Mind W*: Bravery, 2 Mind
Control spells
55
Mage Sight Know. W*: Detect Magic 44
Mage-Stealth Sound W*: Hush 66
Magelock P&W W: M1 64
Magic Resistance Meta C: PI3 • W: M1,
1 spell from
7 different
colleges
51
Major Healing
(VH)
Healing C: PI2 37
Manipulate Move. W: Locksmith 58
Mapmaker M&B W: Copy,
Measurement
49
Mass Daze Mind W*: Daze, IQ 13+ 55
Mass Sleep Mind W*: Sleep, IQ 13+ 55
Master Animal D: PI1 19
Measurement Know. W*: M0/BT1 44
Message C&E/
Sound
W*: Great Voice,
Seeker
67
Might Body C: PI1 • W: Lend
Energy
21
Mind-Reading C&E W*: Truthsayer or
Borrow Language
25
Mind-Search (VH) C&E W*: Mind-Reading 25
Mind-Sending C&E W*: Mind-Reading 25
Minor Healing Healing C: PI1 37
Mirror L&D W: Colors 47
Missile Shield P&W W: Apportation or
Shield
64
Monk’s Banquet Food C: PI4 33
Mystic Mist P&W D: PI2 • W: M1,
Watchdog or
Shield
64
Nauseate Body W: 5 Body Control
spells
21
Neutralize Poison Healing C: PI3 • D: PI2 37
Night Vision L&D W: Keen Vision or
5 L&D spells
47
Nightingale P&W W: Sense Danger 64
No-Smell Air D: PI1 • W: Purify
Air
16
Noise Sound W*: Wall of Silence 67
Pain Body W: Spasm 21
Panic Mind W*: Fear 55
Paralyze Limb Body W: M1, 5 Body
Control spells
including
Clumsiness
21
Pathfinder Know. D: PI2 • W*: M1/
BT1, IQ 12+,
2 “Seek” spells
44
Pentagram Meta C: PI5 • W: Spell
Shield
51
Perfect Illusion Illusion W: M1, Complex
Illusion
41
Persuasion C&E C: PI2 • W*: Sense
Emotion
25
Phantom (VH) Illusion W: M2, Apportation,
Hinder, Perfect
Illusion
41
Phase Gate W: M3, Ethereal
Body
34
Phase Other (VH) Gate W: Phase 34
Plant Control Plant D: PI3 62
Plant Sense Plant D: PI3 62
Plant Speech Plant D: PI3 62
Plant Vision Know./
Plant
D: PI2 62
Poison Food Food W: Purify Food 33
Pollen Cloud Plant D: PI2 62
Possession (VH) C&E W*: M1/BT1,
Control Person
25
Prepare Game Food W: Purify Food 33
Projection Know. W*: Sense Spirit, 4
Knowledge spells
44
Protect Animal Animal/
P&W
D: PI3 19
Protection from Evil
Meta/P&W C: PI2 • D: PI2
• W*: M3/BT3,
Sense Evil
64
Purify Air Air C: PI1 • D: PI1
• W: M0
16
Purify Earth Earth/Plant D: PI1 • W: 6 Earth
spells including
Create Earth
28
Purify Food Food C: PI2 • D: PI2
• W: Decay
33
Purify Water Water C: PI1 • D: PI1
• W: Seek Water
70
Quick March Move. D: PI1 • W: M1,
Haste
58
Recover Energy Healing/
Meta
C: PI1 • D: PI1
• W: M1, Lend
Energy
38
Reflect Meta W: Ward 52
Reflect Gaze (VH) P&W W: Mirror 65
Reflexes Body W: Grace, Haste 22
Regeneration
(VH)
Healing C: PI4 38
Rejoin M&B W: Weaken, Restore 49
Relieve Paralysis Healing C: PI3 38
Relieve Sickness Healing C: PI2 38
Remember Path Know. D: PI3 44
Remove Curse Meta C: PI5 52
Repair M&B W: M2, Rejoin 49
Repel Animal Animal D: PI2 19
Repel Hybrids
(VH)
Animal D: PI3 19
Repel Spirits Necro. C: PI3 • W: Banish,
Turn Spirit
60
Resist Acid P&W/
Water
C: PI2 70
Resist Cold Fire C: PI2 • D: PI3
• W: Heat
31
Resist Disease P&W/
Healing
C: PI2 38
Resist Fire Fire C: PI2 • W: Fireproof 31
Resist Lightning Air/P&W/
Weather
C: PI2 • D: PI3
• W: 6 Air spells
72
Resist Pain Body C: PI2 • W: M2, Pain 22
Resist Poison Healing/
P&W
C: PI2 38
Resist Sound P&W/
Sound
W*: 4 Sound spells 67
Resist Water P&W/
Water
W: Umbrella, or
Shape Water,
Destroy Water
70
Restoration (VH) Healing C: PI3 38
Restore M&B W: Find Weakness or
Simple Illusion
49
Restore Hearing Healing C: PI2 39
Restore Memory Healing C: PI2 39
Restore Sight Healing C: PI2 39
Restore Speech Healing C: PI2 39
Retch Body W: Nauseate, Spasm 22
Rider Animal D: PI2 19
Rider Within Animal D: PI2 20
Rive (VH) M&B W: M2, Shatter 49
Rooted Feet Body W: Hinder 22
Roundabout Body W: Tanglefoot 22
Sanctuary (VH) Gate C: PI6 35
Sandstorm Air/Earth D: PI4 • W: Create
Earth,
Windstorm
16
Scry Gate Gate W: Seek Gate 35
Scryguard Meta W: M1 52
See Invisible L&D W: Invisibility, or
Dark Vision,
Infravision
48
See Secrets Know. C: PI3 • W*: Seeker,
Aura
44
Seek Earth Earth D: PI1 • W: M0 28
Seek Fire Fire W: M0 31
Seek Food Food D: PI1 • W: M0 33
Seek Gate Gate W: M2, Seek Magic,
1 spell from 10
colleges
35
Seek Magic Know./
Meta
W*: Detect Magic 45
Seek Plant Plant D: PI1 62
Seek Water Water D: PI1 • W: M0 70
Seeker Know. C: PI2 • W*: M1/
BT1, IQ 12+, 2
“Seek” spells
45
Sense Danger P&W W: Sense Foes 65
Sense Emotion C&E W*: Sense Foes 26
Sense Evil C&E/Meta C: PI1 • D: PI1
• W*: M2/BT2
26
Sense Foes C&E W*: M0/BT1 26
Sense Life C&E C: PI1 • D: PI1
• W*: M0/BT1
26
Sense Spirit Necro. C: PI1 • W: Death
Vision, or M1,
Sense Life
60
Sensitize Body W: M1, Stun 22
Shape Air Air D: PI2 • W: Create
Air
17
Shape Earth Earth D: PI2 • W: Seek
Earth
28
Shape Fire Fire W: Ignite Fire 31
Shape Plant Plant D: PI2 63
Shape Water Water D: PI2 • W: Create
Water
70
Share Energy Healing/
Meta
C: PI1 • D: PI1
• W: Lend Energy
39
Share Vitality Healing C: PI1 39
Sharpen M&B W: Repair 50
Shatter (VH) M&B W: M1, Weaken 50
Shatterproof M&B W: Repair, Shatter 50
Shield P&W C: PI1 • W: M2 65
Shocking Touch Air/
Weather
W: Lightning 72
Sickness Body/Mind W*: Drunkenness 55
Silence Sound C: PI1 • W*: Sound 67
Silver Tongue Sound C: PI3 • W*: Voices,
5 Mind Control
spells
67
Simple Illusion Illusion W: M0, IQ 11+,
no Blindness
42
Sleep Mind W*: Daze 55
Slow Move. W: M1, Haste,
Hinder
58
Slow Fall Move. W: Apportation 58
Smoke Fire W: Shape Fire,
Extinguish Fire
32
Soul Rider C&E W*: Mind-Reading 26
Sound Sound W*: M0/BT1 67
Sound Jet Sound W*: Great Voice 67
Spark Cloud Air/
Weather
W: Lightning, Shape
Air
72
Spark Storm Air/
Weather
D: PI5
• W: Lightning,
Windstorm
72
Spasm Body W: Itch 22
Spell Shield Meta W: M2, Magic
Resistance,
Scryguard
52
Steelwraith Earth W: M2, Walk
Through Earth
28
Stench Air W: Purify Air 17
Stiffen M&B W: Rejoin 50
Stone Missile Earth W: Create Earth 28
Stone to Earth Earth W: Earth to Stone or
4 Earth spells
28
Stone to Flesh Earth C: PI3 • W: M2,
Flesh to Stone,
Stone to Earth
29
Stop Bleeding Healing C: PI1 39
Stop Paralysis Body/
Healing
C: PI3 40
Stop Spasm Body/
Healing
C: PI2 40
Strengthen Will Mind C: PI3 • W*: M1/
BT1, 6 Mind
Control spells
55
Strike Blind Body W: Spasm, 2 L&D
spells
22
Strike Deaf Body W: Spasm, 2 Sound
spells
22
Strike Dumb Body W: Spasm 23
Stun Body W: Pain 23
Summon Spirit Necro. C: PI2 • W: M2,
Death Vision
60
Sunbolt L&D C: PI3 • W: 6 L&D
spells including
Sunlight
48
Sunlight L&D C: PI3 • D: PI3
• W: M1, Colors,
Glow
48
Suspended Animation Healing C: PI3 40
Swim Move./
Water
D: PI3 • W: Levitation, Shape Water
70
Tangle Growth Plant D: PI3 63
Tanglefoot Body W: Clumsiness 23
Telepathy (VH) C&E W*: Mind-Sending 26
Teleport Shield Gate/P&W W: Spell Shield,
Watchdog
65
Tell Position Know. D: PI1 • W*: Measurement
45
Terror Mind W*: Fear 55
Test Food Food C: PI1 • W: M0 33
Test Load Know. W*: Measurement 45
Thunderclap Sound C: PI1 • D: PI1
• W*: Sound
67
Tickle Body W: Spasm 23
Total Paralysis Body W: Paralyze Limb 23
Trace Know. W*: Seeker 45
Trace Teleport Gate/Move. W: M2, IQ 13+,
1 spell from
10 colleges
35
Truthsayer C&E C: PI2 • W*: Sense
Emotion
26
Turn Spirit Necro. C: PI2 • W: Fear,
Sense Spirit
61
Turn Zombie Necro. C: PI2 61
Umbrella P&W/
Water
C: PI1 • D: PI1
• W: Shape Water
or Shield
70
Undo Move. W: Locksmith 59
Vigil (VH) Mind C: PI4 56
Vigor Body C: PI1 • W: Frailty or
Might
23
Voices Sound W*: Sound 68
Walk on Air Air W: Shape Air 17
Walk on Water Water W: Shape Water 71
Walk Through Earth Earth W: 4 Earth spells 29
Walk Through Plants Plant D: PI3 63
Walk Through Wood Plant D: PI3 63
Wall of Lightning Air/
Weather
W: Lightning 72
Wall of Silence Sound W*: Silence 68
Wallwalker Move. W: Apportation 59
Ward Meta W: M1 53
Warmth Fire/P&W C: PI1 • D: PI1
• W: Heat
32
Watchdog P&W C: PI1 • W: Sense
Danger
65
Water Jet Water W: Shape Water 71
Water Vision Know./
Water
D: PI3 • W*: Shape
Water
71
Weaken M&B W: Find Weakness 50
Weaken Will Mind W*: M1/BT1,
Foolishness
56
Weather Dome P&W/
Weather
D: PI2 65
Windstorm Air D: PI2 • W: Shape
Air
17
Wisdom Mind C: PI3 • W*: 6 Mind
Control spells
56
Wither Limb Body W: M2, Paralyze
Limb
23
Wither Plant Plant D: PI4 63
Wizard Eye Know. W*: Apportation,
Keen Vision
45
"""

let snip = """
Analyze Magic Know. W*: Identify Spell 42
Animal Control Animal D: PI2 17
Apportation Move. W: M1 56
Arboreal
Immurement
Plant D: PI5 61
Armor P&W C: PI1 • W: Shield 63
Astral Block Necro. C: PI4 • W: Repel
Spirits, Summon
Spirit
59"""

let magic = """
86 Accelerate Time* Area/R-Spec. Gate 1 min. Varies 2 sec. M2, IQ 13+, 2 spells each 28
from 10 colleges
65 Accuracy Enchantment Enchantment Perm. Varies – Enchant and 5 Air spells 17
191 Acid Ball Missile Water Instant 1 to Magery# 1 to 3 sec. M2, Create Acid 11
192 Acid Jet Regular Water 1 sec. 1 to 3 1 sec. M2, Water Jet, Create Acid 11
58 Adjustable Clothing Enchantment Enchantment Perm. Varies – Enchant, Reshape 16
151 Affect Spirits Regular Necro. 1 min. 4/2 2 sec. Solidify 6
154 Age* Regular/R-HT Necro. Perm. 10 to 50 1 min. Youth or 6 other 8
Necromantic spells
40 Agonize Regular/R-HT Body 1 min. 8/6 1 sec. M2, Sensitize 7
24 Air Jet Regular Air 1 sec. 1 to 3/S 1 sec. Shape Air 3
24 Air Vision Regular Air/Knowledge 1 min. 1 per mi./H 1 sec. Shape Air 3
26 Air Vortex Area/R-HT or DX Air/Movement 10 sec. 8/3 2 sec. M2, Body of Air, Windstorm 7
100 Alarm Regular Knowledge 1 week 1 1 sec. Tell Time 1
133 Alertness* Regular Mind 10 min. 2 to 10/H 1 sec. Any two Keen spells 2
41 Alter Body Regular/R-HT Body 1 hr. 8/6 2 min. Alter Visage 16
55 Alter Terrain* Area Earth 2d days 1# 10 sec. M3, all four elemental 17
Shape spells, Shape Stone
41 Alter Visage Regular/R-HT Body 1 hr. 4/3 1 min. Shapeshifting or Perfect Illusion, 15
and 8 Body spells
41 Alter Voice Regular/R-HT Body/Sound 1 hr. 2/2 1 min. 4 Body spells, 4 Sound spells 8
39 Ambidexterity Regular Body 1 min. 3/2 1 sec. Grace 4
58 Amulet Enchantment Enchantment Perm. 50/pt. MR – Talisman for appropriate spell 14
102 Analyze Magic Inform./R-spell Knowledge Instant 8 1 hr. Identify Spell 3
106 Ancient History Information Knowledge Instant Varies min.=cost History 10
30 Animal Control Regular/R-Will Animal 1 min. Varies 1 sec. Beast-Soother 1
177 Animate Machine/TL Regular/R-Will Tech 1 min. Varies Varies Machine Control, 17
Animation or Animate Object
117 Animate Object* Regular/R-Spec Mk-Brk 1 min. Varies 3 sec. M2, 3 Shape spells 8
164 Animate Plant Regular Plant 1 min. Varies 5 sec. 7 Plant spells 7
154 Animate Shadow Regular/R-HT Necro. 10 sec. 4/4 2 sec. Skull-Spirit, Shape Darkness 10
150 Animation* Regular Necro. 1 min. Varies 5 sec. Summon Spirit 4
142 Apportation Reg./R-Will Movement 1 min. Varies 1 sec. M1 1
165 Arboreal Immurement Regular/R-HT Plant Indef.# 8# 3 sec. M2, Walk Through Wood 9
167 Armor Regular Protection 1 min. Varies 1 sec. Shield 3
159 Astral Block Area Necro. 10 min. 4/2# 2 sec. Summon Spirit, Repel Spirits 17
105 Astral Vision* Regular Knowledge/Necro. 1 min. 4/2 1 sec. Sense Spirit, See Invisible 7
169 Atmosphere Dome Area Protection/Air 6 hrs. 4/H 1 sec. Purify Air, Weather Dome
69 Attune Enchantment Enchantment Perm. 100 – Bane 14
101 Aura Information Knowledge Instant 3 1 sec. Detect Magic 2
140 Avoid Area Mind 1 hr. 3/3 1 min. Hide, Fear, Forgetfulness 11
90 Awaken Area Healing Instant 1 1 sec. Lend Vitality 3
178 Awaken Computer/TL Regular Tech 1 hr. Varies 10 sec. Animation, Wisdom 12
115 Awaken Craft Spirit Regular Mk-Brk/Necro. 1 min. 3/1 5 sec. Inspired Creation, Sense Spirit 4
39 Balance Regular Body 1 min. 5/3 1 sec. Grace 4
197 Ball of Lightning Regular Weather/Air 1 min. 2 to 6/H 1 to 3 sec. Apportation, Lightning 9
62 Bane Enchantment Enchantment Perm. 100 – Enchant 13
156 Banish Spec./R-Will Necro. Instant Varies 5 sec. M, 1 spell each from 10 colleges 11
83 Beacon Area Gate/Movement 24 hrs. 10/H 30 sec. Teleport, Timeport, or Plane Shift 7
30 Beast Link Regular Animal Special 3 5 sec. Beast Summoning 2
32 Beast Possession Regular/R-Will Animal 1 min. 6/2 5 sec. Rider Within or Possession 4
30 Beast-Rouser Regular Animal 1 hour# 1 to 3 1 sec. Vexation or Animal Empathy 0
32 Beast Seeker Information Animal Instant 3# 1 sec. Seeker or Beast Summoning 4
and 2 Seek spells
29 Beast-Soother Regular Animal Perm.# 1 to 3 1 sec. Persuasion or 0
Animal Empathy advantage
31 Beast Speech Regular Animal 1 min. 4/2 1 sec. Beast Summoning 2
30 Beast Summoning Regular Animal 1 min. 3/2# 1 sec. Beast-Soother 1
134 Berserker Regular/R-Will Mind 10 min.# 3/2 4 sec. Bravery 4
158 Bind Spirit* Regular/R-Will Necro. Perm. Varies 5 min. Command Spirit, Soul Jar 15
112 Blackout Area Lt-Dk 1 min. 2/1 1 sec. Darkness 3
168 Bladeturning Regular/R-Spec Protection 1 min. 2/2 1 sec. Shield or Turn Blade 3
66 Blank Spell Arrow* Enchantment Enchantment Perm. 30¥capacity# – Spell Arrow 21
129 Bless Regular Meta-Spell Special Varies min.=cost M2, 2 spells each from 10 colleges#22
161 Bless Plants Area Plant 1 crop/season 1 5 min. Heal Plant 3
162 Blight Area Plant 1 crop/season 1 5 min. Plant Growth 4
148 Blink Blocking Movement/Gate Instant 2 1 sec. Teleport 7
148 Blink Other* Blocking Movement/Gate Instant 2 1 sec. Blink 8
166 Block Blocking Protection Instant 1/DB+# 1 sec. M1 1
162 Blossom Area Plant 1 hour 2 5 min. Plant Growth 4
113 Blur Regular Lt-Dk 1 min. 1 to 5/S 2 sec. Darkness or Gloom 3
24 Body of Air Regular/R-HT Air 1 min. 4/1 5 sec. Shape Air 3
76 Body of Flames* Regular/R-HT Fire 1 min. 12/4 5 sec. Breathe Fire 9
189 Body of Ice* Regular/R-HT Water 1 min. 7/3 5 sec. M2, Body of Water, Freeze 8
198 Body of Lightning* Regular/R-HT Weather/Air 1 min. 12/4 5 sec. M2, Lightning 9
183 Body of Metal* Regular/R-HT Tech 1 min. 12/6 5 sec. M2, Shape Metal 8
183 Body of Plastic Regular/R-HT Tech 1 min. 10/5 5 sec. M2, Shape Plastic
114 Body of Shadow Regular/R-HT Lt-Dk 1 min. 6/3 5 sec. M2, Shape Darkness 6
165 Body of Slime Regular/R-HT Plant 1 min. 6/2 5 sec. M2, Plant Form, Shape Water 13
54 Body of Stone* Regular/R-HT Earth 1 min. 10/5 5 sec. Stone to Flesh 8
185 Body of Water Regular/R-HT Water 1 min. 5/2 5 sec. Shape Water 4
27 Body of Wind Regular/R-HT Air 1 min. 8/4 2 sec. M3, Body of Air, Windstorm, 13
1 spell each from 5 colleges#
165 Body of Wood Regular/R-HT Plant 1 min. 7/3 5 sec. M2, Plant Form 9
88 Body-Reading Information/R-Will Healing Instant 2 30 sec. Sense Life or Awaken 1
189 Boil Water Regular Water Perm. Varies 10 sec. Shape Water, Heat 8
37 Boost Attribute Regular or Blocking Body/Mind Instant 1 to 5 none Varies Varies
46 Borrow Language Regular Comm. 1 min. 3/1 3 sec. Lend Language 4
47 Borrow Skill Regular Comm. 1 min. 4/3 3 sec. Lend Skill 8
Page Spell Name Class College Duration Energy Time to cast Prerequisites Prerequisite Count
134 Bravery Area/R-Will-1 Mind 1 hr. 2 1 sec. Fear 3
26 Breathe Air Regular Air/Water 1 min. 4/2 1 sec. Create Water, Destroy Air 6
76 Breathe Fire* Regular Fire 1 sec. 1 to 4 2 sec. M1, Flame Jet, Resist Fire 8
182 Breathe Radiation* Regular Tech 1 sec. 1 to 4 2 sec. M2, Radiation Jet 10
192 Breathe Steam* Regular Water 1 sec. 1 to 4 2 sec. M1, Steam Jet, Resist Fire 14
189 Breathe Water Regular Water/Air 1 min. 4/2 1 sec. Create Air, Destroy Water 6
111 Bright Vision Regular Lt-Dk 1 min. 2/1 1 sec. Keen Vision or 5 Light spells; 1
no Blindness
76 Burning Death* Regular/R-HT Fire/Necro. 1 sec. 3/2 3 sec. M2, Heat, Sickness 14
76 Burning Touch Melee Fire Instant 1 to 3 1 sec. M2, 6 Fire spells inc. Heat 8
39 Cadence Regular Body 1 hr. 5/3 10 sec. Haste, Grace 5
168 Catch Missile Blocking Protection Instant 2 1 sec. Deflect Missile 3
123 Catch Spell* Blocking Meta-Spell Instant 3 1 sec. M2, DX 12+, Return Missile 11
126 Charge Powerstone* Regular Meta-Spell Perm. 3/pt 10 min. M3, Powerstone, Lend Energy 15
139 Charm Regular/R-Will Mind 1 min. 6/3 3 sec. M1, Loyalty, 7 other 11
Mind Control spells
40 Choke Regular/R-HT Body 30 sec. 4 1 sec. M1, 5 Body spells inc. Spasm 6
116 Clean Area Mk-Brk Perm. 2 1 sec. Restore 4
94 Cleansing Regular/R-Spec Healing Perm. Varies 3 sec. Minor Healing, Purify Earth 13
35 Climbing Regular Body 1 min. 1 to 3/S 1 sec. – 0
148 Cloud-Vaulting* Regular Movement/Weather 1 sec./100 mi.# 7 1 sec. M2, Jump, Cloud-Walking 13
148 Cloud-Walking Regular Movement/Weather 1 hr. 3/2 1 sec. Walk on Air, Walk on Water 9
194 Clouds Area Weather/Air 10 min. 1/20/S 10 sec. 2 Water spells, 2 Air spells 4
36 Clumsiness Regular/R-HT Body 1 min. 1 to 5/H 1 sec. Spasm 2
74 Cold Regular Fire 1 min. Varies 1 min. Heat 4
110 Colors Regular Lt-Dk 1 min. 2/1 1 sec. Light 1
136 Command Blocking/R-Will Mind Instant 2 1 sec. M2, Forgetfulness 8
153 Command Spirit Regular/R-Will Necro. 1 min. Varies 2 sec. Summon Spirit, Turn Spirit 8
48 Communication* Regular Comm. 1 min. 4/4 4 sec. Wizard Eye, Far-Hearing, 10
Voices, Simple Illusion
137 Compel Lie Regular/R-Will Mind/Comm. 5 min. 4/2 1 sec. Emotion Control 6
47 Compel Truth Inform./R-Will Comm. 5 min. 4/2 1 sec. M2, Truthsayer 6
96 Complex Illusion Area Illusion 1 min. 2/H 1 sec. Sound, Simple Illusion 4
162 Conceal Area Plant 1 min. varies# 4 sec. Plant Growth 4
122 Conceal Magic Regular Meta-Spell 10 hrs. 1 to 5/S# 3 sec. Detect Magic 2
26 Concussion Missile Air/Sound Instant 2 to 2¥Magery# 1 to 3 sec. Shape Air, Thunderclap 5
189 Condense Steam Area Water Perm. 2# 10 sec. Cold or Boil Water 5
180 Conduct Power/TL* Special Tech 1 min. 0/1# 1 sec. M1, Seek Power 2
110 Continual Light Regular Lt-Dk Varies Varies 1 sec. Light 1
113 Continual Mage Light Regular Lt-Dk Varies Varies 1 sec. Mage Light, Continual Light 6
114 Continual Sunlight Area Lt-Dk Varies 3 1 sec. Sunlight 6
120 Contract Object* Regular Mk-Brk 1 hour Varies 3 sec. M3, Transform Object 17
99 Control Creation Regular/R-spell Illusion Instant 1 2 sec. Create Animal or Create Servant 19
28 Control Elemental Special 4 Diff. 1 min. Special 2 sec. Summon Elemental# 10
85 Control Gate Regular/R-Gate Gate 1 min. 6/3 10 sec. M3, Seek Gate 16
97 Control Illusion Regular/R-spell Illusion Perm. 1 2 sec. Perfect Illusion 7
40 Control Limb Regular/R-Will Body 5 sec. 3/3# 1 sec. M1, 5 Body spells inc. Spasm 6
49 Control Person Regular/R-Will Comm. 1 min. 6/3 10 sec. Soul Rider or Telepathy 5
152 Control Zombie Regular/R-spell Necro. Perm. 3 1 sec. Zombie 7
173 Converse Regular/R-spell Sound Indef.# 2 1 sec. M1, Garble, Silence 5
78 Cook Regular Food Instant 1 per meal 5 sec. Test Food, Create Fire 3
195 Cool Area Weather/Air 1 hour 1/10/S 1 min.# Cold, 4 Air spells 9
187 Coolness Regular Water/Protection 1 hr. 2/1 10 sec. Cold 5
116 Copy Regular Mk-Brk Perm. 2 plus 1/copy 5 sec. Dye, 1 Accented language 7
64 Cornucopia Enchantment Enchantment Perm. 50¥$ value# – Enchant, 2 Weapon 15
Enchantment spells
43 Corpulence* Regular/R-HT Body 10 min. 6/6 3 sec. M2, Create Earth, Create Water, 21
4 Body spells inc. Alter Body
121 Counterspell Regular/R-spell Meta-Spell Instant Varies 5 sec. M1 1
190 Create Acid Regular Water Perm. 4/gal. 2 sec. Create Water, Create Earth 8
23 Create Air Area Air 5 sec.# 1 1 sec. Purify Air or Seek Air 1
98 Create Animal Regular Illusion 1 min. Varies sec.=cost Create Water, Create Object, 20
IQ 12+
84 Create Door Regular Gate 10 sec. 2/10 sq. ft.# 5 sec. Teleport, any one 13
Walk Through spell
51 Create Earth Regular Earth Perm. 2/25 cu. ft. 1 sec. Earth to Stone 4
28 Create Elemental Special 4 Diff. Perm. Special Special M2, Control Elemental# 12
72 Create Fire Area Fire 1 min. 2/H 1 sec. Ignite Fire or Seek Fire 1
79 Create Food Regular Food Perm. Varies 30 sec. Cook, Seek Food 5
179 Create Fuel/TL Regular Tech Perm. 1/lb. 30 sec. Seek Fuel, 2 transmutation spells 10
85 Create Gate* Regular Gate 1 min. Varies Varies Control Gate and Teleport, 24
Timeport, or Plane Shift
188 Create Ice Regular Water Perm. 2/gal. 1 sec. Freeze 5
99 Create Mount Regular Illusion 1 hr. Varies 3 sec. M3, Create Animal 22
98 Create Object* Regular Illusion Indef.# 2/5 lbs. sec.=cost M2, Create Earth, Perfect Illusion 12
163 Create Plant Area Plant Perm. Varies sec.=cost M, Plant Growth 5
98 Create Servant Regular Illusion 1 min. Varies 3 sec. M3, IQ 12+, Create Object 18
190 Create Spring Regular Water Perm. Varies 1 min. Dry Spring, Shape Water 8
190 Create Steam Area Water 5 min.# 2 1 sec. Boil Water 9
98 Create Warrior Regular Illusion 1 min. Varies 4 sec. Create Servant 19
184 Create Water Regular Water Perm. 2/gal. 1 sec. Purify Water 2
71 Crystal Ball Enchantment Enchantment Perm. 1,000 – Enchant, Divination 21
(Crystal-Gazing)
91 Cure Disease Regular Healing Instant 4 10 min. Major Healing, Relieve Sickness 6
182 Cure Radiation* Regular Tech/Healing Perm. 1/10 rads# 30 sec. Resist Radiation, Major Healing 13
194 Current Special; Area Weather/Water 1 hr. 1/50/S 1 min. 6 Water spells 6
129 Curse Regular Meta-Spell Special Varies Varies M2, 2 spells each from 10 colleges#22
144 Dancing Object Regular Movement 1 hr. 4/2 10 sec. M2, Apportation 3
67 Dancing Shield Enchantment Enchantment Perm. 250/lb.# – Enchant, Dancing Object 14
63 Dancing Weapon Enchantment Enchantment Perm. 1,000/lb.# – Enchant, Dancing Object 14
111 Dark Vision Regular Lt-Dk 1 min. 5/2 1 sec. Night Vision or Infravision 2
111 Darkness Area Lt-Dk 1 min. 2/1 1 sec. Continual Light 2
134 Daze Regular/R-HT Mind 1 min. 3/2 2 sec. Foolishness 5
149 Death Vision Regular Necro. 1 sec. 2 3 sec. M1 1
41 Deathtouch Melee Body Instant 1 to 3 1 sec. Wither Limb 9
36 Debility Regular/R-HT Body 1 min. 1 per ST-/H 1 sec. – 0
42 Decapitation* Regular/R-HT+2 Body Indef. 4 2 sec. M2, Alter Body 18
77 Decay Regular Food Perm. 1/meal 1 sec. Test Food 1
67 Defending Shield Enchantment Enchantment Perm. Varies – Enchant, Grace 16
64 Defending Weapon Enchantment Enchantment Perm. Varies – Enchant, Dancing Object 14
67 Deflect Enchantment Enchantment Perm. Varies – Enchant 13
73 Deflect Energy Blocking Fire Instant 1 1 sec. M1, Shape Fire 3
143 Deflect Missile Blocking Movement/Protection Instant 1 1 sec. Apportation 2
188 Dehydrate Regular/R-HT Water Perm. 1 to 3 2 sec. 5 Water spells inc. Destroy Water 5
130 Delay Regular Meta-Spell 2 hrs. 3/3 10 sec. M3, 15 spells 18
173 Delayed Message Area Sound Indef.# 3# 4 sec. M1, Voices, Sense Life 4
24 Destroy Air Area Air Instant 2 1 sec. Create Air 2
185 Destroy Water Area Water Perm. 3/S 1 sec. Create Water 3
101 Detect Magic Regular Knowledge Instant 2 5 sec. M1 1
166 Detect Poison Area/Information Protection/Healing Instant 2 2 sec. Sense Danger or Test Food 1
25 Devitalize Air Area Air varies 2 1 sec. Destroy Air 3
120 Disintegrate* Regular Mk-Brk Perm. 1 to 4 1 sec. M2, Shatter, Ruin# 18
135 Disorient Area/R-Will Mind Indef.# 1 10 sec. Foolishness 5
99 Dispel Creation Regular/R-spell Illusion Instant 1 or 3# 1 sec. Control Creation 20
97 Dispel Illusion Regular/R-spell Illusion Instant 1 1 sec. Control Illusion 8
126 Dispel Magic Area/R-spell Meta-Spell Perm. 3 sec.=cost Counterspell and 12 other spells 14
49 Dispel Possession Regular/R-spell Comm. Instant 10 10 sec. Soul Rider or Possession# 5
124 Displace Spell Regular/R-spell Meta-Spell Varies Varies 5 sec. Suspend Magic 11
144 Distant Blow Regular Movement 5 sec. 3/3 3 sec. M2, Apportation 3
46 Distill Regular Food/Water Perm. 1/quart 10 sec. Mature, Destroy Water 7
84 Divert Teleport* Blocking/R-spell Gate/Movement Instant Varies 1 sec. M3, Trace Teleport 9
108 Divination Information Knowledge Instant 10 1 hr.# History, other spells# 10
62 Doppelgänger* Enchantment Enchantment Perm.# 1,000 – M3, Golem, History, Enslave 38
130 Drain Magery* Regular/R-Will+M Meta-Spell Perm. 30 10 min. M3, Suspend Magery 24
127 Drain Mana* Area Meta-Spell Perm. 10 1 hr. Dispel Magic, Suspend Mana 15
180 Draw Power/TL* Special Tech 1 min. 0/1# 1 sec. Steal Power, 2 spells each 23
from 10 colleges
46 Dream Projection Regular Comm./Mind 1 min. 3/3 1 min. Dream Sending 5
45 Dream Sending Regular/R-Will Comm./Mind 1 hr. 3 1 min. Dream Viewing or Sleep 4
45 Dream Viewing Regular/R-Will Comm. 1 hr. 2/1 10 sec. Truthsayer or Sleep 3
136 Drunkenness Regular/R-Will Mind 1 min. Varies 2 sec. Foolishness, Clumsiness 8
188 Dry Spring Regular Water Perm. Varies# 1 min. Destroy Water, Shape Earth 6
133 Dull Sense Regular/R-HT Mind 30 min. 1 to 3/H 1 sec. – 0
134 Dullness* Regular/R-HT Mind 10 min. 2 to 10/H 1 sec. Any two Dull spells 2
98 Duplicate* Regular Illusion Indef.# 3/5 lbs. sec.=cost Create Object, Copy 16
116 Dye Regular Mk-Brk 2d days Varies 3 sec. Restore, Colors 6
25 Earth to Air Regular Air/Earth Perm. 5/25 cu. ft.# 2 sec. Create Air, Shape Earth 4
51 Earth to Stone Regular Earth Perm. 3/25 cu. ft.# 1 sec. M1, Shape Earth 3
52 Earth to Water Regular Earth/Water Perm. 1/25 cu. ft.# 1 sec. M1, Create Water, Shape Earth 6
51 Earth Vision Regular Earth/Know. 30 sec. 2/10 yds.# 1 sec. Shape Earth 2
54 Earthquake Area Earth 1 min. 2/S 30 sec. M2, 6 Earth spells inc. Earth Vision 8
107 Echoes of the Past Regular Knowledge/Sound 1 min. 2/2# 10 sec. M2, History, Voices 13
139 Ecstasy* Regular/R-Will Mind 10 sec. 6 3 sec. M2, Emotion Control 8
71 Effigy* Enchantment Enchantment Perm. 1,000 – Enchant, Scryfool, Ward 17
137 Emotion Control Area/R-Will Mind 1 hr. 2 1 sec. Loyalty or Mental Stun 5
56 Enchant* Enchantment Enchantment Perm. Varies Varies M2, 1 spell each from 10 colleges 12
135 Encrypt Regular/R-Spec. Mind 1 week Varies 1 sec. Daze 6
42 Enlarge* Regular Body 1 hr. 2/+1 SM/S 5 sec. M2, Alter Body 18
120 Enlarge Object* Regular Mk-Brk 1 hour Varies 3 sec. Extend Object 18
43 Enlarge Other* Regular/R-HT Body 1 hr. 2/+1 SM/S 10 sec. M3, Enlarge 20
141 Enslave* Regular/R-Will Mind Perm. 30 1 sec. Charm, Telepathy 16
60 Ensorcel* Enchantment/R-Spec. Enchantment Perm.# 200¥spell cost – Malefice 17
139 Enthrall Special/R-Will Mind 1 hr. 3/3 1 sec. Forgetfulness, Daze, Slow 11
53 Entombment Regular/R-HT Earth Perm. 10# 3 sec. M2, 5 Earth spells 7
157 Entrap Spirit Special Necro. 5 min. Varies 1 sec. M1, Soul Jar, Turn Spirit 14
192 Essential Acid* Regular Water Perm. 8/gal. 1 sec. 6 Acid spells 18
26 Essential Air Area Air Perm. 2 3 sec. 6 Air spells 6
53 Essential Earth Regular Earth Perm. 8 30 sec. 6 Earth spells 7
75 Essential Flame Area Fire 1 min. 3/2# 3 sec. 6 Fire spells 6
79 Essential Food* Regular Food Perm. 3/meal# 30 sec. 6 Food spells inc. Create Food 6
179 Essential Fuel/TL Regular Tech Perm. 8/gal. 1 sec. 6 Energy spells 7
189 Essential Water Regular Water Perm. 3/gal. 1 sec. 6 Water spells 6
164 Essential Wood Regular Plant Perm. 8 30 sec. 6 Plant spells 61
146 Ethereal Body* Regular Movement 10 sec. 8/4 30 sec. 6 Movement spells or 6
M3 and Body of Air
154 Evisceration* Regular/R-HT Necro. Instant 10 5 sec. M3, Apportation, Steal Vitality 9
or IQ
49 Exchange Bodies* Regular/R-Will Comm. Perm. 120 1 hr. Permanent Possession, Soul Jar 20
118 Explode* Regular Mk-Brk Instant 2 to 6 1 sec. M2, Shatter, Apportation 10
75 Explosive Fireball Missile Fire Instant 2 to 2¥Magery# 1 to 3 sec. Fireball 5
196 Explosive Lightning Missile Weather/Air Instant 2 to 2¥Magery# 1 to 3 sec. Lightning 8
120 Extend Object* Regular Mk-Brk 1 hour Varies 3 sec. M3, Transform Object 17
72 Extinguish Fire Regular Fire Perm. 3 1 sec. Ignite Fire 1
181 Extinguish Regular Tech Perm. 1/10 rads/hr. 1 sec. M2, Extinguish Fire, 13
Radiation* Earth to Air, Irradiate
122 False Aura Regular/Area/R-IQ# Meta-Spell 10 hrs. 4/H 10 sec. Conceal Magic, Aura 4
139 False Memory Regular/R-Will Mind Varies Varies 5 sec. Forgetfulness, 6 other 12
Mind Control spells
163 False Tracks Regular/R-Will Plant 1 min. 2/1 1 sec. Shape Plant, Shape Earth 5
100 Far-Feeling Regular Knowledge 1 min. 3/1 3 sec. M1 1
173 Far-Hearing Information Sound/Know. 1 min. 4/2 3 sec. M1, 4 Sound spells, no Deafness 5
or Hard of Hearing
77 Far-Tasting Regular Food/Knowledge 1 min. 3/1 3 sec. M1, no anosmia, Seek Food 2
or Seek Air
135 Fascinate Regular or Blocking/R-Will Mind Indef.# 4 1 sec. Daze 6
73 Fast Fire Regular Fire 1 min. Varies 1 sec. Slow Fire 3
118 Fasten Regular/R-DX Mk-Brk Perm. 3# 1 sec. Knot 10
139 Fear Area/R-Will Mind 10 min. 1 1 sec. Sense Emotion or Empathy 2
89 Final Rest Regular Healing/Necro. Perm. 20 10 min.# M1 or Spirit Empathy 1
101 Find Direction Information Knowledge Instant 2 1 sec. M1 1
116 Find Weakness Information Mk-Brk Instant 1# 2 sec. 1 spell of each four elements 4
75 Fire Cloud Area Fire 10 sec. 1 to 5/S 1 to 5 sec. Shape Air, Fireball 8
74 Fireball Missile Fire Instant 1 to Magery# 1 to 3 sec. M1, Create Fire, Shape Fire 4
73 Fireproof Area Fire 1 day 3# 5 min. Extinguish Fire 2
73 Flame Jet Regular Fire 1 sec. 1 to 3/S 1 sec. Create Fire, Shape Fire 3
75 Flaming Armor Regular Fire 1 min. 6/3 1 sec. M1, Resist Fire, Flame Jet 8
75 Flaming Missiles Regular Fire 1 min. 4/2# 3 sec. Flaming Weapon 7
75 Flaming Weapon Regular Fire 1 min. 4/1 2 sec. M2, Heat 6
112 Flash Regular Lt-Dk Instant 4 2 sec. Continual Light 2
190 Flesh to Ice* Regular/R-HT Water Perm. 12 2 sec. M1, Frostbite, Body of Water 8
51 Flesh to Stone Regular/R-HT Earth Perm. 10# 2 sec. Earth to Stone 4
145 Flight* Regular Movement 1 min. 5/3 2 sec. M2, Levitation 4
146 Flying Carpet* Regular Movement 10 min. 1/sq. ft./H 5 sec. Flight, or M2 and Walk on Air 5
193 Fog Area Weather/Water 1 min. 2/H 1 sec. Shape Water 4
79 Fool’s Banquet Regular Food 1 day 2 per meal 1 sec. M1, Cook, Foolishness 10
134 Foolishness Regular/R-Will Mind 1 min. 1 per IQ-/H 1 sec. IQ 12+ 4
170 Force Dome Area Protection 10 min. 3/2 1 sec. Weather Dome, Apportation 11
170 Force Wall Regular Protection 10 min. 2/yd./S 1 sec. Force Dome 12
162 Forest Warning Area Plant 10 hrs. 2#/S 1 sec. 4 Plant spells 4
135 Forgetfulness Regular/R-Will Mind 1 hr. 3/3 10 sec. M1, Foolishness 6
or skill
66 Fortify Enchantment Enchantment Perm. Varies – Enchant 13
185 Foul Water Area Water/Food Perm. 3 1 sec. Purify Water, Decay 4
37 Frailty Regular/R-HT Body 1 min. 2 per HT-/S# 1 sec. Lend Energy 2
148 Freedom Regular Movement/Protection 1 min. 2/pt/S 1 sec. 3 Body spells, 3 Movement spells, 10
3 Protection spells
185 Freeze Regular Water Perm. Varies 10 sec. Shape Water 4
193 Frost Area Weather/Water Indef. 1 1 sec. Create Water or Cold 3
189 Frostbite Regular/R-HT Water Perm. 1 to 3 3 sec. Frost, Freeze 6
38 Fumble Blocking/R-DX Body Instant 3 none Clumsiness 3
172 Garble Regular/R-Will Sound 1 min. 4/2 1 sec. Voices 2
43 Gauntness* Regular/R-HT Body 10 min. 6/6 3 sec. M2, Earth to Air, Destroy Water, 17
4 Body spells inc. Hunger
190 Geyser* Area Water 1 sec. 5/2 5 sec. 6 Water spells inc. Create Well 10
and either 4 Earth or Fire spells
65 Ghost Weapon Enchantment Enchantment Perm. 250/lb.# – Enchant, Solidify 17
46 Gift of Letters* Regular Comm. 1 min. Varies 1 sec. Borrow Language, 5
3 languages at Accented
46 Gift of Tongues* Regular Comm. 1 min. Varies 1 sec. Borrow Language, 5
3 languages at Accented
103 Glass Wall Regular Knowledge 1 min. 4/2 1 sec. 5 Knowledge spells or Earth Vision 3
141 Glib Tongue Regular/R-Will Mind 5 min. 2/1 1 sec. Suggestion 15
176 Glitch/TL Regular/R-HT Tech Instant 3 1 sec. Machine Control 12
112 Gloom Area Lt-Dk Varies Varies Varies Continual Light 2
112 Glow Area Lt-Dk Varies Varies Varies Continual Light 2
142 Glue Area Movement 10 min. 3/S 1 sec. Haste 1
59 Golem* Enchantment Enchantment Perm. Varies Varies Enchant, Shape Earth, 16
Animation
37 Grace Regular Body 1 min. 4 per DX+/S 1 sec. Clumsiness 3
63 Graceful Weapon Enchantment Enchantment Perm. 150/lb. – Enchant, Apportation 13
142 Grease Area Movement 10 min. 3/S 1 sec. Haste 1
141 Great Geas* Regular/R-Will Mind Perm. 30 1 min. M3, 15 Mind Control spells 20
inc. Lesser Geas
141 Great Hallucination* Regular/R-Will Mind 1 min. 6/3 4 sec. M2, Hallucination 16
146 Great Haste* Regular Movement 10 sec. 5# 3 sec. M1, Haste, IQ 12+ 6
91 Great Healing Regular Healing Perm. 20 1 min. M3, Major Healing 7
34 Great Shapeshift* Special Animal 1 min. 20/H# 5 sec. M3, Alter Body, 26
4 Shapeshifting, 10 other spells
173 Great Voice Regular Sound 1 min. 3/1 2 sec. Voices, Thunderclap 3
122 Great Ward Block/R-spell Meta-Spell Instant 1 per subject# none M2, Ward 3
62 Great Wish* Enchantment Enchantment Special 2,000 – M3, Wish, (DX + IQ):30+ 41
195 Hail Area Weather/Water 1 min. 1/5/S# 1 sec. Snow 8
39 Hair Growth Regular/R-HT Body 5 sec. 1/1 1 sec. 5 Body spells 5
39 Haircut Regular/R-HT Body Instant 2 2 sec. Weaken, 2 Body spells 8
140 Hallucination Regular/R-Will Mind 1 min. 4/2 3 sec. Madness, Suggestion 14
94 Halt Aging* Regular Healing 1 month 20 1 sec. M2, 8 Healing spells 11
128 Hang Spell* Special Meta-Spell 1 hr. Varies 10 sec. Delay 19
167 Hardiness Blocking Protection Instant 1/DR+# 1 sec. Block 2
142 Haste Regular Movement 1 min. 2/pt./H 2 sec. – 0
146 Hawk Flight* Regular Movement 1 min. 8/4 3 sec. Flight 5
111 Hawk Vision Regular Lt-Dk 1 min. 2/lvl./H# 2 sec. Keen Vision or 5 Light spells; 1
no Blindness or Bad Sight
161 Heal Plant Area Plant Perm. 3 1 min. Identify Plant 2
94 Healing Slumber Regular/R-# Healing 8 hrs.# 6 or 10 30 sec. M2, Sleep, Minor Healing 13
74 Heat Regular Fire 1 min. Varies 1 min. Create Fire, Shape Fire 3
57 Hex Enchantment Enchantment Perm. 200 – Enchant 13
113 Hide Regular Lt-Dk 1 hr. 1 to 5/S 5 sec. Blur or Forgetfulness 4
45 Hide Emotion Regular Comm. 1 hour 2/2 1 sec. Sense Emotion 2
86 Hide Object Regular Gate 1 hr. 1/lb./S 10 sec. Hideaway, Teleport 29
162 Hide Path Regular Plant 1 min. 2/1 1 sec. Heal Plant 3
46 Hide Thoughts Regular Comm. 10 min. 3/1 1 sec. Truthsayer or Hide Emotion 3
61 Hideaway Enchantment Enchantment Perm. 50# – Enchant, Create Object, Lighten 21
36 Hinder Regular Body/Movement 1 min. 1 to 4/S 1 sec. Haste or Clumsiness 1
106 History Information Knowledge Instant Varies sec.=cost Trace 9
39 Hold Breath Regular Body 1 min. 4/2 1 sec. M1, Vigor 5
143 Hold Fast Blocking Movement Instant 1/yd.# 1 sec. Apportation 2
70 Homunculus Enchantment Enchantment Perm. 800 – Enchant, Mind-Sending 17
38 Hunger Regular/R-HT Body/Food Instant 2 5 sec. M1, Debility, Decay 4
172 Hush Regular/R-Will Sound 10 sec.# 2/1 2 sec. Silence 2
30 Hybrid Control* Regular/R-Will Animal 1 min. 6/3 1 sec. 2 Control spells# 3
188 Ice Dagger Missile Water Instant 1 to Magery# 1 to 3 sec. Ice Sphere or Water Jet 5
186 Ice Slick Area Water Perm. 3 Varies Frost 4
186 Ice Sphere Missile Water Instant 1 to Magery# 1 to 3 sec. Shape Water 4
192 Icy Breath* Regular Water 1 sec. 1 to 4 2 sec. M1, Snow Jet, Resist Cold 12
186 Icy Missiles Regular Water 1 min. 4/2 3 sec. Icy Weapon 4
188 Icy Touch Melee Water Perm. 2# 1 sec.# M1, 4 Water spells 5
185 Icy Weapon Regular Water 1 min. 3/1 3 sec. Create Water 3
182 Identify Metal Information Tech Instant 1 1 sec. – 0
161 Identify Plant Information Plant Instant 2 1 sec. Seek Plant 1
182 Identify Plastic Information Tech Instant 1 1 sec. – 0
102 Identify Spell Information Knowledge Instant 2 1 sec. Detect Magic 2
72 Ignite Fire Regular Fire 1 sec. 1 to 4/S 1 sec. – 0
96 Illusion Disguise Regular Illusion Varies 3 1 sec. Simple Illusion 3
96 Illusion Shell Regular Illusion 1 min. 1 or 2/H 1 sec. Simple Illusion 3
107 Images of the Past Regular Knowledge/Lt-Dk 1 min. 3/3# 10 sec. M2, History, Simple Illusion 12
172 Imitate Voice Regular/R-HT Sound 1 min. 3/1 1 sec. Voices 2
60 Impression Blocker Enchantment Enchantment Perm. 20/lb. – Enchant, Seeker, Scrywall 16
143 Increase Burden Regular/R-Spec Movement 10 min. 1/25 lbs.# 3 sec. Apportation 2
96 Independence Area Illusion Varies 2 Varies Simple Illusion 3
111 Infravision Regular Lt-Dk 1 min. 3/1 1 sec. Keen Vision or 5 Light spells 1
97 Initiative Area Illusion Indef.# Varies 10 sec. Independence, Wisdom 11
97 Inscribe Area/R-Will Illusion/Mk-Brk 1 min. 2/S 1 sec. Simple Illusion, Copy 8
48 Insignificance Regular/R-Spec. Comm. 1 hour 4/4 10 sec. Persuasion, Avoid 10
115 Inspired Creation* Regular Mk-Brk Perm. 5/day Varies – 0
92 Instant Neutralize Poison Regular Healing Instant 8 1 sec. M2, Neutralize Poison 5
93 Instant Regeneration* Regular Healing Perm. 80 Special M3, Regeneration 9
93 Instant Restoration* Regular Healing Perm. 50 Special M2, Restoration 7
114 Invisibility Regular Lt-Dk 1 min. 5/3 3 sec. 6 Light spells inc. Blur 6
174 Invisible Wizard Ear Regular Sound 1 min. 5/3 4 sec. Wizard Ear, Invisibility 15
104 Invisible Wizard Eye Regular Knowledge 1 min. 5/3 4 sec. Wizard Eye, Invisibility 10
169 Iron Arm Blocking Protection Instant 1 1 sec. Resist Pain, DX 11+ 8
181 Irradiate Area Tech 1 hr. 1/10 rads/hr./h 1 sec. 2 Earth spells, 2 Fire spells 4
35 Itch Regular/R-HT Body Scratch# 2 1 sec. – 0
143 Jump Regular Movement 1 min. 1 to 3 1 sec. Apportation 2
133 Keen Sense Regular Mind 30 min. 1 per +/H# 1 sec. – 0
117 Knot Regular Mk-Brk Indef.# 2 3 sec. Stiffen 9
97 Know Illusion Information Illusion Instant 2 1 sec. Simple Illusion 3
103 Know Location Information Knowledge Instant 2 10 sec. M1, Tell Position 3
78 Know Recipe Information/R-Spec. Food/Knowledge 1 day# 3 15 sec. Far-Tasting, Season 4
106 Know True Shape Information Knowledge Instant 2 1 sec. M1, any one shifting spell, 10
either Aura or Know Illusion
61 Leak Enchantment Enchantment Perm. 100 – Hideaway 22
89 Lend Energy Regular Healing Perm. Varies 1 sec. M1 or Empathy advantage 1
46 Lend Language Regular Comm. 1 min. 3/1 3 sec. 3 Communication spells, 3
or Beast Speech
180 Lend Power/TL Regular Tech Indef. Varies 1 sec. M2, Seek Power 3
47 Lend Skill Regular Comm. 1 min. 3/2 3 sec. Mind-Sending, IQ 11+ 7
126 Lend Spell Regular Meta-Spell Perm. Varies 3 sec. M1, Lend Skill, 1 spell each 14
From 6 colleges
89 Lend Vitality Regular Healing 1 hr. 1 per HP loaned 1 sec. Lend Energy 2
41 Lengthen Limb Regular Body 1 min. 2/2 5 sec. M3, Shapeshifting 10
140 Lesser Geas* Regular/R-Will Mind Perm. 12 30 sec. M2, 10 Mind Control spells 14
58 Lesser Wish* Enchantment Enchantment Special 180 – Enchant 13
143 Levitation Regular/R-ST or Will Movement 1 min. 1 per 80 lbs./H# 2 sec. Apportation 2
159 Lich* Enchantment Necro./Ench. Perm. Varies Varies M3, IQ 13+, Enchant, 28
Soul Jar, Zombie
110 Light Regular Lt-Dk 1 min. 1/1 1 sec. – 0
112 Light Jet Regular Lt-Dk 1 min. 2/1 1 sec. Continual Light or Shape Light 2
145 Light Tread Regular Movement 10 min. 4/1# 1 sec. Apportation, Shape Earth 4
67 Lighten Enchantment Enchantment Perm. Varies – Enchant 13
143 Lighten Burden Regular Movement 10 min. 3 or 5/H# 3 sec. Apportation 2
196 Lightning Missile Weather/Air Instant 1 to Magery# 1 to 3 sec. M1, 6 Air spells 7
198 Lightning Armor Regular Weather/Air 1 min. 7/4 1 sec. 6 Lightning spells inc. 13
Resist Lightning
198 Lightning Missiles Regular Weather/Air 1 min. 4/2# 3 sec. Lightning Weapon 10
198 Lightning Stare* Regular Weather/Air 1 sec. 1 to 4 2 sec. Lightning, Resist Lightning 9
198 Lightning Weapon Regular Weather/Air 1 min. 4/1 2 sec. M2, Lightning 9
196 Lightning Whip Regular Weather/Air 10 sec. 1 per 2 yards# 2 sec. Lightning 8
68 Limit Enchantment Enchantment Perm. 200 – Enchant 13
131 Link Area Meta-Spell Indef.# 8 4 hrs. Delay 19
144 Lockmaster Regular/R-Magelock Movement Perm. 3 10 sec. Locksmith or Apportation and M2 3
143 Locksmith Regular Movement 1 min. 2/2 1 sec. Apportation 2
143 Long March Regular/R-ST Movement 1 day’s march 3 1 min. M1, Clumsiness or Debility 2
63 Loyal Sword Enchantment Enchantment Perm. 750/lb.# – Enchant, Apportation 13
136 Loyalty Regular/R-Will Mind 1 hr. 2/2# 2 sec. Bravery, 2 other 5
Mind Control spells
137 Lure Area/R-Will Mind 1 hr. 1/S 10 sec. Emotion Control 6
176 Machine Control/TL Regular Tech 1 min. 6/3 1 sec. Reveal Function, Locksmith, 11
Lightning
178 Machine Possession/TL Regular/R-Will Tech 1 min. 6/2 30 sec. Machine Control, 16
 Rider Within or Soul Rider
176 Machine Speech/TL Regular Tech/Comm. 1 min. 5/3 1 sec. Machine Summoning 13
176 Machine Summoning/TL Regular Tech 1 min. 4/2 4 sec. Machine Control 12
136 Madness Regular/R-Will-2 Mind 1 min. 4/2 2 sec. Forgetfulness or Drunkenness 7
113 Mage Light Regular Lt-Dk 1 min. Varies 1 sec. Mage Sight, Light 4
102 Mage Sense Information Knowledge 1 min. 3/2 1 sec. Detect Magic 2
102 Mage Sight Regular Knowledge 1 min. 3/2 1 sec. Detect Magic 2
172 Mage-Stealth Regular Sound 1 min. 3/2 3 sec. Hush 3
166 Magelock Regular Protection 6 hrs. 3/2 4 sec. M1 1
123 Magic Resistance Regular/R-Will+M Meta-Spell 1 min. 1 to 5/S# 3 sec. M1, 1 spell each from 7 colleges 8
181 Magnetic Vision Regular Tech 1 min. 2/1 1 sec. Keen Vision 1
128 Maintain Spell* Special Meta-Spell Indef.# Varies 2 sec.# Link 20
91 Major Healing* Regular Healing Perm. 1 to 4 1 sec. M1, Minor Healing 4
60 Malefice* Enchantment Enchantment Indef.# 250 – Enchant, Seeker 16
177 Malfunction/TL Melee/R-HT Tech 1 min. 5 1 sec. M2, Glitch 14
70 Manastone* Enchantment Enchantment Indef. 5 – Enchant 13
145 Manipulate Regular Movement 1 min. 4/3# 3 sec. Locksmith 3
118 Mapmaker Special Mk-Brk 1 hr. 4/2 10 sec. Inscribe, Measurement 10
137 Mass Daze Area/R-HT Mind Instant 2/1# sec.=cost Daze, IQ 13+ 8
137 Mass Sleep Area/R-HT Mind Instant 3# sec.=cost Sleep, IQ 13+ 9
141 Mass Suggestion Area/R-Will Mind 10 min. 4/2# sec.=cost Suggestion 15
153 Mass Zombie* Area Necro. Perm. 7 varies# Zombie, Charisma 2+ 8
30 Master Reg./Block./R-IQ Animal Indef. 2 1 sec. Beast-Soother 1
150 Materialize Special Necro. 1 min. 5/5 1 sec. Summon Spirit 4
78 Mature Regular Food Perm. 1 per pound 10 sec. Decay or Season 2
100 Measurement Area/Inform. Knowledge Instant 1 1 sec. – 0
186 Melt Ice Area Water Perm.# 2# 10 sec. Heat or Freeze 4
105 Memorize Regular Knowledge/Mind 1 day# 3 2 sec. Wisdom or 6 Knowledge spells 6
135 Mental Stun Regular/R-Will Mind Instant 2 1 sec. Daze or Stun 4
174 Message Regular/R-spell Sound/Comm. Varies 1/15 sec. Varies Great Voice, Seeker 12
183 Metal Vision Regular Tech/Knowledge 30 sec. 2/5 yds./S 1 sec. Shape Metal 7
37 Might Regular Body 1 min. 2 per ST+/S 1 sec. Lend Energy 2
46 Mind-Reading Regular/R-Will Comm. 1 min. 4/2 10 sec. Truthsayer or Borrow Language 3
46 Mind-Search* Regular/R-Will Comm. 1 min. 6/3 1 min. Mind-Reading 4
47 Mind-Sending Regular Comm. 1 min. 4/4 4 sec. Mind-Reading 4
137 Mindlessness* Regular/R-Will Mind 1 min. 8/4 5 sec. M2, Forgetfulness 8
91 Minor Healing Regular Healing Perm. 1 to 3 1 sec. Lend Vitality 3
112 Mirror Regular Lt-Dk 1 min. 2/2 1 sec. Colors 2
168 Missile Shield Regular Protection 1 min. 5/2 1 sec. Apportation or Shield 3
79 Monk’s Banquet Regular Food 24 hrs. 6 1 sec. Fool’s Banquet, Resist Pain 15
55 Move Terrain* Area/R-Spec. Earth 1 hour 10/8 1 min. Alter Terrain, Hide Object 48
52 Mud Jet Regular Earth/Water 1 sec. 1 to 3 1 sec. Sand Jet and Create Water 9
or Create Earth and Water Jet
174 Musical Scribe Regular Sound 1 min. 3/1# 1 sec. Scribe 7
119 Mystic Mark Regular/R-Spec Mk-Brk Indef.# 3 10 sec. Dye, Trace 14
168 Mystic Mist Area Protection 10 hrs. 1/S 5 min. M1 and Watchdog or Shield 3
68 Name Enchantment Enchantment Perm. 200 or 400# – Enchant 13
38 Nauseate Regular/R-HT Body 10 sec. 2/S 1 sec. 2 Body spells inc. Perfume 2
92 Neutralize Poison Regular Healing Perm. 5 30 sec. Cure Disease or M3 and Test Food 4
111 Night Vision Regular Lt-Dk 1 min. 3/1 1 sec. Keen Vision or 5 Light spells 1
137 Nightingale Area Protection 10 hrs. 2/2 1 sec. Sense Danger 2
140 Nightmare Regular/R-Will Mind 1 hr. 6 1 min. M2, Death Vision, Fear, Sleep 13
24 No-Smell Regular Air 1 hr. 2/2 1 sec. Purify Air 1
173 Noise Area Sound 5 sec. 4/2 1 sec. Wall of Silence 3
138 Oath Regular/R-spec Mind Perm. 4 1 min. M1, Emotion Control 7
24 Odor Area Air 1 hr. 1 1 sec. No-Smell 2
70 One-College Powerstone Enchantment Enchantment Indef. 12 – Enchant 13
36 Pain Regular/R-HT Body 1 sec. 2 2 sec. Spasm 2
134 Panic Area/R-Will Mind 1 min. 4/2 1 sec. Fear 3
40 Paralyze Limb Melee/R-HT Body 1 min. 3 1 sec. M1, 5 Body Control spell 6
inc. Clumsiness
52 Partial Petrifaction* Regular/R-HT Earth Perm. 12 3 sec. M2, Flesh to Stone 6
34 Partial Shapeshifting* Regular/R-Will Animal 1 hour Varies 10 sec. M3, Shapeshift Others, 21
Alter Body
68 Password Enchantment Enchantment Perm. 400# – Enchant 13
105 Pathfinder Information Knowledge Instant 4 10 sec. M1, IQ 12+, 2 Seek spells 7
138 Peaceful Sleep Regular/R-Spec. Mind 8 hrs. 4 30 sec. Sleep, Silence 9
123 Penetrating Spell Regular Meta-Spell Varies Varies 3 sec. Delay, Find Weakness
63 Penetrating Weapon Enchantment Enchantment Perm. Varies – Enchant, Find Weakness 13
124 Pentagram Special Meta-Spell Perm. 1/sq. ft.# 1/sq. ft.# Spell Shield 12
96 Perfect Illusion Area Illusion 1 min. 3/H# 1 sec. M1, Complex Illusion 6
35 Perfume Regular/R-HT Body 10 min. 2/1 1 sec. Odor 1
32 Permanent Beast Possession* Regular/R-Will Animal Indef. 20 1 min. M2, Beast Possession 7
138 Permanent Forgetfulness* Regular/R-Will or skill Mind Perm. 15 1 hr. M2, Forgetfulness, IQ 13+ 10
178 Permanent Machine Possession/TL* Regular/R-Will Tech Indef.# 30 5 min. M3, Machine Possession 19
139 Permanent Madness* Regular/R-Will-2 Mind Perm. 20 10 min. M2, Madness, IQ 13+ 11
49 Permanent Possession* Regular/R-Will Comm. Indef. 30 5 min. M3, Possession 9
33 Permanent Shapeshifting* Regular Animal Indef. Varies 1 min. M3, Shapeshifting 10
45 Persuasion Regular/R-Will Comm. 1 min. 2¥bonus# 1 sec. Sense Emotion 2
154 Pestilence Regular Necro. Perm. 6 30 sec. M1, Steal Vitality, Decay 8
97 Phantom* Area Illusion 1 min. 5/H# 1 sec. M2, Perfect Illusion, 9
Hinder, Apportation
73 Phantom Flame Area Fire/Illusion 1 min. 1/S 1 sec. Shape Fire or Simple Illusion 2
83 Phase Blocking Gate Instant 3 1 sec. M3, Plane Shift or Ethereal Body 8
83 Phase Other* Blocking Gate Instant 3 1 sec. Phase 9
82 Planar Summons Special Gate 1 hr. 20# 5 min. M1, 1 spell each from 10 colleges 11
82 Planar Visit* Special Gate 1 min. 4/2 30 sec. M2, Projection or 10
Planar Summons
83 Plane Shift* Special Gate Instant 20 5 sec. Planar Summons 12
83 Plane Shift Other* Regular/R-Will+1 Gate Instant 20 5 sec. M3, Plane Shift 14
164 Plant Control Regular/R-Will Plant 1 min. 3/H 1 sec. Plant Sense 6
164 Plant Form Special Plant 1 hr. 5/2 1 sec. M1, 6 Plant spells 7
165 Plant Form Other Special/R-Will Plant 1 hr. 5/2 30 sec. M2, Plant Form 9
162 Plant Growth Area Plant 1 min. 3/2 10 sec. Heal Plant 3
163 Plant Sense Regular/R-Hide Path Plant 1 min. 3/2 1 sec. Forest Warning, Hide Path 5
164 Plant Speech Regular Plant 1 min. 3/2 1 sec. M1, Plant Sense 7
162 Plant Vision Regular Plant/Knowledge 30 sec. 1/10 yds. 1 sec. Shape Plant 3
183 Plastic Vision Regular Tech/Knowledge 30 sec. 2/5 yds./S 1 sec. Shape Plastic 5
78 Poison Food Regular Food Perm. 3 per meal 1 sec. Purify Food, Decay 3
162 Pollen Cloud Area/R-HT Plant 5 min.# 1 1 sec. Shape Plant 3
144 Poltergeist Missile/R-HT Movement Instant 1 or 2# 1 sec. Apportation 2
49 Possession* Regular/R-Will Comm. 1 min. 10/4 1 min. M1, and Control Person
or Beast Possession 6
57 Power Enchantment Enchantment Perm. Varies – Enchant, Recover Energy 13
69 Powerstone Enchantment Enchantment Perm. 20 – Enchant 13
51 Predict Earth Information Earth Instant 2 per day# Varies 4 Earth spells 4
Movement
193 Predict Weather Information Weather/Air Instant Varies 5 sec.# 4 Air spells 4
106 Prehistory Information Knowledge Instant Varies hr.=cost Ancient History 11
78 Prepare Game Regular Food Perm. 2 10 sec. Purify Food 3
48 Presence Regular/R-Spec. Comm. 1 hour 4/4 10 sec. Persuasion, Lure 7
79 Preserve Food Regular Food 1 week Special 1 sec. Decay 2
179 Preserve Fuel/TL Regular Tech 1 week 4/lb./H 1 sec. Test Fuel 1
105 Projection Regular Knowledge 1 min. 4/2 3 sec. Sense Spirit, 4 Knowledge spells 7
180 Propel/TL Regular Tech 10 min. Varies 1 sec. Create Fuel, Dancing Object 15
32 Protect Animal Area Animal/Protection 1 min. 1/S 1 min. Armor, Watchdog, 3 Animal spells 8
65 Puissance Enchantment Enchantment Perm. Varies – Enchant, 5 Earth spells 17
146 Pull Regular Movement 1 min. 1/2 ST/S 5 sec. M2, 4 Movement spells inc. 6
Levitation
23 Purify Air Area Air Instant 1 1 sec. – 0
54 Purify Earth Area Earth/Plant Perm. 2# 30 sec. Create Earth, Plant Growth 9
78 Purify Food Regular Food Perm. 1 per lb. 1 sec. Decay 2
179 Purify Fuel/TL Regular Tech Instant 1# 1 sec. Purify Water or Decay 2
184 Purify Water Special Water Perm. 1/gal. 5-10 sec./gal.# Seek Water 1
65 Quick-Aim Enchantment Enchantment Perm. Varies – Enchant, Grace 16
63 Quick-Draw Enchantment Enchantment Perm. 300/lb.# – Enchant, Apportation 13
144 Quick March Regular Movement 1 day’s march 4# 1 min. M1, Haste 2
182 Radiation Jet Regular Tech 1 sec. 1 to 3/S 1 sec. Irradiate, Resist Radiation 8
181 Radio Hearing Regular Tech 1 min. 2/1 1 sec. Keen Hearing 1
195 Rain Area Weather/Air/Water 1 hr. 1/10/S# 1 min. Clouds 5
191 Rain of Acid Area Water 1 min. 3/3 1 sec. M2, Create Water, Create Earth 10
74 Rain of Fire Area Fire 1 min. 1/S# 1 sec. M2, Create Fire 4
192 Rain of Ice Daggers Area Water 1 min. 2/2# 1 sec. M2, Hail, Ice Dagger 11
165 Rain of Nuts Area Plant 1 min. 1/10/S 1 sec. M1, 6 Plant spells inc. 7
Shape Plant
53 Rain of Stones Area Earth 1 min. 1/S# 1 sec. M2, Create Earth 6
82 Rapid Journey* Special Gate/Movement 1 min. Varies 5 sec. M3, Teleport or Timeport 8
134 Rear Vision Regular Mind 1 min. 3/1 1 sec. Alertness 3
177 Rebuild/TL Regular Tech/Mk-Brk Perm. Varies Varies M3, Repair, Create Object, 28
3 spells of each element#
106 Recall Regular Knowledge/Mind 1 day# 4 10 sec. M2, Memorize, Wisdom 10
106 Reconstruct Spell Information Knowledge Instant 3# 10 sec. M2, History, Identify Spell 13
89 Recover Energy Special Healing Special none Special M1, Lend Energy 2
122 Reflect Block/R-spell Meta-Spell Instant 4 or 6# none Ward 2
168 Reflect Gaze* Blocking/R-Spec Protection Instant 2 1 sec. Mirror 3
132 Reflex Special Meta-Spell 1 hr. Varies 10 sec. Delay, Ward 19
39 Reflexes Regular Body 1 min. 5/3 1 sec. Grace, Haste 5
93 Regeneration* Regular Healing Perm. 20 Special# Magery 2, Restoration 7
116 Rejoin Regular Mk-Brk 10 min. 1 per 10 lbs./H 4 sec./10 lbs. Weaken, Restore 7
163 Rejuvenate Plant Regular Plant Perm. 3 1 sec. M1, Plant Growth 5
92 Relieve Addiction Regular Healing 1 day 6 10 sec. Neutralize Poison 5
92 Relieve Madness Regular/R-spell Healing/Mind 10 min. 2 10 sec. Lend Vitality, Wisdom 10
93 Relieve Paralysis Regular Healing 1 min. Varies 10 sec. Stop Paralysis 6
90 Relieve Sickness Regular/R-spell Healing 10 min. 2 10 sec. Lend Vitality 3
107 Remember Path Regular Knowledge 1 hr. 3/1 10 sec. Find Direction, Memorize 10
127 Remove Aura Regular/R-Will# Meta-Spell Perm. 5 10 sec. Dispel Magic, Aura 15
90 Remove Contagion Area Healing Instant 3 2 sec. Decay, Clean, or Cure Disease 2
126 Remove Curse Regular/R-spell Meta-Spell Instant 20 1 hr. M2, Suspend Curse or 14
1 spell each from 15 colleges
58 Remove Enchantment Enchantment Perm. 100# varies Enchant 13
Enchantment
113 Remove Reflection Regular/R-Will Lt-Dk 1 min. 2/1 1 sec. Remove Shadow 2
110 Remove Shadow Regular/R-Will Lt-Dk 1 min. 2/1 1 sec. Light 1
118 Repair Regular Mk-Brk Perm. 2/5 lbs.# 1 sec./lb. M2, Rejoin 11
147 Repel Regular Movement 1 min. 1/2 ST/S 5 sec. M2, 4 Movement spells inc. 6
Levitation
31 Repel Animal Area/R-HT Animal 1 hour Varies 10 sec. 1 Control spell# 2
31 Repel Hybrids* Area/R-HT Animal 1 hour 6/3 10 sec. Hybrid Control 4
158 Repel Spirits Area/R-Will Necro. 1 hr. 4/H 10 sec. Banish, Turn Spirit 15
117 Reshape Regular Mk-Brk 1 min. 6/3 10 sec. M1, Weaken, Shape Earth 9
or Shape Plant
190 Resist Acid Regular Water/Protection 1 min. 2/H# 1 sec. Create Acid 9
74 Resist Cold Regular Fire 1 min. 2/1 1 sec. Heat 4
90 Resist Disease Regular Healing/Protection 1 hour 4/3 10 sec. Remove Contagion or Vigor 3
58 Resist Enchantment Enchantment Enchantment Perm. Varies – any Limiting Enchantment 14
74 Resist Fire Regular Fire 1 min. 2/1# 1 sec. Fireproof 3
196 Resist Lightning Regular Weather/Air/Protection 1 min. 2/1 1 sec. 6 Air spells 6
38 Resist Pain Regular Body 1 min. 4/2 1 sec. M2, Pain 5
91 Resist Poison Regular Healing/Protection 1 hr. 4/3 10 sec. Vigor 4
169 Resist Pressure Regular Protection 1 min. Varies 1 sec. Weather Dome
182 Resist Radiation Regular Tech/Protection 1 min. varies# 1 sec. 3 Radiation spells 7
173 Resist Sound Regular Sound/Protection 1 min. 2/1 1 sec. 4 Sound spells 4
186 Resist Water Regular Water/Protection 1 min. 2/1 1 sec. Umbrella, or Shape Water 4
and Destroy Water
93 Restoration* Regular Healing Perm. 15 1 min.# Major Healing, or any 2 of Relieve Paralysis and the Restore spells 5
116 Restore Regular Mk-Brk 10 min. 2/1 3 sec. Find Weakness or Simple Illusion 3
92 Restore Hearing Regular Healing 1 hr. Varies 5 sec. Minor Healing, Keen Hearing 5
or Strike Deaf
127 Restore Mana* Area Meta-Spell Perm. 10 1 hr. Dispel Magic, Suspend Mana 15
92 Restore Memory Regular Healing Perm. 3 10 sec. Awaken, IQ 11+ 6
92 Restore Sight Regular Healing 1 hr. Varies 5 sec. Minor Healing, Keen Vision 5
or Strike Blind
93 Restore Speech Regular Healing 1 hr. 5/3 5 sec. Minor Healing, Great Voice 8
or Strike Dumb
94 Resurrection* Regular Heal./Necro. Perm. 300 2 hrs. Instant Regeneration, 14
Summon Spirit
38 Retch Regular/R-HT Body Instant 3 4 sec. Nauseate, Spasm 3
47 Retrogression Regular/R-Will Comm. 1 sec. 5 10 sec. Mind-Search, Mind-Sending 6
168 Return Missile Blocking Protection Instant 2 1 sec. Catch Missile 4
176 Reveal Function/TL Information/R-spell Tech Instant 8 10 min. Seek Machine 1
168 Reverse Missiles Regular Protection 1 min. 7/3 1 sec. Missile Shield or Force Dome 3
31 Rider Regular Animal 5 min. 2/1 1 sec. 1 Control spell# 2
31 Rider Within Regular Animal 1 min. 4/1 3 sec. 2 Control spells# 3
117 Rive* Regular Mk-Brk Instant 1 per die 1 sec. M2, Shatter 9
36 Rooted Feet Regular/R-ST Body 1 min.# 3 1 sec. Hinder 2
154 Rotting Death* Regular/R-HT Necro. 1 sec. 3/2 3 sec. M2, Sickness, Pestilence 10
36 Roundabout Regular/R-HT Body Instant 3 1 sec. Tanglefoot 4
118 Ruin Regular Mk-Brk 1 min.# 2 per lb./S 5 sec./lb. M1, Weaken, Decay 9
86 Sanctuary* Special Gate 1 hr. 5/S 10 sec. Hide Object 30
52 Sand Jet Regular Earth 1 sec. 1 to 3/S 1 sec. Create Earth 5
27 Sandstorm Area Air/Earth 1 minute# 3/H Instant# Windstorm, Create Earth 9
107 Scents of the Past Regular Knowledge/Food 1 min. 1/1# 10 sec. M2, History, Odor 14
177 Schematic/TL Information Tech/Knowledge 1 min. 5/H# 5 sec. Reveal Function, History 12
174 Scribe Regular Sound 1 min. 3/1 1 sec. Voices, Dancing Object, 6
1 Accented language
57 Scroll Enchantment Enchantment Varies Special days=cost M1, 1 language at Accented 1
85 Scry Gate Regular Gate 1 min. 4/4 10 sec. Seek Gate 15
123 Scryfool Regular/R-Spec Meta-Spell 10 hrs. 4/2 10 sec. M2, Sense Observation, 8
Simple Illusion
121 Scryguard Regular Meta-Spell 10 hrs. 3/1 5 sec. M1 1
122 Scrywall Area Meta-Spell 10 hrs. 3/2 sec.=cost Scryguard 2
77 Season Regular Food Perm. 2/meal 10 sec. Test Food 1
113 See Invisible Regular Lt-Dk 1 min. 4/2 1 sec. Invisibility, or Dark Vision 3
and Infravision
181 See Radiation Regular Tech 1 min. 3/2 1 sec. – 0
107 See Secrets Regular Knowledge 1 min. 5/2 5 sec. Seeker, Aura 10
23 Seek Air Information Air Instant 1 1 sec. – 0
184 Seek Coastline Information Water Instant 3 10 sec. Seek Water 1
50 Seek Earth Information Earth Instant 3 10 sec. – 0
72 Seek Fire Information Fire Instant 1 1 sec. – 0
77 Seek Food Information Food Instant 2 1 sec. – 0
179 Seek Fuel/TL Information Tech Instant 3 10 sec. – 0
85 Seek Gate Information Gate Instant 3 10 sec. M2, Seek Magic, 1 spell each 14
from 10 colleges
175 Seek Machine/TL Information Tech Instant 3 10 sec. – 0
102 Seek Magic Information Knowledge/Meta-Spell Instant 6 10 sec. Detect Magic 2
51 Seek Pass Information Earth Instant 3 10 sec. Seek Earth 1
161 Seek Plant Information Plant Instant 2 1 sec. – 0
182 Seek Plastic Information Tech Instant 3 10 sec. – 0
179 Seek Power/TL Information Tech Instant 3 10 sec. – 0
181 Seek Radiation Regular Tech Instant 3 10 sec. See Radiation 1
184 Seek Water Information Water Instant 2 1 sec. – 0
105 Seeker Information Knowledge Instant 3 1 sec. M1, IQ 12+, 2 Seek spells 7
166 Sense Danger Information Protection Instant 3 1 sec. Sense Foes or Danger Sense 1
45 Sense Emotion Regular Comm. Instant 2 1 sec. Sense Foes 1
44 Sense Foes Inform./Area Comm. Instant 2# 1 sec. – 0
45 Sense Life Inform./Area Comm. Instant 1# 1 sec. – 0
101 Sense Mana Information Knowledge Instant 3 5 sec. Detect Magic 2
167 Sense Observation Area Protection 1 hr. 1/H# 5 sec. Sense Danger or Scryguard 2
149 Sense Spirit Inform./Area Necro. Instant 1# 1 sec. Death Vision, or Sense Life and M1 2
39 Sensitize Regular/R-HT Body 1 min. 3/2 1 sec. M1, Stun 5
169 Shade Regular Protection/Lt-Dk 1 hr. 1/H 10 sec. Continual Light or Shield 3
24 Shape Air Regular Air 1 min. 1 to 10# 1 sec. Create Air 2
113 Shape Darkness Area Lt-Dk 1 min. 2/S# 1 sec. Darkness 350
50 Shape Earth Regular Earth 1 min. 1/25 cu. ft./h 1 sec. Seek Earth 1
72 Shape Fire Area Fire 1 min. 2/H 1 sec. Ignite Fire 1
111 Shape Light Regular Lt-Dk 1 min. 2/2 1 sec. Light 1
182 Shape Metal Regular Tech 1 min. 6/H# 1 sec. M1, Shape Earth or 6 Tech spells 6
161 Shape Plant Regular Plant 1 min. 3/1# 10 sec. Identify Plant 2
183 Shape Plastic Regular Tech 1 min. 6/3 1 sec. M1, Shape Plant or 6 Tech spells 4
185 Shape Water Regular Water 1 min. 1/1# 2 sec. Create Water 3
33 Shapeshift Others* Special/R-Will Animal 1 hour Varies 30 sec. M2, Shapeshifting for that form 9
32 Shapeshifting* Special Animal 1 hour Varies 3 sec. M1, 6 other spells 7
89 Share Energy Regular Healing Special Varies 1 sec. Lend Energy 2
90 Share Vitality Regular Healing Perm. 0# 1 sec./HP Lend Vitality 3
118 Sharpen Regular Mk-Brk 1 min. Varies 4 sec. Repair 12
116 Shatter* Regular Mk-Brk Instant 1 to 3 1 sec. M1, Weaken 7
118 Shatterproof Regular Mk-Brk 1 hr. 3/3 1 sec. Repair, Shatter 11
167 Shield Regular Protection 1 min. Varies 1 sec. M2 2
196 Shocking Touch Melee Weather/Air Instant 1 to 3 1 sec. Lightning 8
42 Shrink* Regular Body 1 hr. 2/-1 SM/S 5 sec. M2, Alter Body 18
120 Shrink Object* Regular Mk-Brk 1 hour Varies 3 sec. Contract Object 18
42 Shrink Other* Regular/R-HT Body 1 hr. 2/-1 SM/S 10 sec. M3, Shrink 20
138 Sickness Regular/R-HT Mind/Body 1 min. 3/3 4 sec. Drunkenness or Pestilence 9
171 Silence Area Sound 1 min. 2/1 1 sec. Sound 1
174 Silver Tongue Regular Sound 1 min. 3/2 1 sec. Voices, Emotion Control 11
95 Simple Illusion Area Illusion 1 min. 1/H 1 sec. not blind, IQ 11+ 2
61 Simulacrum* Enchantment Enchantment Perm.# 2¥golem – M3, Golem, Perfect Illusion, 23
Illusion Disguise
151 Skull-Spirit Regular Necro. 24 hrs. 20 1 sec. 4 other Necromantic spells 6
135 Sleep Regular/R-HT Mind Instant 4 3 sec. Daze 6
145 Slide Regular/R-Will Movement 1 min. 2/2 1 sec. Apportation, Grease 4
145 Slow Regular/R-HT Movement 10 sec. 5/4 3 sec. M1, Haste, Hinder 3
144 Slow Fall Regular Movement 1 min. 1 per 50 lbs./H 1 sec. Apportation 2
73 Slow Fire Regular Fire 1 min. Varies 1 sec. Extinguish Fire 2
153 Slow Healing Regular/R-HT Necro. 1 day 1 to 5/S 10 sec. M1, Frailty, Steal Vitality 7
86 Slow Time* Area/R-Spec. Gate 1 min. Varies 2 sec. M2, IQ 13+, 2 spells each 28
from 10 colleges
111 Small Vision Regular Lt-Dk/Knowledge 1 min. 4/2# 2 sec. Keen Vision or 5 Light spells; 1
no Blindness or Bad Sight
73 Smoke Area Fire 5 min.# 1/H 1 sec. Shape Fire, Extinguish Fire 3
195 Snow Area Weather/Air/Water 1 hr. 1/15#/S 1 sec. Clouds, Frost 7
189 Snow Jet Regular Water 1 sec. 1 to 3 1 sec. Water Jet, Freeze 6
186 Snow Shoes Regular Water 1 min. 2/1 2 sec. Shape Water 4
116 Soilproof Regular Mk-Brk 10 min. 2/1 2 sec. Clean 5
151 Solidify Special Necro. 1 min. 50/10 1 sec. Materialize 5
154 Soul Jar* Regular Necro. Perm. 8 1 min. M1, 6 Necro. spells inc. 10
Steal Vitality
49 Soul Rider Regular/R-Will Comm. 1 min. 5/2 3 sec. Mind-Reading 4
71 Soul Stone* Enchantment Enchantment Perm. 500 – M3, Enchant, Soul Jar 20
171 Sound Regular Sound Varies Varies 1 sec. – 0
173 Sound Jet Regular Sound 1 sec. 1 to 4/S 1 sec. Great Voice 4
171 Sound Vision Regular Sound 1 min. 5/2 1 sec. Keen Hearing or Acute Hearing 1
196 Spark Cloud Area Weather/Air 10 sec. 1 to 5/S 1 to 5 sec. Shape Air, Lightning 8
197 Spark Storm Area Weather/Air 1 min.# 2, 4, or 6/H Instant# Windstorm, Lightning 8
35 Spasm Regular/R-HT Body Instant 2 1 sec. Itch 1
181 Spectrum Vision* Regular Tech 1 min. 4/4 1 sec. Infravision 2
57 Speed Enchantment Enchantment Perm. Varies – Enchant, Haste 13
66 Speed Spell Arrow Enchantment Enchantment Perm. Varies – Speed, Spell Arrow 23
65 Spell Arrow Enchantment Enchantment Perm. 30¥spell cost – Spell Stone 20
124 Spell Shield Area Meta-Spell 1 min. 3/2 1 sec. M2, Scryguard, Magic Resistance 11
60 Spell Stone Enchantment Enchantment Varies 20¥spell cost – Enchant, Delay 19
124 Spell Wall Regular/R-spell Meta-Spell 1 min. 2/2# 1 sec. Spell Shield 12
127 Spellguard* Regular/R-Spec Meta-Spell 10 hrs. 1 to 3/S# Varies Dispel Magic 15
32 Spider Silk Missile Animal 1 min. 1/5 yds.# 1 sec. M1, 2 Animal spells 3
192 Spit Acid* Regular Water 1 sec. 1 to 4 2 sec. M3, Acid Jet, Resist Acid 14
70 Staff Enchantment Enchantment Perm. 30 – Enchant 13
158 Steal Attribute* Regular/R-Spec Necro. 1 day Varies 1 min. Varies
159 Steal Beauty* Regular Necro. 24 hrs. Varies 30 sec. M3, Alter Visage, Steal Vitality 23
150 Steal Energy Regular Necro. Perm. none# 1 min/3 FP-# Minor Healing 4
180 Steal Power/TL* Regular Tech 1 min. 0# 1 sec. M2, Minor Healing, 8
Conduct Power
158 Steal Skill* Regular/R-Will Necro. 24 hrs. Varies 1 min. M3, Borrow Skill, Daze 16
127 Steal Spell* Regular/R-spec Meta-Spell Perm. Varies 5 sec. Lend Spell, Great Ward 16
150 Steal Vitality Regular Necro. Perm. none# 1 min./3 HP-# Steal Energy 5
158 Steal Youth* Regular/R-HT Necro. Perm. 10 to 30 1 hr. Youth, Age, Steal Vitality 16
191 Steam Jet Regular Water 1 sec. 1 to 3 1 sec. Water Jet, Boil Water 10
54 Steelwraith Regular/R-HT Earth 1 min. 7/4 2 sec. M2, Walk Through Earth 8
24 Stench Area Air 5 min. 1 1 sec. Purify Air 1
117 Stiffen Regular/R-Spec. Mk-Brk 10 min. 1 per lb./H# 2 sec./lb. Rejoin 8
52 Stone Missile Missile Earth Instant 1 to Magery 1 to 3 sec. Create Earth 5
51 Stone to Earth Regular Earth Perm. 6/25 cu. ft. 1 sec. Earth to Stone or any 4
4 Earth spells
53 Stone to Flesh Regular Earth Perm. 10 5 sec. M2, Stone to Earth, Flesh to Stone 7
91 Stop Bleeding Regular Healing Perm. 1 or 10 1 sec. Lend Vitality 3
153 Stop Healing Regular Necro. Indef.# 10 10 sec. Slow Healing 8
93 Stop Paralysis Regular Healing Perm. 1 or 2 1 sec. Major Healing, or Minor Healing 5
and Paralyze Limb
179 Stop Power Area Tech 1 min. 3/H 3 sec. M1, Seek Power 2
35 Stop Spasm Regular Body/Healing Instant 1 1 sec. Spasm or Lend Vitality 2
195 Storm Area Weather/Air/Water 1 hour 1/50/S 1 min. Rain, Hail 10
136 Strengthen Will Regular Mind 1 min. 1/pt/H 1 sec. M1, 6 Mind spells 7
41 Strike Barren Regular/R-HT Body/Necro. Perm. 5 30 sec. M1, Steal Vitality, Decay 8
38 Strike Blind Regular/R-HT Body 10 sec. 4/2 1 sec. 2 Light spells, Spasm 3
38 Strike Deaf Regular/R-HT Body 10 sec. 3/1 1 sec. 2 Sound spells, Spasm 3
38 Strike Dumb Regular/R-HT Body 10 sec. 3/1 1 sec. Spasm 2
40 Strike Numb Regular/R-HT Body 10 sec. 3/1 1 sec. Resist Pain 6
37 Stun Regular/R-HT Body Instant 2 1 sec. Pain 3
140 Suggestion Regular/R-Will Mind 10 min. 4/3 10 sec. Emotion Control, Forgetfulness 14
155 Summon Demon Special Necro. 1 hr.# 20# 5 min. M1, 1 spell each from 10 colleges 11
27 Summon Elemental Special 4 Diff. 1 hr. 4# 30 sec. M, # 9
102 Summon Shade* Inform./R-Will Knowledge 1 min. 50/20 10 min. Summon Spirit or Divination 4
150 Summon Spirit Inform./R-Will Necro. 1 min. 20/10# 5 min. Death Vision, M2 3
114 Sunbolt Missile Lt-Dk Instant 1 to Magery# 1 to 3 sec. 6 Light spells inc. Sunlight 7
114 Sunlight Area Lt-Dk 1 min. 2/1 1 sec. M1, Glow, Colors 5
125 Suspend Curse Regular/R-spell Meta-Spell 10 min. 10/10 1 min. M1, 1 spell each From 12 colleges 13
58 Suspend Enchantment Enchantment 1 hr. 25# 1 sec. Enchant 13
Enchantment
130 Suspend Magery* Regular/R-Will+M Meta-Spell 1 hr. 12/12# 10 sec. M2, 2 spells each from 10 colleges#22
123 Suspend Magic Area/R-spell Meta-Spell 1 min. 3/2 sec.=cost Suspend Spell, 8 other spells 10
125 Suspend Mana* Area Meta-Spell Varies 5 10 min. Suspend Magic, 1 spell each 13
From 10 colleges
121 Suspend Spell Regular/R-spell Meta-Spell 1 min. Varies 1 sec. M1 1
86 Suspend Time* Area/R-Spec. Gate 1 day 5/5 5 min. M3, Slow Time 30
94 Suspended Animation Regular/R-HT Healing Indef.# 6 30 sec. Sleep, 4 Healing spells 12
147 Swim Regular Water/Move 1 min. 6/3 3 sec. Shape Water, Levitation 7
58 Talisman Enchantment Enchantment Perm. Varies – Enchant, spell to be opposed 13
162 Tangle Growth Area Plant 1 min. 1 or 2#/H 2 sec. Plant Growth 4
36 Tanglefoot Regular/R-DX Body Instant 2 1 sec. Clumsiness 3
128 Telecast* Special Meta-Spell 1 min. Varies 1 min. M3, Teleport, Wizard Eye, 17
1 spell each From 10 colleges
47 Telepathy* Regular Comm. 1 min. 4/4# 4 sec. Mind-Sending 5
147 Teleport* Special Movement/Gate Instant Varies 1 sec. Hawk Flight or IQ 13+ and 6
1 spell each from 10 colleges
147 Teleport Other* Regular/R-Will+1 Movement/Gate Instant Varies 1 sec. M3, Teleport 8
170 Teleport Shield Area Protection/Gate 1 hr. 1/S# 10 sec. Watchdog, either 10
Spell Shield or Teleport
101 Tell Position Information Knowledge Instant 1 1 sec. Measurement 1
100 Tell Time Information Knowledge Instant 1 1 sec. – 0
56 Temporary Enchantment Enchantment Enchantment Indef.# Varies – Enchant 13
134 Terror Area/R-Will Mind Instant 4 1 sec. Fear 3
77 Test Food Information Food Instant 1 to 3# 1 sec. – 0
179 Test Fuel/TL Information Tech Instant Varies 1 sec. – 0
101 Test Load Area/Inform. Knowledge Instant 2# 1 sec. Measurement 1
38 Thirst Regular/R-HT Body/Food Instant 5 10 sec. M1, Debility, Destroy Water 6
128 Throw Spell* Missile/Special Meta-Spell Indef.# 3 1 sec. Delay, Catch Spell 20
171 Thunderclap Regular Sound Instant 2 1 sec. Sound 1
36 Tickle Regular/R-Will Body 1 min. 5/5 1 sec. Spasm 2
194 Tide Special; Area Weather/Water 1 hr. 1/30/S 1 min. 8 Water spells 8
87 Time Out* Area Gate Instant# 5 5 min. M3, Accelerate Time 30
81 Timeport* Special Gate Instant Varies 1 sec. M3, Teleport 8
81 Timeport Other* Regular/R-Will+1 Gate Instant Varies 1 sec. Timeport 9
81 Timeslip Blocking Gate Instant 1/sec.# 1 sec. Timeport 9
81 Timeslip Other* Blocking Gate Instant 1/sec.# 1 sec. Timeslip 10
40 Total Paralysis Melee/R-HT Body 1 min. 5 1 sec. Paralyze Limb 7
35 Touch Regular Body Instant 1 1 sec. – 0
119 Toughen Regular Mk-Brk 1 hr. Varies 5 sec. Shatterproof 12
106 Trace Regular Knowledge 1 hr. 3/1 1 min. Seeker 8
84 Trace Teleport Information/R-spell Gate/Movement Instant 3 1 sec. Teleport, Timeport, or Plane Shift 7
43 Transform Body Special Body 1 hr. Varies 1 min. 3 forms of Shapeshifting, 18
Alter Body
120 Transform Object* Regular/R-Spec Mk-Brk 1 hour Varies Varies M2, Reshape, 4 Create spells 15
43 Transform Other Special/R-Will Body 1 hr. Varies 2 min. Shapeshift Others, 20
Transform Body#
43 Transmogrification Regular/R-Will Body 1 hr. 20/20 2 min. M3, Transform Other, 27
Transform Object, Flesh to Stone
119 Transparency Regular Mk-Brk 1 min. 4/2 10 sec. Dye, Stone to Earth 12
45 Truthsayer Inform./R-Will Comm. Instant 2 1 sec. Sense Emotion 2
167 Turn Blade Blocking/R-DX Protection Instant 1 1 sec. Apportation or Spasm 2
151 Turn Spirit Regular/R-Will Necro. 10 sec. 4/2# 1 sec. Fear, Sense Spirit 6
152 Turn Zombie Area Necro. 1 day 2 4 sec. Zombie# 7
185 Umbrella Regular Water/Protection 10 min. 1/1 2 sec. Shape Water or Shield 3
145 Undo Regular/R-Spec. Movement Instant 3 or 6# 1 sec. Locksmith 3
170 Utter Dome Area Protection 1 min. 6/4 1 sec. M2, Force Dome, Spell Shield 16
170 Utter Wall Regular Protection 1 min. 4/yd./S 1 sec. Utter Dome, Spell Wall 18
45 Vexation Regular/R-Will Comm. 1 min. 2¥penalty# 1 sec. Sense Emotion 2
138 Vigil* Regular Mind 1 night 8 1 sec. M2, Sleep, Lend Energy 10
37 Vigor Regular Body 1 min. 2 per HT+/S# 1 sec. Lend Vitality or Frailty 3
172 Voices Regular Sound 1 min. 3/2 1 sec. Sound 1
54 Volcano Regular Earth 1 day 15/10 1 hr.# Earthquake, 6 Fire spells 14
25 Walk on Air Regular Air 1 min. 3/2 1 sec. Shape Air 3
186 Walk on Water Regular Water 1 min. 3/2 4 sec. Shape Water 4
52 Walk Through Earth Regular Earth 10 sec. 3/3# 1 sec. 4 Earth spells 5
163 Walk Through Plants Regular Plant 1 min. 3/1 1 sec. Hide Path, Shape Plant 5
188 Walk Through Water Regular Water 1 sec. 4/3 3 sec. M1, Shape Water 5
164 Walk Through Wood Regular Plant 1 sec. 3/2 1 sec. Walk Through Plants 6
113 Wall of Light Area Lt-Dk 1 min. 1 to 3/S 1 sec. Continual Light 2
197 Wall of Lightning Regular Weather/Air 1 min. 2 to 6/S 1 sec. Lightning 8
172 Wall of Silence Area Sound 1 min. 2/1 1 sec. Silence 2
25 Wall of Wind Area Air 1 min. 2/H Instant# Shape Air 3
144 Wallwalker Regular Movement 1 min. 1 per 50 lbs./H# 1 sec. Apportation 2
122 Ward Block/R-spell Meta-Spell Instant 2 or 3# none M1 1
195 Warm Area Weather/Air 1 hour 1/10/S 1 min.# Heat, 4 Air spells 8
74 Warmth Regular Fire/Protection 1 hour 2/1 10 sec. Heat 4
167 Watchdog Area Protection 10 hrs. 1/1 10 sec. Sense Danger 2
187 Water Jet Regular Water 1 sec. 1 to 3 1 sec. Shape Water 4
79 Water to Wine Regular Food Perm. 4 per gal.# 10 sec. Purify Water, Mature 5
187 Water Vision Information Water/Knowledge 30 sec. 1/1# 1 sec. Shape Water 4
194 Waves Special; Area Weather/Water 1 hr. 1/60/S 1 min. Shape Water 4
116 Weaken Regular Mk-Brk Perm. 2 to 6 5 sec. Find Weakness 5
40 Weaken Blood Regular/R-HT Body/Necro. 1 day 9/5 1 sec. Sickness or Steal Vitality 6
136 Weaken Will Regular/R-Will Mind 1 min. 2/pt/H 1 sec. M1, Foolishness 6
119 Weapon Self* Regular/R-HT# Mk-Brk 1 min. 8/4 5 sec. M2, Apportation, 14
6 Mk-Brk spells inc. Reshape
64 Weapon Spirit* Enchantment Enchantment Perm. Varies – Enchant, Summon Spirit 14
169 Weather Dome Area Protection/Weather 6 hrs. 3/2 1 sec. 2 spells each from 4 elements 8
187 Whirlpool Area Water 1 min.# 2/H Varies Shape Water 4
138 Will Lock Area/R-(ST+Will)/2 Mind 1 day 3 Varies Emotion Control 6
195 Wind Special; Area Weather/Air 1 hour 1/50/S 1 min. Windstorm 4
25 Windstorm Area Air 1 min.# 2/H Instant# Shape Air 3
145 Winged Knife Missile Movement Instant 1 per lb.# 1 sec. Poltergeist 3
135 Wisdom Regular Mind 1 min. 4 per IQ+/S 1 sec. 6 other Mind Control spells 6
61 Wish* Enchantment Enchantment Special 250 – Lesser Wish, 1 spell each 19
from 15 colleges
40 Wither Limb Melee/R-HT Body Perm. 5 1 sec. M2, Paralyze Limb 8
163 Wither Plant Area/R-HT Plant Perm. 2 10 sec. Blight 5
174 Wizard Ear Regular Sound 1 min. 4/3 2 sec. Apportation, Far-Hearing, 7
Sound Vision
104 Wizard Eye Regular Knowledge 1 min. 4/2 2 sec. Apportation, Keen Vision 3
104 Wizard Hand Regular Knowledge/Movement 1 min. Varies 3 sec. Manipulate, Far-Feeling 5
104 Wizard Mouth Regular Knowledge/Food/Sound 1 min. 4/2 2 sec. Apportation, Far-Tasting, 8
Great Voice
104 Wizard Nose Regular Knowledge/Food 1 min. 3/2 2 sec. Apportation, Far-Tasting 4
160 Wraith* Enchantment/R-HT Necro./Ench. Perm. 250 or 500# Varies M3, IQ 13+, Enchant, 34
Halt Aging, Soul Jar
94 Youth* Regular Healing Special 100 1 sec. M3, Halt Aging 13
151 Zombie Regular Necro. Perm. 8 1 min. Summon Spirit, Lend Vitality 6
153 Zombie Summoning Special Necro. 1 min. 5/2# 4 sec. Zombie 7
"""
