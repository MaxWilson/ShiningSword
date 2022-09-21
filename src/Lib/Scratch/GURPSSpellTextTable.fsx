
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
153 Zombie Summoning Special Necro. 1 min. 5/2# 4 sec. Zombie
151 Zombie Regular Necro. Perm. 8 1 min. Summon Spirit, Lend Vitality
94 Youth* Regular Healing Special 100 1 sec. M3, Halt Aging
160 Wraith* Enchantment/R-HT Necro./Ench. Perm. 250 or 500# Varies M3, IQ 13+, Enchant, Halt Aging, Soul Jar
104 Wizard Nose Regular Knowledge/Food 1 min. 3/2 2 sec. Apportation, Far-Tasting
104 Wizard Mouth Regular Knowledge/Food/Sound 1 min. 4/2 2 sec. Apportation, Far-Tasting, Great Voice
104 Wizard Hand Regular Knowledge/Movement 1 min. Varies 3 sec. Manipulate, Far-Feeling
104 Wizard Eye Regular Knowledge 1 min. 4/2 2 sec. Apportation, Keen Vision
174 Wizard Ear Regular Sound 1 min. 4/3 2 sec. Apportation, Far-Hearing, Sound Vision
163 Wither Plant Area/R-HT Plant Perm. 2 10 sec. Blight
40 Wither Limb Melee/R-HT Body Perm. 5 1 sec. M2, Paralyze Limb
61 Wish* Enchantment Enchantment Special 250 – Lesser Wish, 1 spell each from 15 colleges
135 Wisdom Regular Mind 1 min. 4 per IQ+/S 1 sec. 6 other Mind Control spells
145 Winged Knife Missile Movement Instant 1 per lb.# 1 sec. Poltergeist
25 Windstorm Area Air 1 min.# 2/H Instant# Shape Air
195 Wind Special; Area Weather/Air 1 hour 1/50/S 1 min. Windstorm
138 Will Lock Area/R-(ST+Will)/2 Mind 1 day 3 Varies Emotion Control
187 Whirlpool Area Water 1 min.# 2/H Varies Shape Water
169 Weather Dome Area Protection/Weather 6 hrs. 3/2 1 sec. 2 spells each from 4 elements
64 Weapon Spirit* Enchantment Enchantment Perm. Varies – Enchant, Summon Spirit
119 Weapon Self* Regular/R-HT# Mk-Brk 1 min. 8/4 5 sec. M2, Apportation, 6 Mk-Brk spells inc. Reshape
136 Weaken Will Regular/R-Will Mind 1 min. 2/pt/H 1 sec. M1, Foolishness
40 Weaken Blood Regular/R-HT Body/Necro. 1 day 9/5 1 sec. Sickness or Steal Vitality
116 Weaken Regular Mk-Brk Perm. 2 to 6 5 sec. Find Weakness
194 Waves Special; Area Weather/Water 1 hr. 1/60/S 1 min. Shape Water
187 Water Vision Information Water/Knowledge 30 sec. 1/1# 1 sec. Shape Water
79 Water to Wine Regular Food Perm. 4 per gal.# 10 sec. Purify Water, Mature
187 Water Jet Regular Water 1 sec. 1 to 3 1 sec. Shape Water
167 Watchdog Area Protection 10 hrs. 1/1 10 sec. Sense Danger
74 Warmth Regular Fire/Protection 1 hour 2/1 10 sec. Heat
195 Warm Area Weather/Air 1 hour 1/10/S 1 min.# Heat, 4 Air spells
122 Ward Block/R-spell Meta-Spell Instant 2 or 3# none M1
144 Wallwalker Regular Movement 1 min. 1 per 50 lbs./H# 1 sec. Apportation
25 Wall of Wind Area Air 1 min. 2/H Instant# Shape Air
172 Wall of Silence Area Sound 1 min. 2/1 1 sec. Silence
197 Wall of Lightning Regular Weather/Air 1 min. 2 to 6/S 1 sec. Lightning
113 Wall of Light Area Lt-Dk 1 min. 1 to 3/S 1 sec. Continual Light
164 Walk Through Wood Regular Plant 1 sec. 3/2 1 sec. Walk Through Plants
188 Walk Through Water Regular Water 1 sec. 4/3 3 sec. M1, Shape Water
163 Walk Through Plants Regular Plant 1 min. 3/1 1 sec. Hide Path, Shape Plant
52 Walk Through Earth Regular Earth 10 sec. 3/3# 1 sec. 4 Earth spells
186 Walk on Water Regular Water 1 min. 3/2 4 sec. Shape Water
25 Walk on Air Regular Air 1 min. 3/2 1 sec. Shape Air
54 Volcano Regular Earth 1 day 15/10 1 hr.# Earthquake, 6 Fire spells
172 Voices Regular Sound 1 min. 3/2 1 sec. Sound
37 Vigor Regular Body 1 min. 2 per HT+/S# 1 sec. Lend Vitality or Frailty
138 Vigil* Regular Mind 1 night 8 1 sec. M2, Sleep, Lend Energy
45 Vexation Regular/R-Will Comm. 1 min. 2¥penalty# 1 sec. Sense Emotion
170 Utter Wall Regular Protection 1 min. 4/yd./S 1 sec. Utter Dome, Spell Wall
170 Utter Dome Area Protection 1 min. 6/4 1 sec. M2, Force Dome, Spell Shield
145 Undo Regular/R-Spec. Movement Instant 3 or 6# 1 sec. Locksmith
185 Umbrella Regular Water/Protection 10 min. 1/1 2 sec. Shape Water or Shield
152 Turn Zombie Area Necro. 1 day 2 4 sec. Zombie#
151 Turn Spirit Regular/R-Will Necro. 10 sec. 4/2# 1 sec. Fear, Sense Spirit
167 Turn Blade Blocking/R-DX Protection Instant 1 1 sec. Apportation or Spasm
45 Truthsayer Inform./R-Will Comm. Instant 2 1 sec. Sense Emotion
119 Transparency Regular Mk-Brk 1 min. 4/2 10 sec. Dye, Stone to Earth
43 Transmogrification Regular/R-Will Body 1 hr. 20/20 2 min. M3, Transform Other, Transform Object, Flesh to Stone
43 Transform Other Special/R-Will Body 1 hr. Varies 2 min. Shapeshift Others, Transform Body#
120 Transform Object* Regular/R-Spec Mk-Brk 1 hour Varies Varies M2, Reshape, 4 Create spells
43 Transform Body Special Body 1 hr. Varies 1 min. 3 forms of Shapeshifting, Alter Body
84 Trace Teleport Information/R-spell Gate/Movement Instant 3 1 sec. Teleport, Timeport, or Plane Shift
106 Trace Regular Knowledge 1 hr. 3/1 1 min. Seeker
119 Toughen Regular Mk-Brk 1 hr. Varies 5 sec. Shatterproof
35 Touch Regular Body Instant 1 1 sec. –
40 Total Paralysis Melee/R-HT Body 1 min. 5 1 sec. Paralyze Limb
81 Timeslip Other* Blocking Gate Instant 1/sec.# 1 sec. Timeslip
81 Timeslip Blocking Gate Instant 1/sec.# 1 sec. Timeport
81 Timeport Other* Regular/R-Will+1 Gate Instant Varies 1 sec. Timeport
81 Timeport* Special Gate Instant Varies 1 sec. M3, Teleport
87 Time Out* Area Gate Instant# 5 5 min. M3, Accelerate Time
194 Tide Special; Area Weather/Water 1 hr. 1/30/S 1 min. 8 Water spells
36 Tickle Regular/R-Will Body 1 min. 5/5 1 sec. Spasm
171 Thunderclap Regular Sound Instant 2 1 sec. Sound
128 Throw Spell* Missile/Special Meta-Spell Indef.# 3 1 sec. Delay, Catch Spell
38 Thirst Regular/R-HT Body/Food Instant 5 10 sec. M1, Debility, Destroy Water
101 Test Load Area/Inform. Knowledge Instant 2# 1 sec. Measurement
179 Test Fuel/TL Information Tech Instant Varies 1 sec. –
77 Test Food Information Food Instant 1 to 3# 1 sec. –
134 Terror Area/R-Will Mind Instant 4 1 sec. Fear
56 Temporary Enchantment Enchantment Indef.# Varies – Enchant Enchantment
100 Tell Time Information Knowledge Instant 1 1 sec. –
101 Tell Position Information Knowledge Instant 1 1 sec. Measurement
170 Teleport Shield Area Protection/Gate 1 hr. 1/S# 10 sec. Watchdog, either Spell Shield or Teleport
147 Teleport Other* Regular/R-Will+1 Movement/Gate Instant Varies 1 sec. M3, Teleport
147 Teleport* Special Movement/Gate Instant Varies 1 sec. Hawk Flight or IQ 13+ and 1 spell each from 10 colleges
47 Telepathy* Regular Comm. 1 min. 4/4# 4 sec. Mind-Sending
128 Telecast* Special Meta-Spell 1 min. Varies 1 min. M3, Teleport, Wizard Eye, 1 spell each From 10 colleges
36 Tanglefoot Regular/R-DX Body Instant 2 1 sec. Clumsiness
162 Tangle Growth Area Plant 1 min. 1 or 2#/H 2 sec. Plant Growth
58 Talisman Enchantment Enchantment Perm. Varies – Enchant, spell to be opposed
147 Swim Regular Water/Move 1 min. 6/3 3 sec. Shape Water, Levitation
94 Suspended Animation Regular/R-HT Healing Indef.# 6 30 sec. Sleep, 4 Healing spells
86 Suspend Time* Area/R-Spec. Gate 1 day 5/5 5 min. M3, Slow Time
121 Suspend Spell Regular/R-spell Meta-Spell 1 min. Varies 1 sec. M1
125 Suspend Mana* Area Meta-Spell Varies 5 10 min. Suspend Magic, 1 spell each From 10 colleges
123 Suspend Magic Area/R-spell Meta-Spell 1 min. 3/2 sec.=cost Suspend Spell, 8 other spells
130 Suspend Magery* Regular/R-Will+M Meta-Spell 1 hr. 12/12# 10 sec. M2, 2 spells each from 10 colleges#
58 Suspend Enchantment Enchantment 1 hr. 25# 1 sec. Enchant Enchantment
125 Suspend Curse Regular/R-spell Meta-Spell 10 min. 10/10 1 min. M1, 1 spell each From 12 colleges
114 Sunlight Area Lt-Dk 1 min. 2/1 1 sec. M1, Glow, Colors
114 Sunbolt Missile Lt-Dk Instant 1 to Magery# 1 to 3 sec. 6 Light spells inc. Sunlight
150 Summon Spirit Inform./R-Will Necro. 1 min. 20/10# 5 min. Death Vision, M2
102 Summon Shade* Inform./R-Will Knowledge 1 min. 50/20 10 min. Summon Spirit or Divination
27 Summon Elemental Special 4 Diff. 1 hr. 4# 30 sec. M, #
155 Summon Demon Special Necro. 1 hr.# 20# 5 min. M1, 1 spell each from 10 colleges
140 Suggestion Regular/R-Will Mind 10 min. 4/3 10 sec. Emotion Control, Forgetfulness
37 Stun Regular/R-HT Body Instant 2 1 sec. Pain
40 Strike Numb Regular/R-HT Body 10 sec. 3/1 1 sec. Resist Pain
38 Strike Dumb Regular/R-HT Body 10 sec. 3/1 1 sec. Spasm
38 Strike Deaf Regular/R-HT Body 10 sec. 3/1 1 sec. 2 Sound spells, Spasm
38 Strike Blind Regular/R-HT Body 10 sec. 4/2 1 sec. 2 Light spells, Spasm
41 Strike Barren Regular/R-HT Body/Necro. Perm. 5 30 sec. M1, Steal Vitality, Decay
136 Strengthen Will Regular Mind 1 min. 1/pt/H 1 sec. M1, 6 Mind spells
195 Storm Area Weather/Air/Water 1 hour 1/50/S 1 min. Rain, Hail
35 Stop Spasm Regular Body/Healing Instant 1 1 sec. Spasm or Lend Vitality
179 Stop Power Area Tech 1 min. 3/H 3 sec. M1, Seek Power
93 Stop Paralysis Regular Healing Perm. 1 or 2 1 sec. Major Healing, or Minor Healing and Paralyze Limb
153 Stop Healing Regular Necro. Indef.# 10 10 sec. Slow Healing
91 Stop Bleeding Regular Healing Perm. 1 or 10 1 sec. Lend Vitality
53 Stone to Flesh Regular Earth Perm. 10 5 sec. M2, Stone to Earth, Flesh to Stone
51 Stone to Earth Regular Earth Perm. 6/25 cu. ft. 1 sec. Earth to Stone or any 4 Earth spells
52 Stone Missile Missile Earth Instant 1 to Magery 1 to 3 sec. Create Earth
117 Stiffen Regular/R-Spec. Mk-Brk 10 min. 1 per lb./H# 2 sec./lb. Rejoin
24 Stench Area Air 5 min. 1 1 sec. Purify Air
54 Steelwraith Regular/R-HT Earth 1 min. 7/4 2 sec. M2, Walk Through Earth
191 Steam Jet Regular Water 1 sec. 1 to 3 1 sec. Water Jet, Boil Water
158 Steal Youth* Regular/R-HT Necro. Perm. 10 to 30 1 hr. Youth, Age, Steal Vitality
150 Steal Vitality Regular Necro. Perm. none# 1 min./3 HP-# Steal Energy
127 Steal Spell* Regular/R-spec Meta-Spell Perm. Varies 5 sec. Lend Spell, Great Ward
158 Steal Skill* Regular/R-Will Necro. 24 hrs. Varies 1 min. M3, Borrow Skill, Daze
180 Steal Power/TL* Regular Tech 1 min. 0# 1 sec. M2, Minor Healing, Conduct Power
150 Steal Energy Regular Necro. Perm. none# 1 min/3 FP-# Minor Healing
159 Steal Beauty* Regular Necro. 24 hrs. Varies 30 sec. M3, Alter Visage, Steal Vitality
70 Staff Enchantment Enchantment Perm. 30 – Enchant 158 Steal Attribute* Regular/R-Spec Necro. 1 day Varies 1 min. Varies
192 Spit Acid* Regular Water 1 sec. 1 to 4 2 sec. M3, Acid Jet, Resist Acid
32 Spider Silk Missile Animal 1 min. 1/5 yds.# 1 sec. M1, 2 Animal spells
127 Spellguard* Regular/R-Spec Meta-Spell 10 hrs. 1 to 3/S# Varies Dispel Magic
124 Spell Wall Regular/R-spell Meta-Spell 1 min. 2/2# 1 sec. Spell Shield
60 Spell Stone Enchantment Enchantment Varies 20¥spell cost – Enchant, Delay
124 Spell Shield Area Meta-Spell 1 min. 3/2 1 sec. M2, Scryguard, Magic Resistance
65 Spell Arrow Enchantment Enchantment Perm. 30¥spell cost – Spell Stone
66 Speed Spell Arrow Enchantment Enchantment Perm. Varies – Speed, Spell Arrow
57 Speed Enchantment Enchantment Perm. Varies – Enchant, Haste
181 Spectrum Vision* Regular Tech 1 min. 4/4 1 sec. Infravision
35 Spasm Regular/R-HT Body Instant 2 1 sec. Itch
197 Spark Storm Area Weather/Air 1 min.# 2, 4, or 6/H Instant# Windstorm, Lightning
196 Spark Cloud Area Weather/Air 10 sec. 1 to 5/S 1 to 5 sec. Shape Air, Lightning
171 Sound Vision Regular Sound 1 min. 5/2 1 sec. Keen Hearing or Acute Hearing
173 Sound Jet Regular Sound 1 sec. 1 to 4/S 1 sec. Great Voice
171 Sound Regular Sound Varies Varies 1 sec. –
71 Soul Stone* Enchantment Enchantment Perm. 500 – M3, Enchant, Soul Jar
49 Soul Rider Regular/R-Will Comm. 1 min. 5/2 3 sec. Mind-Reading
154 Soul Jar* Regular Necro. Perm. 8 1 min. M1, 6 Necro. spells inc. Steal Vitality
151 Solidify Special Necro. 1 min. 50/10 1 sec. Materialize
116 Soilproof Regular Mk-Brk 10 min. 2/1 2 sec. Clean
186 Snow Shoes Regular Water 1 min. 2/1 2 sec. Shape Water
189 Snow Jet Regular Water 1 sec. 1 to 3 1 sec. Water Jet, Freeze
195 Snow Area Weather/Air/Water 1 hr. 1/15#/S 1 sec. Clouds, Frost
73 Smoke Area Fire 5 min.# 1/H 1 sec. Shape Fire, Extinguish Fire
111 Small Vision Regular Lt-Dk/Knowledge 1 min. 4/2# 2 sec. Keen Vision or 5 Light spells; no Blindness or Bad Sight
86 Slow Time* Area/R-Spec. Gate 1 min. Varies 2 sec. M2, IQ 13+, 2 spells each from 10 colleges
153 Slow Healing Regular/R-HT Necro. 1 day 1 to 5/S 10 sec. M1, Frailty, Steal Vitality
73 Slow Fire Regular Fire 1 min. Varies 1 sec. Extinguish Fire
144 Slow Fall Regular Movement 1 min. 1 per 50 lbs./H 1 sec. Apportation
145 Slow Regular/R-HT Movement 10 sec. 5/4 3 sec. M1, Haste, Hinder
145 Slide Regular/R-Will Movement 1 min. 2/2 1 sec. Apportation, Grease
135 Sleep Regular/R-HT Mind Instant 4 3 sec. Daze
151 Skull-Spirit Regular Necro. 24 hrs. 20 1 sec. 4 other Necromantic spells
61 Simulacrum* Enchantment Enchantment Perm.# 2¥golem – M3, Golem, Perfect Illusion, Illusion Disguise
95 Simple Illusion Area Illusion 1 min. 1/H 1 sec. not blind, IQ 11+
174 Silver Tongue Regular Sound 1 min. 3/2 1 sec. Voices, Emotion Control
171 Silence Area Sound 1 min. 2/1 1 sec. Sound
138 Sickness Regular/R-HT Mind/Body 1 min. 3/3 4 sec. Drunkenness or Pestilence
42 Shrink Other* Regular/R-HT Body 1 hr. 2/-1 SM/S 10 sec. M3, Shrink
120 Shrink Object* Regular Mk-Brk 1 hour Varies 3 sec. Contract Object
42 Shrink* Regular Body 1 hr. 2/-1 SM/S 5 sec. M2, Alter Body
196 Shocking Touch Melee Weather/Air Instant 1 to 3 1 sec. Lightning
167 Shield Regular Protection 1 min. Varies 1 sec. M2
118 Shatterproof Regular Mk-Brk 1 hr. 3/3 1 sec. Repair, Shatter
116 Shatter* Regular Mk-Brk Instant 1 to 3 1 sec. M1, Weaken
118 Sharpen Regular Mk-Brk 1 min. Varies 4 sec. Repair
90 Share Vitality Regular Healing Perm. 0# 1 sec./HP Lend Vitality
89 Share Energy Regular Healing Special Varies 1 sec. Lend Energy
32 Shapeshifting* Special Animal 1 hour Varies 3 sec. M1, 6 other spells
33 Shapeshift Others* Special/R-Will Animal 1 hour Varies 30 sec. M2, Shapeshifting for that form
185 Shape Water Regular Water 1 min. 1/1# 2 sec. Create Water
183 Shape Plastic Regular Tech 1 min. 6/3 1 sec. M1, Shape Plant or 6 Tech spells
161 Shape Plant Regular Plant 1 min. 3/1# 10 sec. Identify Plant
182 Shape Metal Regular Tech 1 min. 6/H# 1 sec. M1, Shape Earth or 6 Tech spells
111 Shape Light Regular Lt-Dk 1 min. 2/2 1 sec. Light
72 Shape Fire Area Fire 1 min. 2/H 1 sec. Ignite Fire
50 Shape Earth Regular Earth 1 min. 1/25 cu. ft./h 1 sec. Seek Earth
113 Shape Darkness Area Lt-Dk 1 min. 2/S# 1 sec. Darkness
24 Shape Air Regular Air 1 min. 1 to 10# 1 sec. Create Air
169 Shade Regular Protection/Lt-Dk 1 hr. 1/H 10 sec. Continual Light or Shield
39 Sensitize Regular/R-HT Body 1 min. 3/2 1 sec. M1, Stun
149 Sense Spirit Inform./Area Necro. Instant 1# 1 sec. Death Vision, or Sense Life and M1
167 Sense Observation Area Protection 1 hr. 1/H# 5 sec. Sense Danger or Scryguard
101 Sense Mana Information Knowledge Instant 3 5 sec. Detect Magic
45 Sense Life Inform./Area Comm. Instant 1# 1 sec. –
44 Sense Foes Inform./Area Comm. Instant 2# 1 sec. –
45 Sense Emotion Regular Comm. Instant 2 1 sec. Sense Foes
166 Sense Danger Information Protection Instant 3 1 sec. Sense Foes or Danger Sense
105 Seeker Information Knowledge Instant 3 1 sec. M1, IQ 12+, 2 Seek spells
184 Seek Water Information Water Instant 2 1 sec. –
181 Seek Radiation Regular Tech Instant 3 10 sec. See Radiation
179 Seek Power/TL Information Tech Instant 3 10 sec. –
182 Seek Plastic Information Tech Instant 3 10 sec. –
161 Seek Plant Information Plant Instant 2 1 sec. –
51 Seek Pass Information Earth Instant 3 10 sec. Seek Earth
102 Seek Magic Information Knowledge/Meta-Spell Instant 6 10 sec. Detect Magic
175 Seek Machine/TL Information Tech Instant 3 10 sec. –
85 Seek Gate Information Gate Instant 3 10 sec. M2, Seek Magic, 1 spell each from 10 colleges
179 Seek Fuel/TL Information Tech Instant 3 10 sec. –
77 Seek Food Information Food Instant 2 1 sec. –
72 Seek Fire Information Fire Instant 1 1 sec. –
50 Seek Earth Information Earth Instant 3 10 sec. –
184 Seek Coastline Information Water Instant 3 10 sec. Seek Water
23 Seek Air Information Air Instant 1 1 sec. –
107 See Secrets Regular Knowledge 1 min. 5/2 5 sec. Seeker, Aura
181 See Radiation Regular Tech 1 min. 3/2 1 sec. –
113 See Invisible Regular Lt-Dk 1 min. 4/2 1 sec. Invisibility, or Dark Vision and Infravision
77 Season Regular Food Perm. 2/meal 10 sec. Test Food
122 Scrywall Area Meta-Spell 10 hrs. 3/2 sec.=cost Scryguard
121 Scryguard Regular Meta-Spell 10 hrs. 3/1 5 sec. M1
123 Scryfool Regular/R-Spec Meta-Spell 10 hrs. 4/2 10 sec. M2, Sense Observation, Simple Illusion
85 Scry Gate Regular Gate 1 min. 4/4 10 sec. Seek Gate
57 Scroll Enchantment Enchantment Varies Special days=cost M1, 1 language at Accented
174 Scribe Regular Sound 1 min. 3/1 1 sec. Voices, Dancing Object, 1 Accented language
177 Schematic/TL Information Tech/Knowledge 1 min. 5/H# 5 sec. Reveal Function, History
107 Scents of the Past Regular Knowledge/Food 1 min. 1/1# 10 sec. M2, History, Odor
27 Sandstorm Area Air/Earth 1 minute# 3/H Instant# Windstorm, Create Earth
52 Sand Jet Regular Earth 1 sec. 1 to 3/S 1 sec. Create Earth
86 Sanctuary* Special Gate 1 hr. 5/S 10 sec. Hide Object
118 Ruin Regular Mk-Brk 1 min.# 2 per lb./S 5 sec./lb. M1, Weaken, Decay
36 Roundabout Regular/R-HT Body Instant 3 1 sec. Tanglefoot
154 Rotting Death* Regular/R-HT Necro. 1 sec. 3/2 3 sec. M2, Sickness, Pestilence
36 Rooted Feet Regular/R-ST Body 1 min.# 3 1 sec. Hinder
117 Rive* Regular Mk-Brk Instant 1 per die 1 sec. M2, Shatter
31 Rider Within Regular Animal 1 min. 4/1 3 sec. 2 Control spells#
31 Rider Regular Animal 5 min. 2/1 1 sec. 1 Control spell#
168 Reverse Missiles Regular Protection 1 min. 7/3 1 sec. Missile Shield or Force Dome
176 Reveal Function/TL Information/R-spell Tech Instant 8 10 min. Seek Machine
168 Return Missile Blocking Protection Instant 2 1 sec. Catch Missile
47 Retrogression Regular/R-Will Comm. 1 sec. 5 10 sec. Mind-Search, Mind-Sending
38 Retch Regular/R-HT Body Instant 3 4 sec. Nauseate, Spasm
94 Resurrection* Regular Heal./Necro. Perm. 300 2 hrs. Instant Regeneration, Summon Spirit
93 Restore Speech Regular Healing 1 hr. 5/3 5 sec. Minor Healing, Great Voice or Strike Dumb
92 Restore Sight Regular Healing 1 hr. Varies 5 sec. Minor Healing, Keen Vision or Strike Blind
92 Restore Memory Regular Healing Perm. 3 10 sec. Awaken, IQ 11+
127 Restore Mana* Area Meta-Spell Perm. 10 1 hr. Dispel Magic, Suspend Mana
92 Restore Hearing Regular Healing 1 hr. Varies 5 sec. Minor Healing, Keen Hearing or Strike Deaf
116 Restore Regular Mk-Brk 10 min. 2/1 3 sec. Find Weakness or Simple Illusion
93 Restoration* Regular Healing Perm. 15 1 min.# Major Healing, or any 2 of Relieve Paralysis and the
186 Resist Water Regular Water/Protection 1 min. 2/1 1 sec. Umbrella, or Shape Water and Destroy Water
173 Resist Sound Regular Sound/Protection 1 min. 2/1 1 sec. 4 Sound spells
182 Resist Radiation Regular Tech/Protection 1 min. varies# 1 sec. 3 Radiation spells
91 Resist Poison Regular Healing/Protection 1 hr. 4/3 10 sec. Vigor 169 Resist Pressure Regular Protection 1 min. Varies 1 sec. Weather Dome
38 Resist Pain Regular Body 1 min. 4/2 1 sec. M2, Pain
196 Resist Lightning Regular Weather/Air/Protection 1 min. 2/1 1 sec. 6 Air spells
74 Resist Fire Regular Fire 1 min. 2/1# 1 sec. Fireproof
58 Resist Enchantment Enchantment Enchantment Perm. Varies – any Limiting Enchantment
90 Resist Disease Regular Healing/Protection 1 hour 4/3 10 sec. Remove Contagion or Vigor
74 Resist Cold Regular Fire 1 min. 2/1 1 sec. Heat
190 Resist Acid Regular Water/Protection 1 min. 2/H# 1 sec. Create Acid
117 Reshape Regular Mk-Brk 1 min. 6/3 10 sec. M1, Weaken, Shape Earth or Shape Plant
158 Repel Spirits Area/R-Will Necro. 1 hr. 4/H 10 sec. Banish, Turn Spirit
31 Repel Hybrids* Area/R-HT Animal 1 hour 6/3 10 sec. Hybrid Control
31 Repel Animal Area/R-HT Animal 1 hour Varies 10 sec. 1 Control spell#
147 Repel Regular Movement 1 min. 1/2 ST/S 5 sec. M2, 4 Movement spells inc. Levitation
118 Repair Regular Mk-Brk Perm. 2/5 lbs.# 1 sec./lb. M2, Rejoin
110 Remove Shadow Regular/R-Will Lt-Dk 1 min. 2/1 1 sec. Light
113 Remove Reflection Regular/R-Will Lt-Dk 1 min. 2/1 1 sec. Remove Shadow
58 Remove Enchantment Enchantment Perm. 100# varies Enchant Enchantment
126 Remove Curse Regular/R-spell Meta-Spell Instant 20 1 hr. M2, Suspend Curse or 1 spell each from 15 colleges
90 Remove Contagion Area Healing Instant 3 2 sec. Decay, Clean, or Cure Disease
127 Remove Aura Regular/R-Will# Meta-Spell Perm. 5 10 sec. Dispel Magic, Aura
107 Remember Path Regular Knowledge 1 hr. 3/1 10 sec. Find Direction, Memorize
90 Relieve Sickness Regular/R-spell Healing 10 min. 2 10 sec. Lend Vitality
93 Relieve Paralysis Regular Healing 1 min. Varies 10 sec. Stop Paralysis
92 Relieve Madness Regular/R-spell Healing/Mind 10 min. 2 10 sec. Lend Vitality, Wisdom
92 Relieve Addiction Regular Healing 1 day 6 10 sec. Neutralize Poison
163 Rejuvenate Plant Regular Plant Perm. 3 1 sec. M1, Plant Growth
116 Rejoin Regular Mk-Brk 10 min. 1 per 10 lbs./H 4 sec./10 lbs. Weaken, Restore
93 Regeneration* Regular Healing Perm. 20 Special# Magery 2, Restoration
39 Reflexes Regular Body 1 min. 5/3 1 sec. Grace, Haste
132 Reflex Special Meta-Spell 1 hr. Varies 10 sec. Delay, Ward
168 Reflect Gaze* Blocking/R-Spec Protection Instant 2 1 sec. Mirror
122 Reflect Block/R-spell Meta-Spell Instant 4 or 6# none Ward
89 Recover Energy Special Healing Special none Special M1, Lend Energy
106 Reconstruct Spell Information Knowledge Instant 3# 10 sec. M2, History, Identify Spell
106 Recall Regular Knowledge/Mind 1 day# 4 10 sec. M2, Memorize, Wisdom
177 Rebuild/TL Regular Tech/Mk-Brk Perm. Varies Varies M3, Repair, Create Object, 3 spells of each element#
134 Rear Vision Regular Mind 1 min. 3/1 1 sec. Alertness
82 Rapid Journey* Special Gate/Movement 1 min. Varies 5 sec. M3, Teleport or Timeport
53 Rain of Stones Area Earth 1 min. 1/S# 1 sec. M2, Create Earth
165 Rain of Nuts Area Plant 1 min. 1/10/S 1 sec. M1, 6 Plant spells inc. Shape Plant
192 Rain of Ice Daggers Area Water 1 min. 2/2# 1 sec. M2, Hail, Ice Dagger
74 Rain of Fire Area Fire 1 min. 1/S# 1 sec. M2, Create Fire
191 Rain of Acid Area Water 1 min. 3/3 1 sec. M2, Create Water, Create Earth
195 Rain Area Weather/Air/Water 1 hr. 1/10/S# 1 min. Clouds
181 Radio Hearing Regular Tech 1 min. 2/1 1 sec. Keen Hearing
182 Radiation Jet Regular Tech 1 sec. 1 to 3/S 1 sec. Irradiate, Resist Radiation
144 Quick March Regular Movement 1 day’s march 4# 1 min. M1, Haste
63 Quick-Draw Enchantment Enchantment Perm. 300/lb.# – Enchant, Apportation
65 Quick-Aim Enchantment Enchantment Perm. Varies – Enchant, Grace
184 Purify Water Special Water Perm. 1/gal. 5-10 sec./gal.# Seek Water
179 Purify Fuel/TL Regular Tech Instant 1# 1 sec. Purify Water or Decay
78 Purify Food Regular Food Perm. 1 per lb. 1 sec. Decay
54 Purify Earth Area Earth/Plant Perm. 2# 30 sec. Create Earth, Plant Growth
23 Purify Air Area Air Instant 1 1 sec. –
146 Pull Regular Movement 1 min. 1/2 ST/S 5 sec. M2, 4 Movement spells inc. Levitation
65 Puissance Enchantment Enchantment Perm. Varies – Enchant, 5 Earth spells
32 Protect Animal Area Animal/Protection 1 min. 1/S 1 min. Armor, Watchdog, 3 Animal spells
180 Propel/TL Regular Tech 10 min. Varies 1 sec. Create Fuel, Dancing Object
105 Projection Regular Knowledge 1 min. 4/2 3 sec. Sense Spirit, 4 Knowledge spells
179 Preserve Fuel/TL Regular Tech 1 week 4/lb./H 1 sec. Test Fuel
79 Preserve Food Regular Food 1 week Special 1 sec. Decay
48 Presence Regular/R-Spec. Comm. 1 hour 4/4 10 sec. Persuasion, Lure
78 Prepare Game Regular Food Perm. 2 10 sec. Purify Food
106 Prehistory Information Knowledge Instant Varies hr.=cost Ancient History
193 Predict Weather Information Weather/Air Instant Varies 5 sec.# 4 Air spells
51 Predict Earth Information Earth Instant 2 per day# Varies 4 Earth spells Movement
69 Powerstone Enchantment Enchantment Perm. 20 – Enchant
57 Power Enchantment Enchantment Perm. Varies – Enchant, Recover Energy or Beast Possession
144 Poltergeist Missile/R-HT Movement Instant 1 or 2# 1 sec. Apportation 49 Possession* Regular/R-Will Comm. 1 min. 10/4 1 min. M1, and Control Person
162 Pollen Cloud Area/R-HT Plant 5 min.# 1 1 sec. Shape Plant
78 Poison Food Regular Food Perm. 3 per meal 1 sec. Purify Food, Decay
183 Plastic Vision Regular Tech/Knowledge 30 sec. 2/5 yds./S 1 sec. Shape Plastic
162 Plant Vision Regular Plant/Knowledge 30 sec. 1/10 yds. 1 sec. Shape Plant
164 Plant Speech Regular Plant 1 min. 3/2 1 sec. M1, Plant Sense
163 Plant Sense Regular/R-Hide Path Plant 1 min. 3/2 1 sec. Forest Warning, Hide Path
162 Plant Growth Area Plant 1 min. 3/2 10 sec. Heal Plant
165 Plant Form Other Special/R-Will Plant 1 hr. 5/2 30 sec. M2, Plant Form
164 Plant Form Special Plant 1 hr. 5/2 1 sec. M1, 6 Plant spells
164 Plant Control Regular/R-Will Plant 1 min. 3/H 1 sec. Plant Sense
83 Plane Shift Other* Regular/R-Will+1 Gate Instant 20 5 sec. M3, Plane Shift
83 Plane Shift* Special Gate Instant 20 5 sec. Planar Summons
82 Planar Visit* Special Gate 1 min. 4/2 30 sec. M2, Projection or Planar Summons
82 Planar Summons Special Gate 1 hr. 20# 5 min. M1, 1 spell each from 10 colleges
83 Phase Other* Blocking Gate Instant 3 1 sec. Phase
83 Phase Blocking Gate Instant 3 1 sec. M3, Plane Shift or Ethereal Body
73 Phantom Flame Area Fire/Illusion 1 min. 1/S 1 sec. Shape Fire or Simple Illusion
97 Phantom* Area Illusion 1 min. 5/H# 1 sec. M2, Perfect Illusion, Hinder, Apportation
154 Pestilence Regular Necro. Perm. 6 30 sec. M1, Steal Vitality, Decay
45 Persuasion Regular/R-Will Comm. 1 min. 2¥bonus# 1 sec. Sense Emotion
33 Permanent Shapeshifting* Regular Animal Indef. Varies 1 min. M3, Shapeshifting
49 Permanent Possession* Regular/R-Will Comm. Indef. 30 5 min. M3, Possession
139 Permanent Madness* Regular/R-Will-2 Mind Perm. 20 10 min. M2, Madness, IQ 13+
178 Permanent Machine Possession/TL* Regular/R-Will Tech Indef.# 30 5 min. M3, Machine Possession
138 Permanent Forgetfulness* Regular/R-Will or skill Mind Perm. 15 1 hr. M2, Forgetfulness, IQ 13+
32 Permanent Beast Possession* Regular/R-Will Animal Indef. 20 1 min. M2, Beast Possession
35 Perfume Regular/R-HT Body 10 min. 2/1 1 sec. Odor
96 Perfect Illusion Area Illusion 1 min. 3/H# 1 sec. M1, Complex Illusion
124 Pentagram Special Meta-Spell Perm. 1/sq. ft.# 1/sq. ft.# Spell Shield
63 Penetrating Weapon Enchantment Enchantment Perm. Varies – Enchant, Find Weakness
138 Peaceful Sleep Regular/R-Spec. Mind 8 hrs. 4 30 sec. Sleep, Silence 123 Penetrating Spell Regular Meta-Spell Varies Varies 3 sec. Delay, Find Weakness
105 Pathfinder Information Knowledge Instant 4 10 sec. M1, IQ 12+, 2 Seek spells
68 Password Enchantment Enchantment Perm. 400# – Enchant
34 Partial Shapeshifting* Regular/R-Will Animal 1 hour Varies 10 sec. M3, Shapeshift Others, Alter Body
52 Partial Petrifaction* Regular/R-HT Earth Perm. 12 3 sec. M2, Flesh to Stone
40 Paralyze Limb Melee/R-HT Body 1 min. 3 1 sec. M1, 5 Body Control spell inc. Clumsiness
134 Panic Area/R-Will Mind 1 min. 4/2 1 sec. Fear
36 Pain Regular/R-HT Body 1 sec. 2 2 sec. Spasm
70 One-College Powerstone Enchantment Enchantment Indef. 12 – Enchant
24 Odor Area Air 1 hr. 1 1 sec. No-Smell
138 Oath Regular/R-spec Mind Perm. 4 1 min. M1, Emotion Control
173 Noise Area Sound 5 sec. 4/2 1 sec. Wall of Silence
24 No-Smell Regular Air 1 hr. 2/2 1 sec. Purify Air
140 Nightmare Regular/R-Will Mind 1 hr. 6 1 min. M2, Death Vision, Fear, Sleep
137 Nightingale Area Protection 10 hrs. 2/2 1 sec. Sense Danger
111 Night Vision Regular Lt-Dk 1 min. 3/1 1 sec. Keen Vision or 5 Light spells
92 Neutralize Poison Regular Healing Perm. 5 30 sec. Cure Disease or M3 and Test Food
38 Nauseate Regular/R-HT Body 10 sec. 2/S 1 sec. 2 Body spells inc. Perfume
68 Name Enchantment Enchantment Perm. 200 or 400# – Enchant
168 Mystic Mist Area Protection 10 hrs. 1/S 5 min. M1 and Watchdog or Shield
119 Mystic Mark Regular/R-Spec Mk-Brk Indef.# 3 10 sec. Dye, Trace
174 Musical Scribe Regular Sound 1 min. 3/1# 1 sec. Scribe
52 Mud Jet Regular Earth/Water 1 sec. 1 to 3 1 sec. Sand Jet and Create Water or Create Earth and Water Jet
55 Move Terrain* Area/R-Spec. Earth 1 hour 10/8 1 min. Alter Terrain, Hide Object
79 Monk’s Banquet Regular Food 24 hrs. 6 1 sec. Fool’s Banquet, Resist Pain
168 Missile Shield Regular Protection 1 min. 5/2 1 sec. Apportation or Shield
112 Mirror Regular Lt-Dk 1 min. 2/2 1 sec. Colors
91 Minor Healing Regular Healing Perm. 1 to 3 1 sec. Lend Vitality
137 Mindlessness* Regular/R-Will Mind 1 min. 8/4 5 sec. M2, Forgetfulness
47 Mind-Sending Regular Comm. 1 min. 4/4 4 sec. Mind-Reading
46 Mind-Search* Regular/R-Will Comm. 1 min. 6/3 1 min. Mind-Reading
46 Mind-Reading Regular/R-Will Comm. 1 min. 4/2 10 sec. Truthsayer or Borrow Language
37 Might Regular Body 1 min. 2 per ST+/S 1 sec. Lend Energy
183 Metal Vision Regular Tech/Knowledge 30 sec. 2/5 yds./S 1 sec. Shape Metal
174 Message Regular/R-spell Sound/Comm. Varies 1/15 sec. Varies Great Voice, Seeker
135 Mental Stun Regular/R-Will Mind Instant 2 1 sec. Daze or Stun
105 Memorize Regular Knowledge/Mind 1 day# 3 2 sec. Wisdom or 6 Knowledge spells
186 Melt Ice Area Water Perm.# 2# 10 sec. Heat or Freeze
100 Measurement Area/Inform. Knowledge Instant 1 1 sec. –
78 Mature Regular Food Perm. 1 per pound 10 sec. Decay or Season
150 Materialize Special Necro. 1 min. 5/5 1 sec. Summon Spirit
30 Master Reg./Block./R-IQ Animal Indef. 2 1 sec. Beast-Soother
153 Mass Zombie* Area Necro. Perm. 7 varies# Zombie, Charisma 2+
141 Mass Suggestion Area/R-Will Mind 10 min. 4/2# sec.=cost Suggestion
137 Mass Sleep Area/R-HT Mind Instant 3# sec.=cost Sleep, IQ 13+
137 Mass Daze Area/R-HT Mind Instant 2/1# sec.=cost Daze, IQ 13+
118 Mapmaker Special Mk-Brk 1 hr. 4/2 10 sec. Inscribe, Measurement
145 Manipulate Regular Movement 1 min. 4/3# 3 sec. Locksmith
70 Manastone* Enchantment Enchantment Indef. 5 – Enchant
177 Malfunction/TL Melee/R-HT Tech 1 min. 5 1 sec. M2, Glitch
60 Malefice* Enchantment Enchantment Indef.# 250 – Enchant, Seeker
91 Major Healing* Regular Healing Perm. 1 to 4 1 sec. M1, Minor Healing
128 Maintain Spell* Special Meta-Spell Indef.# Varies 2 sec.# Link
181 Magnetic Vision Regular Tech 1 min. 2/1 1 sec. Keen Vision
123 Magic Resistance Regular/R-Will+M Meta-Spell 1 min. 1 to 5/S# 3 sec. M1, 1 spell each from 7 colleges
166 Magelock Regular Protection 6 hrs. 3/2 4 sec. M1
172 Mage-Stealth Regular Sound 1 min. 3/2 3 sec. Hush
102 Mage Sight Regular Knowledge 1 min. 3/2 1 sec. Detect Magic
102 Mage Sense Information Knowledge 1 min. 3/2 1 sec. Detect Magic
113 Mage Light Regular Lt-Dk 1 min. Varies 1 sec. Mage Sight, Light
136 Madness Regular/R-Will-2 Mind 1 min. 4/2 2 sec. Forgetfulness or Drunkenness
176 Machine Summoning/TL Regular Tech 1 min. 4/2 4 sec. Machine Control
176 Machine Speech/TL Regular Tech/Comm. 1 min. 5/3 1 sec. Machine Summoning
178 Machine Possession/TL Regular/R-Will Tech 1 min. 6/2 30 sec. Machine Control, Rider Within or Soul Rider
176 Machine Control/TL Regular Tech 1 min. 6/3 1 sec. Reveal Function, Locksmith, Lightning
137 Lure Area/R-Will Mind 1 hr. 1/S 10 sec. Emotion Control
136 Loyalty Regular/R-Will Mind 1 hr. 2/2# 2 sec. Bravery, 2 other Mind Control spells
63 Loyal Sword Enchantment Enchantment Perm. 750/lb.# – Enchant, Apportation
143 Long March Regular/R-ST Movement 1 day’s march 3 1 min. M1, Clumsiness or Debility
143 Locksmith Regular Movement 1 min. 2/2 1 sec. Apportation
144 Lockmaster Regular/R-Magelock Movement Perm. 3 10 sec. Locksmith or Apportation and M2
131 Link Area Meta-Spell Indef.# 8 4 hrs. Delay
68 Limit Enchantment Enchantment Perm. 200 – Enchant
196 Lightning Whip Regular Weather/Air 10 sec. 1 per 2 yards# 2 sec. Lightning
198 Lightning Weapon Regular Weather/Air 1 min. 4/1 2 sec. M2, Lightning
198 Lightning Stare* Regular Weather/Air 1 sec. 1 to 4 2 sec. Lightning, Resist Lightning
198 Lightning Missiles Regular Weather/Air 1 min. 4/2# 3 sec. Lightning Weapon
198 Lightning Armor Regular Weather/Air 1 min. 7/4 1 sec. 6 Lightning spells inc. Resist Lightning
196 Lightning Missile Weather/Air Instant 1 to Magery# 1 to 3 sec. M1, 6 Air spells
143 Lighten Burden Regular Movement 10 min. 3 or 5/H# 3 sec. Apportation
67 Lighten Enchantment Enchantment Perm. Varies – Enchant
145 Light Tread Regular Movement 10 min. 4/1# 1 sec. Apportation, Shape Earth
112 Light Jet Regular Lt-Dk 1 min. 2/1 1 sec. Continual Light or Shape Light
110 Light Regular Lt-Dk 1 min. 1/1 1 sec. –
159 Lich* Enchantment Necro./Ench. Perm. Varies Varies M3, IQ 13+, Enchant, Soul Jar, Zombie
143 Levitation Regular/R-ST or Will Movement 1 min. 1 per 80 lbs./H# 2 sec. Apportation
58 Lesser Wish* Enchantment Enchantment Special 180 – Enchant
140 Lesser Geas* Regular/R-Will Mind Perm. 12 30 sec. M2, 10 Mind Control spells
41 Lengthen Limb Regular Body 1 min. 2/2 5 sec. M3, Shapeshifting
89 Lend Vitality Regular Healing 1 hr. 1 per HP loaned 1 sec. Lend Energy
126 Lend Spell Regular Meta-Spell Perm. Varies 3 sec. M1, Lend Skill, 1 spell each From 6 colleges
47 Lend Skill Regular Comm. 1 min. 3/2 3 sec. Mind-Sending, IQ 11+
180 Lend Power/TL Regular Tech Indef. Varies 1 sec. M2, Seek Power
46 Lend Language Regular Comm. 1 min. 3/1 3 sec. 3 Communication spells, or Beast Speech
89 Lend Energy Regular Healing Perm. Varies 1 sec. M1 or Empathy advantage
61 Leak Enchantment Enchantment Perm. 100 – Hideaway
106 Know True Shape Information Knowledge Instant 2 1 sec. M1, any one shifting spell, either Aura or Know Illusion
78 Know Recipe Information/R-Spec. Food/Knowledge 1 day# 3 15 sec. Far-Tasting, Season
103 Know Location Information Knowledge Instant 2 10 sec. M1, Tell Position
97 Know Illusion Information Illusion Instant 2 1 sec. Simple Illusion
117 Knot Regular Mk-Brk Indef.# 2 3 sec. Stiffen
133 Keen Sense Regular Mind 30 min. 1 per +/H# 1 sec. –
143 Jump Regular Movement 1 min. 1 to 3 1 sec. Apportation
35 Itch Regular/R-HT Body Scratch# 2 1 sec. –
181 Irradiate Area Tech 1 hr. 1/10 rads/hr./h 1 sec. 2 Earth spells, 2 Fire spells
169 Iron Arm Blocking Protection Instant 1 1 sec. Resist Pain, DX 11+
104 Invisible Wizard Eye Regular Knowledge 1 min. 5/3 4 sec. Wizard Eye, Invisibility
174 Invisible Wizard Ear Regular Sound 1 min. 5/3 4 sec. Wizard Ear, Invisibility
114 Invisibility Regular Lt-Dk 1 min. 5/3 3 sec. 6 Light spells inc. Blur
93 Instant Restoration* Regular Healing Perm. 50 Special M2, Restoration
93 Instant Regeneration* Regular Healing Perm. 80 Special M3, Regeneration
92 Instant Neutralize Regular Healing Instant 8 1 sec. M2, Neutralize Poison Poison
115 Inspired Creation* Regular Mk-Brk Perm. 5/day Varies –
48 Insignificance Regular/R-Spec. Comm. 1 hour 4/4 10 sec. Persuasion, Avoid
97 Inscribe Area/R-Will Illusion/Mk-Brk 1 min. 2/S 1 sec. Simple Illusion, Copy
97 Initiative Area Illusion Indef.# Varies 10 sec. Independence, Wisdom
111 Infravision Regular Lt-Dk 1 min. 3/1 1 sec. Keen Vision or 5 Light spells
96 Independence Area Illusion Varies 2 Varies Simple Illusion
143 Increase Burden Regular/R-Spec Movement 10 min. 1/25 lbs.# 3 sec. Apportation
60 Impression Blocker Enchantment Enchantment Perm. 20/lb. – Enchant, Seeker, Scrywall
172 Imitate Voice Regular/R-HT Sound 1 min. 3/1 1 sec. Voices
107 Images of the Past Regular Knowledge/Lt-Dk 1 min. 3/3# 10 sec. M2, History, Simple Illusion
96 Illusion Shell Regular Illusion 1 min. 1 or 2/H 1 sec. Simple Illusion
96 Illusion Disguise Regular Illusion Varies 3 1 sec. Simple Illusion
72 Ignite Fire Regular Fire 1 sec. 1 to 4/S 1 sec. –
102 Identify Spell Information Knowledge Instant 2 1 sec. Detect Magic
182 Identify Plastic Information Tech Instant 1 1 sec. –
161 Identify Plant Information Plant Instant 2 1 sec. Seek Plant
182 Identify Metal Information Tech Instant 1 1 sec. –
185 Icy Weapon Regular Water 1 min. 3/1 3 sec. Create Water
188 Icy Touch Melee Water Perm. 2# 1 sec.# M1, 4 Water spells
186 Icy Missiles Regular Water 1 min. 4/2 3 sec. Icy Weapon
192 Icy Breath* Regular Water 1 sec. 1 to 4 2 sec. M1, Snow Jet, Resist Cold
186 Ice Sphere Missile Water Instant 1 to Magery# 1 to 3 sec. Shape Water
186 Ice Slick Area Water Perm. 3 Varies Frost
188 Ice Dagger Missile Water Instant 1 to Magery# 1 to 3 sec. Ice Sphere or Water Jet
30 Hybrid Control* Regular/R-Will Animal 1 min. 6/3 1 sec. 2 Control spells#
172 Hush Regular/R-Will Sound 10 sec.# 2/1 2 sec. Silence
38 Hunger Regular/R-HT Body/Food Instant 2 5 sec. M1, Debility, Decay
70 Homunculus Enchantment Enchantment Perm. 800 – Enchant, Mind-Sending
143 Hold Fast Blocking Movement Instant 1/yd.# 1 sec. Apportation
39 Hold Breath Regular Body 1 min. 4/2 1 sec. M1, Vigor
106 History Information Knowledge Instant Varies sec.=cost Trace
36 Hinder Regular Body/Movement 1 min. 1 to 4/S 1 sec. Haste or Clumsiness
61 Hideaway Enchantment Enchantment Perm. 50# – Enchant, Create Object, Lighten
46 Hide Thoughts Regular Comm. 10 min. 3/1 1 sec. Truthsayer or Hide Emotion
162 Hide Path Regular Plant 1 min. 2/1 1 sec. Heal Plant
86 Hide Object Regular Gate 1 hr. 1/lb./S 10 sec. Hideaway, Teleport
45 Hide Emotion Regular Comm. 1 hour 2/2 1 sec. Sense Emotion
113 Hide Regular Lt-Dk 1 hr. 1 to 5/S 5 sec. Blur or Forgetfulness
57 Hex Enchantment Enchantment Perm. 200 – Enchant
74 Heat Regular Fire 1 min. Varies 1 min. Create Fire, Shape Fire
94 Healing Slumber Regular/R-# Healing 8 hrs.# 6 or 10 30 sec. M2, Sleep, Minor Healing
161 Heal Plant Area Plant Perm. 3 1 min. Identify Plant
111 Hawk Vision Regular Lt-Dk 1 min. 2/lvl./H# 2 sec. Keen Vision or 5 Light spells; no Blindness or Bad Sight
146 Hawk Flight* Regular Movement 1 min. 8/4 3 sec. Flight
142 Haste Regular Movement 1 min. 2/pt./H 2 sec. –
167 Hardiness Blocking Protection Instant 1/DR+# 1 sec. Block
128 Hang Spell* Special Meta-Spell 1 hr. Varies 10 sec. Delay
94 Halt Aging* Regular Healing 1 month 20 1 sec. M2, 8 Healing spells
140 Hallucination Regular/R-Will Mind 1 min. 4/2 3 sec. Madness, Suggestion
39 Haircut Regular/R-HT Body Instant 2 2 sec. Weaken, 2 Body spells
39 Hair Growth Regular/R-HT Body 5 sec. 1/1 1 sec. 5 Body spells
195 Hail Area Weather/Water 1 min. 1/5/S# 1 sec. Snow
62 Great Wish* Enchantment Enchantment Special 2,000 – M3, Wish, (DX + IQ):30+
122 Great Ward Block/R-spell Meta-Spell Instant 1 per subject# none M2, Ward
173 Great Voice Regular Sound 1 min. 3/1 2 sec. Voices, Thunderclap
34 Great Shapeshift* Special Animal 1 min. 20/H# 5 sec. M3, Alter Body, 4 Shapeshifting, 10 other spells
91 Great Healing Regular Healing Perm. 20 1 min. M3, Major Healing
146 Great Haste* Regular Movement 10 sec. 5# 3 sec. M1, Haste, IQ 12+
141 Great Hallucination* Regular/R-Will Mind 1 min. 6/3 4 sec. M2, Hallucination
141 Great Geas* Regular/R-Will Mind Perm. 30 1 min. M3, 15 Mind Control spells inc. Lesser Geas
142 Grease Area Movement 10 min. 3/S 1 sec. Haste
63 Graceful Weapon Enchantment Enchantment Perm. 150/lb. – Enchant, Apportation
37 Grace Regular Body 1 min. 4 per DX+/S 1 sec. Clumsiness
59 Golem* Enchantment Enchantment Perm. Varies Varies Enchant, Shape Earth, Animation
142 Glue Area Movement 10 min. 3/S 1 sec. Haste
112 Glow Area Lt-Dk Varies Varies Varies Continual Light
112 Gloom Area Lt-Dk Varies Varies Varies Continual Light
176 Glitch/TL Regular/R-HT Tech Instant 3 1 sec. Machine Control
141 Glib Tongue Regular/R-Will Mind 5 min. 2/1 1 sec. Suggestion
103 Glass Wall Regular Knowledge 1 min. 4/2 1 sec. 5 Knowledge spells or Earth Vision
46 Gift of Tongues* Regular Comm. 1 min. Varies 1 sec. Borrow Language, 3 languages at Accented
46 Gift of Letters* Regular Comm. 1 min. Varies 1 sec. Borrow Language, 3 languages at Accented
65 Ghost Weapon Enchantment Enchantment Perm. 250/lb.# – Enchant, Solidify
190 Geyser* Area Water 1 sec. 5/2 5 sec. 6 Water spells inc. Create Well and either 4 Earth or Fire spells
43 Gauntness* Regular/R-HT Body 10 min. 6/6 3 sec. M2, Earth to Air, Destroy Water, 4 Body spells inc. Hunger
172 Garble Regular/R-Will Sound 1 min. 4/2 1 sec. Voices
38 Fumble Blocking/R-DX Body Instant 3 none Clumsiness
189 Frostbite Regular/R-HT Water Perm. 1 to 3 3 sec. Frost, Freeze
193 Frost Area Weather/Water Indef. 1 1 sec. Create Water or Cold
185 Freeze Regular Water Perm. Varies 10 sec. Shape Water
148 Freedom Regular Movement/Protection 1 min. 2/pt/S 1 sec. 3 Body spells, 3 Movement spells, 3 Protection spells
37 Frailty Regular/R-HT Body 1 min. 2 per HT-/S# 1 sec. Lend Energy
185 Foul Water Area Water/Food Perm. 3 1 sec. Purify Water, Decay
66 Fortify Enchantment Enchantment Perm. Varies – Enchant
135 Forgetfulness Regular/R-Will Mind 1 hr. 3/3 10 sec. M1, Foolishness or skill
162 Forest Warning Area Plant 10 hrs. 2#/S 1 sec. 4 Plant spells
170 Force Wall Regular Protection 10 min. 2/yd./S 1 sec. Force Dome
170 Force Dome Area Protection 10 min. 3/2 1 sec. Weather Dome, Apportation
134 Foolishness Regular/R-Will Mind 1 min. 1 per IQ-/H 1 sec. IQ 12+
79 Fool’s Banquet Regular Food 1 day 2 per meal 1 sec. M1, Cook, Foolishness
193 Fog Area Weather/Water 1 min. 2/H 1 sec. Shape Water
146 Flying Carpet* Regular Movement 10 min. 1/sq. ft./H 5 sec. Flight, or M2 and Walk on Air
145 Flight* Regular Movement 1 min. 5/3 2 sec. M2, Levitation
51 Flesh to Stone Regular/R-HT Earth Perm. 10# 2 sec. Earth to Stone
190 Flesh to Ice* Regular/R-HT Water Perm. 12 2 sec. M1, Frostbite, Body of Water
112 Flash Regular Lt-Dk Instant 4 2 sec. Continual Light
75 Flaming Weapon Regular Fire 1 min. 4/1 2 sec. M2, Heat
75 Flaming Missiles Regular Fire 1 min. 4/2# 3 sec. Flaming Weapon
75 Flaming Armor Regular Fire 1 min. 6/3 1 sec. M1, Resist Fire, Flame Jet
73 Flame Jet Regular Fire 1 sec. 1 to 3/S 1 sec. Create Fire, Shape Fire
73 Fireproof Area Fire 1 day 3# 5 min. Extinguish Fire
74 Fireball Missile Fire Instant 1 to Magery# 1 to 3 sec. M1, Create Fire, Shape Fire
75 Fire Cloud Area Fire 10 sec. 1 to 5/S 1 to 5 sec. Shape Air, Fireball
116 Find Weakness Information Mk-Brk Instant 1# 2 sec. 1 spell of each four elements
101 Find Direction Information Knowledge Instant 2 1 sec. M1
89 Final Rest Regular Healing/Necro. Perm. 20 10 min.# M1 or Spirit Empathy
139 Fear Area/R-Will Mind 10 min. 1 1 sec. Sense Emotion or Empathy
118 Fasten Regular/R-DX Mk-Brk Perm. 3# 1 sec. Knot
73 Fast Fire Regular Fire 1 min. Varies 1 sec. Slow Fire
135 Fascinate Regular or Blocking/R-Will Mind Indef.# 4 1 sec. Daze
77 Far-Tasting Regular Food/Knowledge 1 min. 3/1 3 sec. M1, no anosmia, Seek Food or Seek Air
173 Far-Hearing Information Sound/Know. 1 min. 4/2 3 sec. M1, 4 Sound spells, no Deafness or Hard of Hearing
100 Far-Feeling Regular Knowledge 1 min. 3/1 3 sec. M1
163 False Tracks Regular/R-Will Plant 1 min. 2/1 1 sec. Shape Plant, Shape Earth
139 False Memory Regular/R-Will Mind Varies Varies 5 sec. Forgetfulness, 6 other Mind Control spells
122 False Aura Regular/Area/R-IQ# Meta-Spell 10 hrs. 4/H 10 sec. Conceal Magic, Aura
181 Extinguish Regular Tech Perm. 1/10 rads/hr. 1 sec. M2, Extinguish Fire, Radiation* Earth to Air, Irradiate
72 Extinguish Fire Regular Fire Perm. 3 1 sec. Ignite Fire
120 Extend Object* Regular Mk-Brk 1 hour Varies 3 sec. M3, Transform Object
196 Explosive Lightning Missile Weather/Air Instant 2 to 2¥Magery# 1 to 3 sec. Lightning
75 Explosive Fireball Missile Fire Instant 2 to 2¥Magery# 1 to 3 sec. Fireball
118 Explode* Regular Mk-Brk Instant 2 to 6 1 sec. M2, Shatter, Apportation
49 Exchange Bodies* Regular/R-Will Comm. Perm. 120 1 hr. Permanent Possession, Soul Jar
154 Evisceration* Regular/R-HT Necro. Instant 10 5 sec. M3, Apportation, Steal Vitality or IQ
146 Ethereal Body* Regular Movement 10 sec. 8/4 30 sec. 6 Movement spells or M3 and Body of Air
164 Essential Wood Regular Plant Perm. 8 30 sec. 6 Plant spells
189 Essential Water Regular Water Perm. 3/gal. 1 sec. 6 Water spells
179 Essential Fuel/TL Regular Tech Perm. 8/gal. 1 sec. 6 Energy spells
79 Essential Food* Regular Food Perm. 3/meal# 30 sec. 6 Food spells inc. Create Food
75 Essential Flame Area Fire 1 min. 3/2# 3 sec. 6 Fire spells
53 Essential Earth Regular Earth Perm. 8 30 sec. 6 Earth spells
26 Essential Air Area Air Perm. 2 3 sec. 6 Air spells
192 Essential Acid* Regular Water Perm. 8/gal. 1 sec. 6 Acid spells
157 Entrap Spirit Special Necro. 5 min. Varies 1 sec. M1, Soul Jar, Turn Spirit
53 Entombment Regular/R-HT Earth Perm. 10# 3 sec. M2, 5 Earth spells
139 Enthrall Special/R-Will Mind 1 hr. 3/3 1 sec. Forgetfulness, Daze, Slow
60 Ensorcel* Enchantment/R-Spec. Enchantment Perm.# 200¥spell cost – Malefice
141 Enslave* Regular/R-Will Mind Perm. 30 1 sec. Charm, Telepathy
43 Enlarge Other* Regular/R-HT Body 1 hr. 2/+1 SM/S 10 sec. M3, Enlarge
120 Enlarge Object* Regular Mk-Brk 1 hour Varies 3 sec. Extend Object
42 Enlarge* Regular Body 1 hr. 2/+1 SM/S 5 sec. M2, Alter Body
135 Encrypt Regular/R-Spec. Mind 1 week Varies 1 sec. Daze
56 Enchant* Enchantment Enchantment Perm. Varies Varies M2, 1 spell each from 10 colleges
137 Emotion Control Area/R-Will Mind 1 hr. 2 1 sec. Loyalty or Mental Stun
71 Effigy* Enchantment Enchantment Perm. 1,000 – Enchant, Scryfool, Ward
139 Ecstasy* Regular/R-Will Mind 10 sec. 6 3 sec. M2, Emotion Control
107 Echoes of the Past Regular Knowledge/Sound 1 min. 2/2# 10 sec. M2, History, Voices
54 Earthquake Area Earth 1 min. 2/S 30 sec. M2, 6 Earth spells inc. Earth Vision
51 Earth Vision Regular Earth/Know. 30 sec. 2/10 yds.# 1 sec. Shape Earth
52 Earth to Water Regular Earth/Water Perm. 1/25 cu. ft.# 1 sec. M1, Create Water, Shape Earth
51 Earth to Stone Regular Earth Perm. 3/25 cu. ft.# 1 sec. M1, Shape Earth
25 Earth to Air Regular Air/Earth Perm. 5/25 cu. ft.# 2 sec. Create Air, Shape Earth
116 Dye Regular Mk-Brk 2d days Varies 3 sec. Restore, Colors
98 Duplicate* Regular Illusion Indef.# 3/5 lbs. sec.=cost Create Object, Copy
134 Dullness* Regular/R-HT Mind 10 min. 2 to 10/H 1 sec. Any two Dull spells
133 Dull Sense Regular/R-HT Mind 30 min. 1 to 3/H 1 sec. –
188 Dry Spring Regular Water Perm. Varies# 1 min. Destroy Water, Shape Earth
136 Drunkenness Regular/R-Will Mind 1 min. Varies 2 sec. Foolishness, Clumsiness
45 Dream Viewing Regular/R-Will Comm. 1 hr. 2/1 10 sec. Truthsayer or Sleep
45 Dream Sending Regular/R-Will Comm./Mind 1 hr. 3 1 min. Dream Viewing or Sleep
46 Dream Projection Regular Comm./Mind 1 min. 3/3 1 min. Dream Sending
180 Draw Power/TL* Special Tech 1 min. 0/1# 1 sec. Steal Power, 2 spells each from 10 colleges
127 Drain Mana* Area Meta-Spell Perm. 10 1 hr. Dispel Magic, Suspend Mana
130 Drain Magery* Regular/R-Will+M Meta-Spell Perm. 30 10 min. M3, Suspend Magery
62 Doppelgänger* Enchantment Enchantment Perm.# 1,000 – M3, Golem, History, Enslave
108 Divination Information Knowledge Instant 10 1 hr.# History, other spells#
84 Divert Teleport* Blocking/R-spell Gate/Movement Instant Varies 1 sec. M3, Trace Teleport
46 Distill Regular Food/Water Perm. 1/quart 10 sec. Mature, Destroy Water
144 Distant Blow Regular Movement 5 sec. 3/3 3 sec. M2, Apportation
124 Displace Spell Regular/R-spell Meta-Spell Varies Varies 5 sec. Suspend Magic
49 Dispel Possession Regular/R-spell Comm. Instant 10 10 sec. Soul Rider or Possession#
126 Dispel Magic Area/R-spell Meta-Spell Perm. 3 sec.=cost Counterspell and 12 other spells
97 Dispel Illusion Regular/R-spell Illusion Instant 1 1 sec. Control Illusion
99 Dispel Creation Regular/R-spell Illusion Instant 1 or 3# 1 sec. Control Creation
135 Disorient Area/R-Will Mind Indef.# 1 10 sec. Foolishness
120 Disintegrate* Regular Mk-Brk Perm. 1 to 4 1 sec. M2, Shatter, Ruin#
25 Devitalize Air Area Air varies 2 1 sec. Destroy Air
166 Detect Poison Area/Information Protection/Healing Instant 2 2 sec. Sense Danger or Test Food
101 Detect Magic Regular Knowledge Instant 2 5 sec. M1
185 Destroy Water Area Water Perm. 3/S 1 sec. Create Water
24 Destroy Air Area Air Instant 2 1 sec. Create Air
173 Delayed Message Area Sound Indef.# 3# 4 sec. M1, Voices, Sense Life
130 Delay Regular Meta-Spell 2 hrs. 3/3 10 sec. M3, 15 spells
188 Dehydrate Regular/R-HT Water Perm. 1 to 3 2 sec. 5 Water spells inc. Destroy Water
143 Deflect Missile Blocking Movement/Protection Instant 1 1 sec. Apportation
73 Deflect Energy Blocking Fire Instant 1 1 sec. M1, Shape Fire
67 Deflect Enchantment Enchantment Perm. Varies – Enchant
64 Defending Weapon Enchantment Enchantment Perm. Varies – Enchant, Dancing Object
67 Defending Shield Enchantment Enchantment Perm. Varies – Enchant, Grace
77 Decay Regular Food Perm. 1/meal 1 sec. Test Food
42 Decapitation* Regular/R-HT+2 Body Indef. 4 2 sec. M2, Alter Body
36 Debility Regular/R-HT Body 1 min. 1 per ST-/H 1 sec. –
41 Deathtouch Melee Body Instant 1 to 3 1 sec. Wither Limb
149 Death Vision Regular Necro. 1 sec. 2 3 sec. M1
134 Daze Regular/R-HT Mind 1 min. 3/2 2 sec. Foolishness
111 Darkness Area Lt-Dk 1 min. 2/1 1 sec. Continual Light
111 Dark Vision Regular Lt-Dk 1 min. 5/2 1 sec. Night Vision or Infravision
63 Dancing Weapon Enchantment Enchantment Perm. 1,000/lb.# – Enchant, Dancing Object
67 Dancing Shield Enchantment Enchantment Perm. 250/lb.# – Enchant, Dancing Object
144 Dancing Object Regular Movement 1 hr. 4/2 10 sec. M2, Apportation
129 Curse Regular Meta-Spell Special Varies Varies M2, 2 spells each from 10 colleges#
194 Current Special; Area Weather/Water 1 hr. 1/50/S 1 min. 6 Water spells
182 Cure Radiation* Regular Tech/Healing Perm. 1/10 rads# 30 sec. Resist Radiation, Major Healing
91 Cure Disease Regular Healing Instant 4 10 min. Major Healing, Relieve Sickness
71 Crystal Ball Enchantment Enchantment Perm. 1,000 – Enchant, Divination (Crystal-Gazing)
184 Create Water Regular Water Perm. 2/gal. 1 sec. Purify Water
98 Create Warrior Regular Illusion 1 min. Varies 4 sec. Create Servant
190 Create Steam Area Water 5 min.# 2 1 sec. Boil Water
190 Create Spring Regular Water Perm. Varies 1 min. Dry Spring, Shape Water
98 Create Servant Regular Illusion 1 min. Varies 3 sec. M3, IQ 12+, Create Object
163 Create Plant Area Plant Perm. Varies sec.=cost M, Plant Growth
98 Create Object* Regular Illusion Indef.# 2/5 lbs. sec.=cost M2, Create Earth, Perfect Illusion
99 Create Mount Regular Illusion 1 hr. Varies 3 sec. M3, Create Animal
188 Create Ice Regular Water Perm. 2/gal. 1 sec. Freeze
85 Create Gate* Regular Gate 1 min. Varies Varies Control Gate and Teleport, Timeport, or Plane Shift
179 Create Fuel/TL Regular Tech Perm. 1/lb. 30 sec. Seek Fuel, 2 transmutation spells
79 Create Food Regular Food Perm. Varies 30 sec. Cook, Seek Food
72 Create Fire Area Fire 1 min. 2/H 1 sec. Ignite Fire or Seek Fire
28 Create Elemental Special 4 Diff. Perm. Special Special M2, Control Elemental#
51 Create Earth Regular Earth Perm. 2/25 cu. ft. 1 sec. Earth to Stone
84 Create Door Regular Gate 10 sec. 2/10 sq. ft.# 5 sec. Teleport, any one Walk Through spell
98 Create Animal Regular Illusion 1 min. Varies sec.=cost Create Water, Create Object, IQ 12+
23 Create Air Area Air 5 sec.# 1 1 sec. Purify Air or Seek Air
190 Create Acid Regular Water Perm. 4/gal. 2 sec. Create Water, Create Earth
121 Counterspell Regular/R-spell Meta-Spell Instant Varies 5 sec. M1
43 Corpulence* Regular/R-HT Body 10 min. 6/6 3 sec. M2, Create Earth, Create Water, 4 Body spells inc. Alter Body
64 Cornucopia Enchantment Enchantment Perm. 50¥$ value# – Enchant, 2 Weapon Enchantment spells
116 Copy Regular Mk-Brk Perm. 2 plus 1/copy 5 sec. Dye, 1 Accented language
187 Coolness Regular Water/Protection 1 hr. 2/1 10 sec. Cold
195 Cool Area Weather/Air 1 hour 1/10/S 1 min.# Cold, 4 Air spells
78 Cook Regular Food Instant 1 per meal 5 sec. Test Food, Create Fire
173 Converse Regular/R-spell Sound Indef.# 2 1 sec. M1, Garble, Silence
152 Control Zombie Regular/R-spell Necro. Perm. 3 1 sec. Zombie
49 Control Person Regular/R-Will Comm. 1 min. 6/3 10 sec. Soul Rider or Telepathy
40 Control Limb Regular/R-Will Body 5 sec. 3/3# 1 sec. M1, 5 Body spells inc. Spasm
97 Control Illusion Regular/R-spell Illusion Perm. 1 2 sec. Perfect Illusion
85 Control Gate Regular/R-Gate Gate 1 min. 6/3 10 sec. M3, Seek Gate
28 Control Elemental Special 4 Diff. 1 min. Special 2 sec. Summon Elemental#
99 Control Creation Regular/R-spell Illusion Instant 1 2 sec. Create Animal or Create Servant
120 Contract Object* Regular Mk-Brk 1 hour Varies 3 sec. M3, Transform Object
114 Continual Sunlight Area Lt-Dk Varies 3 1 sec. Sunlight
113 Continual Mage Light Regular Lt-Dk Varies Varies 1 sec. Mage Light, Continual Light
110 Continual Light Regular Lt-Dk Varies Varies 1 sec. Light
180 Conduct Power/TL* Special Tech 1 min. 0/1# 1 sec. M1, Seek Power
189 Condense Steam Area Water Perm. 2# 10 sec. Cold or Boil Water
26 Concussion Missile Air/Sound Instant 2 to 2¥Magery# 1 to 3 sec. Shape Air, Thunderclap
122 Conceal Magic Regular Meta-Spell 10 hrs. 1 to 5/S# 3 sec. Detect Magic
162 Conceal Area Plant 1 min. varies# 4 sec. Plant Growth
96 Complex Illusion Area Illusion 1 min. 2/H 1 sec. Sound, Simple Illusion
47 Compel Truth Inform./R-Will Comm. 5 min. 4/2 1 sec. M2, Truthsayer
137 Compel Lie Regular/R-Will Mind/Comm. 5 min. 4/2 1 sec. Emotion Control
48 Communication* Regular Comm. 1 min. 4/4 4 sec. Wizard Eye, Far-Hearing, Voices, Simple Illusion
153 Command Spirit Regular/R-Will Necro. 1 min. Varies 2 sec. Summon Spirit, Turn Spirit
136 Command Blocking/R-Will Mind Instant 2 1 sec. M2, Forgetfulness
110 Colors Regular Lt-Dk 1 min. 2/1 1 sec. Light
74 Cold Regular Fire 1 min. Varies 1 min. Heat
36 Clumsiness Regular/R-HT Body 1 min. 1 to 5/H 1 sec. Spasm
194 Clouds Area Weather/Air 10 min. 1/20/S 10 sec. 2 Water spells, 2 Air spells
148 Cloud-Walking Regular Movement/Weather 1 hr. 3/2 1 sec. Walk on Air, Walk on Water
148 Cloud-Vaulting* Regular Movement/Weather 1 sec./100 mi.# 7 1 sec. M2, Jump, Cloud-Walking
35 Climbing Regular Body 1 min. 1 to 3/S 1 sec. –
94 Cleansing Regular/R-Spec Healing Perm. Varies 3 sec. Minor Healing, Purify Earth
116 Clean Area Mk-Brk Perm. 2 1 sec. Restore
40 Choke Regular/R-HT Body 30 sec. 4 1 sec. M1, 5 Body spells inc. Spasm
139 Charm Regular/R-Will Mind 1 min. 6/3 3 sec. M1, Loyalty, 7 other Mind Control spells
126 Charge Powerstone* Regular Meta-Spell Perm. 3/pt 10 min. M3, Powerstone, Lend Energy
123 Catch Spell* Blocking Meta-Spell Instant 3 1 sec. M2, DX 12+, Return Missile
168 Catch Missile Blocking Protection Instant 2 1 sec. Deflect Missile
39 Cadence Regular Body 1 hr. 5/3 10 sec. Haste, Grace
76 Burning Touch Melee Fire Instant 1 to 3 1 sec. M2, 6 Fire spells inc. Heat
76 Burning Death* Regular/R-HT Fire/Necro. 1 sec. 3/2 3 sec. M2, Heat, Sickness
111 Bright Vision Regular Lt-Dk 1 min. 2/1 1 sec. Keen Vision or 5 Light spells; no Blindness
189 Breathe Water Regular Water/Air 1 min. 4/2 1 sec. Create Air, Destroy Water
192 Breathe Steam* Regular Water 1 sec. 1 to 4 2 sec. M1, Steam Jet, Resist Fire
182 Breathe Radiation* Regular Tech 1 sec. 1 to 4 2 sec. M2, Radiation Jet
76 Breathe Fire* Regular Fire 1 sec. 1 to 4 2 sec. M1, Flame Jet, Resist Fire
26 Breathe Air Regular Air/Water 1 min. 4/2 1 sec. Create Water, Destroy Air
134 Bravery Area/R-Will-1 Mind 1 hr. 2 1 sec. Fear
47 Borrow Skill Regular Comm. 1 min. 4/3 3 sec. Lend Skill Page Spell Name Class College Duration Energy Time to cast Prerequisites Prerequisite Count
46 Borrow Language Regular Comm. 1 min. 3/1 3 sec. Lend Language
189 Boil Water Regular Water Perm. Varies 10 sec. Shape Water, Heat 37 Boost Attribute Regular or Blocking Body/Mind Instant 1 to 5 none Varies Varies
88 Body-Reading Information/R-Will Healing Instant 2 30 sec. Sense Life or Awaken
165 Body of Wood Regular/R-HT Plant 1 min. 7/3 5 sec. M2, Plant Form
27 Body of Wind Regular/R-HT Air 1 min. 8/4 2 sec. M3, Body of Air, Windstorm, 1 spell each from 5 colleges#
185 Body of Water Regular/R-HT Water 1 min. 5/2 5 sec. Shape Water
54 Body of Stone* Regular/R-HT Earth 1 min. 10/5 5 sec. Stone to Flesh
165 Body of Slime Regular/R-HT Plant 1 min. 6/2 5 sec. M2, Plant Form, Shape Water
114 Body of Shadow Regular/R-HT Lt-Dk 1 min. 6/3 5 sec. M2, Shape Darkness
183 Body of Metal* Regular/R-HT Tech 1 min. 12/6 5 sec. M2, Shape Metal 183 Body of Plastic Regular/R-HT Tech 1 min. 10/5 5 sec. M2, Shape Plastic
198 Body of Lightning* Regular/R-HT Weather/Air 1 min. 12/4 5 sec. M2, Lightning
189 Body of Ice* Regular/R-HT Water 1 min. 7/3 5 sec. M2, Body of Water, Freeze
76 Body of Flames* Regular/R-HT Fire 1 min. 12/4 5 sec. Breathe Fire
24 Body of Air Regular/R-HT Air 1 min. 4/1 5 sec. Shape Air
113 Blur Regular Lt-Dk 1 min. 1 to 5/S 2 sec. Darkness or Gloom
162 Blossom Area Plant 1 hour 2 5 min. Plant Growth
166 Block Blocking Protection Instant 1/DB+# 1 sec. M1
148 Blink Other* Blocking Movement/Gate Instant 2 1 sec. Blink
148 Blink Blocking Movement/Gate Instant 2 1 sec. Teleport
162 Blight Area Plant 1 crop/season 1 5 min. Plant Growth
161 Bless Plants Area Plant 1 crop/season 1 5 min. Heal Plant
129 Bless Regular Meta-Spell Special Varies min.=cost M2, 2 spells each from 10 colleges#
66 Blank Spell Arrow* Enchantment Enchantment Perm. 30¥capacity# – Spell Arrow
168 Bladeturning Regular/R-Spec Protection 1 min. 2/2 1 sec. Shield or Turn Blade
112 Blackout Area Lt-Dk 1 min. 2/1 1 sec. Darkness
158 Bind Spirit* Regular/R-Will Necro. Perm. Varies 5 min. Command Spirit, Soul Jar
134 Berserker Regular/R-Will Mind 10 min.# 3/2 4 sec. Bravery
30 Beast Summoning Regular Animal 1 min. 3/2# 1 sec. Beast-Soother
31 Beast Speech Regular Animal 1 min. 4/2 1 sec. Beast Summoning
29 Beast-Soother Regular Animal Perm.# 1 to 3 1 sec. Persuasion or Animal Empathy advantage
32 Beast Seeker Information Animal Instant 3# 1 sec. Seeker or Beast Summoning and 2 Seek spells
30 Beast-Rouser Regular Animal 1 hour# 1 to 3 1 sec. Vexation or Animal Empathy
32 Beast Possession Regular/R-Will Animal 1 min. 6/2 5 sec. Rider Within or Possession
30 Beast Link Regular Animal Special 3 5 sec. Beast Summoning
83 Beacon Area Gate/Movement 24 hrs. 10/H 30 sec. Teleport, Timeport, or Plane Shift
156 Banish Spec./R-Will Necro. Instant Varies 5 sec. M, 1 spell each from 10 colleges
62 Bane Enchantment Enchantment Perm. 100 – Enchant
197 Ball of Lightning Regular Weather/Air 1 min. 2 to 6/H 1 to 3 sec. Apportation, Lightning
39 Balance Regular Body 1 min. 5/3 1 sec. Grace
115 Awaken Craft Spirit Regular Mk-Brk/Necro. 1 min. 3/1 5 sec. Inspired Creation, Sense Spirit
178 Awaken Computer/TL Regular Tech 1 hr. Varies 10 sec. Animation, Wisdom
90 Awaken Area Healing Instant 1 1 sec. Lend Vitality
140 Avoid Area Mind 1 hr. 3/3 1 min. Hide, Fear, Forgetfulness
101 Aura Information Knowledge Instant 3 1 sec. Detect Magic
69 Attune Enchantment Enchantment Perm. 100 – Bane
105 Astral Vision* Regular Knowledge/Necro. 1 min. 4/2 1 sec. Sense Spirit, See Invisible 169 Atmosphere Dome Area Protection/Air 6 hrs. 4/H 1 sec. Purify Air, Weather Dome
159 Astral Block Area Necro. 10 min. 4/2# 2 sec. Summon Spirit, Repel Spirits
167 Armor Regular Protection 1 min. Varies 1 sec. Shield
165 Arboreal Immurement Regular/R-HT Plant Indef.# 8# 3 sec. M2, Walk Through Wood
142 Apportation Reg./R-Will Movement 1 min. Varies 1 sec. M1
150 Animation* Regular Necro. 1 min. Varies 5 sec. Summon Spirit
154 Animate Shadow Regular/R-HT Necro. 10 sec. 4/4 2 sec. Skull-Spirit, Shape Darkness
164 Animate Plant Regular Plant 1 min. Varies 5 sec. 7 Plant spells
117 Animate Object* Regular/R-Spec Mk-Brk 1 min. Varies 3 sec. M2, 3 Shape spells
177 Animate Machine/TL Regular/R-Will Tech 1 min. Varies Varies Machine Control, Animation or Animate Object
30 Animal Control Regular/R-Will Animal 1 min. Varies 1 sec. Beast-Soother
106 Ancient History Information Knowledge Instant Varies min.=cost History
102 Analyze Magic Inform./R-spell Knowledge Instant 8 1 hr. Identify Spell
58 Amulet Enchantment Enchantment Perm. 50/pt. MR – Talisman for appropriate spell
39 Ambidexterity Regular Body 1 min. 3/2 1 sec. Grace
41 Alter Voice Regular/R-HT Body/Sound 1 hr. 2/2 1 min. 4 Body spells, 4 Sound spells
41 Alter Visage Regular/R-HT Body 1 hr. 4/3 1 min. Shapeshifting or Perfect Illusion, and 8 Body spells
55 Alter Terrain* Area Earth 2d days 1# 10 sec. M3, all four elemental Shape spells, Shape Stone
41 Alter Body Regular/R-HT Body 1 hr. 8/6 2 min. Alter Visage
133 Alertness* Regular Mind 10 min. 2 to 10/H 1 sec. Any two Keen spells
100 Alarm Regular Knowledge 1 week 1 1 sec. Tell Time
26 Air Vortex Area/R-HT or DX Air/Movement 10 sec. 8/3 2 sec. M2, Body of Air, Windstorm
24 Air Vision Regular Air/Knowledge 1 min. 1 per mi./H 1 sec. Shape Air
24 Air Jet Regular Air 1 sec. 1 to 3/S 1 sec. Shape Air
40 Agonize Regular/R-HT Body 1 min. 8/6 1 sec. M2, Sensitize
154 Age* Regular/R-HT Necro. Perm. 10 to 50 1 min. Youth or 6 other Necromantic spells
151 Affect Spirits Regular Necro. 1 min. 4/2 2 sec. Solidify
58 Adjustable Clothing Enchantment Enchantment Perm. Varies – Enchant, Reshape
192 Acid Jet Regular Water 1 sec. 1 to 3 1 sec. M2, Water Jet, Create Acid
191 Acid Ball Missile Water Instant 1 to Magery# 1 to 3 sec. M2, Create Acid
65 Accuracy Enchantment Enchantment Perm. Varies – Enchant and 5 Air spells
86 Accelerate Time* Area/R-Spec. Gate 1 min. Varies 2 sec. M2, IQ 13+, 2 spells each from 10 colleges
"""

let compare = """
Affect Spirits (Clerical, "C"): [PI3]
    WAS Affect Spirits Regular Necro. 1 min. 4/2 2 sec. Solidify
Agonize (Wizardly, "W"): [M2, Sensitize]
    WAS Agonize Regular/R-HT Body 1 min. 8/6 1 sec. M2, Sensitize
Air Jet (Wizardly, "W"): [Shape Air]
    WAS Air Jet Regular Air 1 sec. 1 to 3/S 1 sec. Shape Air
Alertness (Wizardly, "W*"): [2 Keen Sense spells]
    WAS Alertness* Regular Mind 10 min. 2 to 10/H 1 sec. Any two Keen spells
Ambidexterity (Wizardly, "W"): [Grace]
    WAS Ambidexterity Regular Body 1 min. 3/2 1 sec. Grace
Analyze Magic (Wizardly, "W*"): [Identify Spell]
    WAS Analyze Magic Inform./R-spell Knowledge Instant 8 1 hr. Identify Spell
Animal Control (Druidic, "D"): [PI2]
    WAS Animal Control Regular/R-Will Animal 1 min. Varies 1 sec. Beast-Soother
Apportation (Wizardly, "W"): [M1]
    WAS Apportation Reg./R-Will Movement 1 min. Varies 1 sec. M1
Arboreal Immurement (Druidic, "D"): [PI5]
    WAS Arboreal Immurement Regular/R-HT Plant Indef.# 8# 3 sec. M2, Walk Through Wood
Armor (Clerical, "C"): [PI1] or (Wizardly, "W"): [Shield]
    WAS Armor Regular Protection 1 min. Varies 1 sec. Shield
Astral Block (Clerical, "C"): [PI4] or (Wizardly, "W"): [Repel Spirits, Summon Spirit]
    WAS Astral Block Area Necro. 10 min. 4/2# 2 sec. Summon Spirit, Repel Spirits
Astral Vision (Clerical, "C"): [PI3] or (Wizardly, "W*"): [Sense Spirit, See Invisible]
    WAS Astral Vision* Regular Knowledge/Necro. 1 min. 4/2 1 sec. Sense Spirit, See Invisible 169 Atmosphere Dome Area Protection/Air 6 hrs. 4/H 1 sec. Purify Air, Weather Dome
Aura (Clerical, "C"): [PI1] or (Wizardly, "W*"): [Detect Magic]
    WAS Aura Information Knowledge Instant 3 1 sec. Detect Magic
Awaken (Clerical, "C"): [PI2]
    WAS Awaken Area Healing Instant 1 1 sec. Lend Vitality
Awaken (Clerical, "C"): [PI2]
    WAS Awaken Computer/TL Regular Tech 1 hr. Varies 10 sec. Animation, Wisdom
Awaken (Clerical, "C"): [PI2]
    WAS Awaken Craft Spirit Regular Mk-Brk/Necro. 1 min. 3/1 5 sec. Inspired Creation, Sense Spirit
Balance (Wizardly, "W"): [Grace]
    WAS Balance Regular Body 1 min. 5/3 1 sec. Grace
Banish (Clerical, "C"): [PI4] or (Wizardly, "W"): [M1, 1 spell from 10 colleges]
    WAS Banish Spec./R-Will Necro. Instant Varies 5 sec. M, 1 spell each from 10 colleges
Beast Link (Druidic, "D"): [PI2]
    WAS Beast Link Regular Animal Special 3 5 sec. Beast Summoning
Beast Possession (Druidic, "D"): [PI4]
    WAS Beast Possession Regular/R-Will Animal 1 min. 6/2 5 sec. Rider Within or Possession
Beast-Rouser (Druidic, "D"): [PI1]
    WAS Beast-Rouser Regular Animal 1 hour# 1 to 3 1 sec. Vexation or Animal Empathy
Beast Seeker (Druidic, "D"): [PI2]
    WAS Beast Seeker Information Animal Instant 3# 1 sec. Seeker or Beast Summoning and 2 Seek spells
Beast-Soother (Druidic, "D"): [PI1]
    WAS Beast-Soother Regular Animal Perm.# 1 to 3 1 sec. Persuasion or Animal Empathy advantage
Beast Speech (Druidic, "D"): [PI2]
    WAS Beast Speech Regular Animal 1 min. 4/2 1 sec. Beast Summoning
Beast Summoning (Druidic, "D"): [PI3]
    WAS Beast Summoning Regular Animal 1 min. 3/2# 1 sec. Beast-Soother
Blackout (Wizardly, "W"): [Darkness]
    WAS Blackout Area Lt-Dk 1 min. 2/1 1 sec. Darkness
Bladeturning (Wizardly, "W"): [Shield]
    WAS Bladeturning Regular/R-Spec Protection 1 min. 2/2 1 sec. Shield or Turn Blade
Bless (Clerical, "C"): [PI5]
    WAS Bless Regular Meta-Spell Special Varies min.=cost M2, 2 spells each from 10 colleges#
Bless (Clerical, "C"): [PI5]
    WAS Bless Plants Area Plant 1 crop/season 1 5 min. Heal Plant
Blink (Wizardly, "W"): [M3, IQ 13+, 1 spell from 10 colleges]
    WAS Blink Blocking Movement/Gate Instant 2 1 sec. Teleport
Blink Other (Wizardly, "W"): [Blink]
    WAS Blink Other* Blocking Movement/Gate Instant 2 1 sec. Blink
Blur (Wizardly, "W"): [Darkness]
    WAS Blur Regular Lt-Dk 1 min. 1 to 5/S 2 sec. Darkness or Gloom
Borrow Language (Wizardly, "W*"): [Lend Language]
    WAS Borrow Language Regular Comm. 1 min. 3/1 3 sec. Lend Language
Borrow Skill (Wizardly, "W*"): [Lend Skill]
    WAS Borrow Skill Regular Comm. 1 min. 4/3 3 sec. Lend Skill Page Spell Name Class College Duration Energy Time to cast Prerequisites Prerequisite Count
Bravery (Clerical, "C"): [PI1] or (Wizardly, "W*"): [Fear]
    WAS Bravery Area/R-Will-1 Mind 1 hr. 2 1 sec. Fear
Breathe Water (Clerical, "C"): [PI3] or (Druidic, "D"): [PI3] or (Wizardly, "W"): [Create Air, Destroy Water]
    WAS Breathe Water Regular Water/Air 1 min. 4/2 1 sec. Create Air, Destroy Water
Bright Vision (Wizardly, "W"): [Keen Vision or 5 L&D spells]
    WAS Bright Vision Regular Lt-Dk 1 min. 2/1 1 sec. Keen Vision or 5 Light spells; no Blindness
Burning Touch (Wizardly, "W"): [M2, 6 Fire spells including Heat]
    WAS Burning Touch Melee Fire Instant 1 to 3 1 sec. M2, 6 Fire spells inc. Heat
Charm (Wizardly, "W*"): [M1/BT1, Loyalty, 7 other Mind Control spells]
    WAS Charm Regular/R-Will Mind 1 min. 6/3 3 sec. M1, Loyalty, 7 other Mind Control spells
Cleansing (Clerical, "C"): [PI1]
    WAS Cleansing Regular/R-Spec Healing Perm. Varies 3 sec. Minor Healing, Purify Earth
Climbing (Wizardly, "W"): [M0]
    WAS Climbing Regular Body 1 min. 1 to 3/S 1 sec. –
Clumsiness (Wizardly, "W"): [Spasm]
    WAS Clumsiness Regular/R-HT Body 1 min. 1 to 5/H 1 sec. Spasm
Cold (Wizardly, "W"): [Heat]
    WAS Cold Regular Fire 1 min. Varies 1 min. Heat
Colors (Wizardly, "W"): [Light]
    WAS Colors Regular Lt-Dk 1 min. 2/1 1 sec. Light
Command (Clerical, "C"): [PI2] or (Wizardly, "W*"): [M2/ BT2, Forgetfulness]
    WAS Command Blocking/R-Will Mind Instant 2 1 sec. M2, Forgetfulness
Command Spirit (Clerical, "C"): [PI3] or (Wizardly, "W"): [Summon Spirit, Turn Spirit]
    WAS Command Spirit Regular/R-Will Necro. 1 min. Varies 2 sec. Summon Spirit, Turn Spirit
Compel Truth (Clerical, "C"): [PI2] or (Wizardly, "W*"): [M2/ BT2, Truthsayer]
    WAS Compel Truth Inform./R-Will Comm. 5 min. 4/2 1 sec. M2, Truthsayer
Complex Illusion (Wizardly, "W"): [Simple Illusion, Sound]
    WAS Complex Illusion Area Illusion 1 min. 2/H 1 sec. Sound, Simple Illusion
Conceal (Druidic, "D"): [PI3]
    WAS Conceal Area Plant 1 min. varies# 4 sec. Plant Growth
Conceal (Druidic, "D"): [PI3]
    WAS Conceal Magic Regular Meta-Spell 10 hrs. 1 to 5/S# 3 sec. Detect Magic
Concussion (Wizardly, "W*"): [Shape Air, Thunderclap]
    WAS Concussion Missile Air/Sound Instant 2 to 2¥Magery# 1 to 3 sec. Shape Air, Thunderclap
Continual Light (Clerical, "C"): [PI2] or (Wizardly, "W"): [Light]
    WAS Continual Light Regular Lt-Dk Varies Varies 1 sec. Light
Control Gate (Wizardly, "W"): [M3, Seek Gate]
    WAS Control Gate Regular/R-Gate Gate 1 min. 6/3 10 sec. M3, Seek Gate
Control Illusion (Wizardly, "W"): [Perfect Illusion]
    WAS Control Illusion Regular/R-spell Illusion Perm. 1 2 sec. Perfect Illusion
Control Person (Wizardly, "W*"): [Soul Rider or Telepathy]
    WAS Control Person Regular/R-Will Comm. 1 min. 6/3 10 sec. Soul Rider or Telepathy
Cook (Wizardly, "W"): [Test Food, Create Fire]
    WAS Cook Regular Food Instant 1 per meal 5 sec. Test Food, Create Fire
Coolness (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [Cold]
    WAS Coolness Regular Water/Protection 1 hr. 2/1 10 sec. Cold
Copy (Wizardly, "W"): [5 M&B spells including Restore, no Illiteracy]
    WAS Copy Regular Mk-Brk Perm. 2 plus 1/copy 5 sec. Dye, 1 Accented language
Counterspell (Wizardly, "W"): [M1]
    WAS Counterspell Regular/R-spell Meta-Spell Instant Varies 5 sec. M1
Create Air (Wizardly, "W"): [Purify Air]
    WAS Create Air Area Air 5 sec.# 1 1 sec. Purify Air or Seek Air
Create Animal (Druidic, "D"): [PI4]
    WAS Create Animal Regular Illusion 1 min. Varies sec.=cost Create Water, Create Object, IQ 12+
Create Earth (Wizardly, "W"): [Earth to Stone]
    WAS Create Earth Regular Earth Perm. 2/25 cu. ft. 1 sec. Earth to Stone
Create Fire (Wizardly, "W"): [Ignite Fire or Seek Fire]
    WAS Create Fire Area Fire 1 min. 2/H 1 sec. Ignite Fire or Seek Fire
Create Food (Clerical, "C"): [PI3] or (Wizardly, "W"): [Cook, Seek Food]
    WAS Create Food Regular Food Perm. Varies 30 sec. Cook, Seek Food
Create Plant (Druidic, "D"): [PI3]
    WAS Create Plant Area Plant Perm. Varies sec.=cost M, Plant Growth
Create Water (Clerical, "C"): [PI2] or (Wizardly, "W"): [Purify Water]
    WAS Create Water Regular Water Perm. 2/gal. 1 sec. Purify Water
Cure Disease (Clerical, "C"): [PI3] or (Druidic, "D"): [PI2]
    WAS Cure Disease Regular Healing Instant 4 10 min. Major Healing, Relieve Sickness
Curse (Clerical, "C"): [PI5]
    WAS Curse Regular Meta-Spell Special Varies Varies M2, 2 spells each from 10 colleges#
Dark Vision (Wizardly, "W"): [Infravision or Night Vision]
    WAS Dark Vision Regular Lt-Dk 1 min. 5/2 1 sec. Night Vision or Infravision
Darkness (Wizardly, "W"): [Continual Light]
    WAS Darkness Area Lt-Dk 1 min. 2/1 1 sec. Continual Light
Daze (Wizardly, "W*"): [Foolishness]
    WAS Daze Regular/R-HT Mind 1 min. 3/2 2 sec. Foolishness
Death Vision (Wizardly, "W"): [M1]
    WAS Death Vision Regular Necro. 1 sec. 2 3 sec. M1
Deathtouch (Wizardly, "W"): [Wither Limb]
    WAS Deathtouch Melee Body Instant 1 to 3 1 sec. Wither Limb
Debility (Wizardly, "W"): [M0]
    WAS Debility Regular/R-HT Body 1 min. 1 per ST-/H 1 sec. –
Decay (Wizardly, "W"): [Test Food]
    WAS Decay Regular Food Perm. 1/meal 1 sec. Test Food
Deflect Energy (Wizardly, "W"): [M1, Shape Fire]
    WAS Deflect Energy Blocking Fire Instant 1 1 sec. M1, Shape Fire
Deflect Missile (Wizardly, "W"): [Apportation]
    WAS Deflect Missile Blocking Movement/Protection Instant 1 1 sec. Apportation
Dehydrate (Wizardly, "W"): [5 Water spells including Destroy Water]
    WAS Dehydrate Regular/R-HT Water Perm. 1 to 3 2 sec. 5 Water spells inc. Destroy Water
Delayed Message (Wizardly, "W*"): [M1/BT1, Sense Life, Voices]
    WAS Delayed Message Area Sound Indef.# 3# 4 sec. M1, Voices, Sense Life
Destroy Air (Wizardly, "W"): [Create Air]
    WAS Destroy Air Area Air Instant 2 1 sec. Create Air
Destroy Water (Wizardly, "W"): [Create Water]
    WAS Destroy Water Area Water Perm. 3/S 1 sec. Create Water
Detect Magic (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W*"): [M1/BT1]
    WAS Detect Magic Regular Knowledge Instant 2 5 sec. M1
Detect Poison (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1]
    WAS Detect Poison Area/Information Protection/Healing Instant 2 2 sec. Sense Danger or Test Food
Dispel Illusion (Wizardly, "W"): [Control Illusion]
    WAS Dispel Illusion Regular/R-spell Illusion Instant 1 1 sec. Control Illusion
Dispel Magic (Clerical, "C"): [PI4] or (Druidic, "D"): [PI4] or (Wizardly, "W"): [Counterspell, any 12 other spells]
    WAS Dispel Magic Area/R-spell Meta-Spell Perm. 3 sec.=cost Counterspell and 12 other spells
Dispel Possession (Clerical, "C"): [PI3]
    WAS Dispel Possession Regular/R-spell Comm. Instant 10 10 sec. Soul Rider or Possession#
Divert Teleport (Wizardly, "W"): [M3, Trace Teleport]
    WAS Divert Teleport* Blocking/R-spell Gate/Movement Instant Varies 1 sec. M3, Trace Teleport
Drunkenness (Wizardly, "W*"): [Foolishness, Clumsiness]
    WAS Drunkenness Regular/R-Will Mind 1 min. Varies 2 sec. Foolishness, Clumsiness
Dull Sense (Wizardly, "W*"): [M0/BT1]
    WAS Dull Sense Regular/R-HT Mind 30 min. 1 to 3/H 1 sec. –
Dullness (Wizardly, "W*"): [2 Dull Sense spells]
    WAS Dullness* Regular/R-HT Mind 10 min. 2 to 10/H 1 sec. Any two Dull spells
Earth to Air (Wizardly, "W"): [Create Air, Shape Earth]
    WAS Earth to Air Regular Air/Earth Perm. 5/25 cu. ft.# 2 sec. Create Air, Shape Earth
Earth to Stone (Wizardly, "W"): [M1, Shape Earth]
    WAS Earth to Stone Regular Earth Perm. 3/25 cu. ft.# 1 sec. M1, Shape Earth
Earth Vision (Druidic, "D"): [PI3] or (Wizardly, "W*"): [Shape Earth]
    WAS Earth Vision Regular Earth/Know. 30 sec. 2/10 yds.# 1 sec. Shape Earth
Earthquake (Clerical, "C"): [PI5] or (Druidic, "D"): [PI6]
    WAS Earthquake Area Earth 1 min. 2/S 30 sec. M2, 6 Earth spells inc. Earth Vision
Entombment (Druidic, "D"): [PI5] or (Wizardly, "W"): [M2, 5 Earth spells]
    WAS Entombment Regular/R-HT Earth Perm. 10# 3 sec. M2, 5 Earth spells
Entrap Spirit (Clerical, "C"): [PI5] or (Wizardly, "W"): [M1, 7 Necromantic spells including Turn Spirit]
    WAS Entrap Spirit Special Necro. 5 min. Varies 1 sec. M1, Soul Jar, Turn Spirit
Essential Food (Clerical, "C"): [PI4] or (Wizardly, "W"): [6 Food spells including Create Food]
    WAS Essential Food* Regular Food Perm. 3/meal# 30 sec. 6 Food spells inc. Create Food
Ethereal Body (Wizardly, "W"): [M3, 6 Movement spells]
    WAS Ethereal Body* Regular Movement 10 sec. 8/4 30 sec. 6 Movement spells or M3 and Body of Air
Explosive Fireball (Wizardly, "W"): [Fireball]
    WAS Explosive Fireball Missile Fire Instant 2 to 2¥Magery# 1 to 3 sec. Fireball
Explosive Lightning (Wizardly, "W"): [Lightning]
    WAS Explosive Lightning Missile Weather/Air Instant 2 to 2¥Magery# 1 to 3 sec. Lightning
Extinguish Fire (Druidic, "D"): [PI1] or (Wizardly, "W"): [Ignite Fire]
    WAS Extinguish Fire Regular Fire Perm. 3 1 sec. Ignite Fire
Far-Feeling (Wizardly, "W*"): [M1/BT1]
    WAS Far-Feeling Regular Knowledge 1 min. 3/1 3 sec. M1
Far-Hearing (Wizardly, "W*"): [M1/BT1, 4 Sound spells]
    WAS Far-Hearing Information Sound/Know. 1 min. 4/2 3 sec. M1, 4 Sound spells, no Deafness or Hard of Hearing
Far-Tasting (Wizardly, "W*"): [M1, Seek Food]
    WAS Far-Tasting Regular Food/Knowledge 1 min. 3/1 3 sec. M1, no anosmia, Seek Food or Seek Air
Fascinate (Wizardly, "W*"): [Daze]
    WAS Fascinate Regular or Blocking/R-Will Mind Indef.# 4 1 sec. Daze
Fasten (Wizardly, "W"): [Knot]
    WAS Fasten Regular/R-DX Mk-Brk Perm. 3# 1 sec. Knot
Fear (Wizardly, "W*"): [Sense Emotion]
    WAS Fear Area/R-Will Mind 10 min. 1 1 sec. Sense Emotion or Empathy
Final Rest (Clerical, "C"): [PI1]
    WAS Final Rest Regular Healing/Necro. Perm. 20 10 min.# M1 or Spirit Empathy
Find Direction (Druidic, "D"): [PI1] or (Wizardly, "W*"): [M1/BT1]
    WAS Find Direction Information Knowledge Instant 2 1 sec. M1
Find Weakness (Wizardly, "W"): [1 Air, 1 Earth, 1 Fire, 1 Water spell]
    WAS Find Weakness Information Mk-Brk Instant 1# 2 sec. 1 spell of each four elements
Fire Cloud (Wizardly, "W"): [Fireball, Shape Air]
    WAS Fire Cloud Area Fire 10 sec. 1 to 5/S 1 to 5 sec. Shape Air, Fireball
Fireball (Wizardly, "W"): [M1, Create Fire, Shape Fire]
    WAS Fireball Missile Fire Instant 1 to Magery# 1 to 3 sec. M1, Create Fire, Shape Fire
Fireproof (Druidic, "D"): [PI2] or (Wizardly, "W"): [Extinguish Fire]
    WAS Fireproof Area Fire 1 day 3# 5 min. Extinguish Fire
Flame Jet (Wizardly, "W"): [Create Fire, Shape Fire]
    WAS Flame Jet Regular Fire 1 sec. 1 to 3/S 1 sec. Create Fire, Shape Fire
Flaming Missiles (Wizardly, "W"): [Flaming Weapon]
    WAS Flaming Missiles Regular Fire 1 min. 4/2# 3 sec. Flaming Weapon
Flaming Weapon (Clerical, "C"): [PI3] or (Wizardly, "W"): [M2, Heat]
    WAS Flaming Weapon Regular Fire 1 min. 4/1 2 sec. M2, Heat
Flesh to Stone (Wizardly, "W"): [Earth to Stone]
    WAS Flesh to Stone Regular/R-HT Earth Perm. 10# 2 sec. Earth to Stone
Flight (Wizardly, "W"): [M2, Levitation]
    WAS Flight* Regular Movement 1 min. 5/3 2 sec. M2, Levitation
Fog (Druidic, "D"): [PI2]
    WAS Fog Area Weather/Water 1 min. 2/H 1 sec. Shape Water
Foolishness (Wizardly, "W*"): [M0/BT1, IQ 12+]
    WAS Foolishness Regular/R-Will Mind 1 min. 1 per IQ-/H 1 sec. IQ 12+
Forest Warning (Druidic, "D"): [PI3]
    WAS Forest Warning Area Plant 10 hrs. 2#/S 1 sec. 4 Plant spells
Forgetfulness (Wizardly, "W*"): [M1/BT1, Foolishness]
    WAS Forgetfulness Regular/R-Will Mind 1 hr. 3/3 10 sec. M1, Foolishness or skill
Frailty (Wizardly, "W"): [Lend Energy]
    WAS Frailty Regular/R-HT Body 1 min. 2 per HT-/S# 1 sec. Lend Energy
Freeze (Druidic, "D"): [PI3] or (Wizardly, "W"): [Shape Water]
    WAS Freeze Regular Water Perm. Varies 10 sec. Shape Water
Frostbite (Druidic, "D"): [PI4] or (Wizardly, "W"): [Cold, Freeze]
    WAS Frostbite Regular/R-HT Water Perm. 1 to 3 3 sec. Frost, Freeze
Garble (Wizardly, "W*"): [Voices]
    WAS Garble Regular/R-Will Sound 1 min. 4/2 1 sec. Voices
Geyser (Druidic, "D"): [PI6]
    WAS Geyser* Area Water 1 sec. 5/2 5 sec. 6 Water spells inc. Create Well and either 4 Earth or Fire spells
Gift of Letters (Clerical, "C"): [PI4] or (Wizardly, "W*"): [Borrow Language, 3 written languages]
    WAS Gift of Letters* Regular Comm. 1 min. Varies 1 sec. Borrow Language, 3 languages at Accented
Gift of Tongues (Clerical, "C"): [PI4] or (Wizardly, "W*"): [Borrow Language, 3 spoken languages]
    WAS Gift of Tongues* Regular Comm. 1 min. Varies 1 sec. Borrow Language, 3 languages at Accented
Glass Wall (Wizardly, "W*"): [5 Knowledge spells or Earth Vision]
    WAS Glass Wall Regular Knowledge 1 min. 4/2 1 sec. 5 Knowledge spells or Earth Vision
Glow (Clerical, "C"): [PI2] or (Wizardly, "W"): [Continual Light]
    WAS Glow Area Lt-Dk Varies Varies Varies Continual Light
Glue (Wizardly, "W"): [Haste]
    WAS Glue Area Movement 10 min. 3/S 1 sec. Haste
Grace (Wizardly, "W"): [Clumsiness]
    WAS Grace Regular Body 1 min. 4 per DX+/S 1 sec. Clumsiness
Grace (Wizardly, "W"): [Clumsiness]
    WAS Graceful Weapon Enchantment Enchantment Perm. 150/lb. – Enchant, Apportation
Grease (Wizardly, "W"): [Haste]
    WAS Grease Area Movement 10 min. 3/S 1 sec. Haste
Great Haste (Wizardly, "W"): [M1, IQ 12+, Haste]
    WAS Great Haste* Regular Movement 10 sec. 5# 3 sec. M1, Haste, IQ 12+
Great Healing (Clerical, "C"): [PI3]
    WAS Great Healing Regular Healing Perm. 20 1 min. M3, Major Healing
Great Voice (Clerical, "C"): [PI2] or (Wizardly, "W*"): [Thunderclap, Voices]
    WAS Great Voice Regular Sound 1 min. 3/1 2 sec. Voices, Thunderclap
Great Ward (Wizardly, "W"): [M2, Ward]
    WAS Great Ward Block/R-spell Meta-Spell Instant 1 per subject# none M2, Ward
Hail (Druidic, "D"): [PI4]
    WAS Hail Area Weather/Water 1 min. 1/5/S# 1 sec. Snow
Haste (Wizardly, "W"): [M0]
    WAS Haste Regular Movement 1 min. 2/pt./H 2 sec. –
Hawk Vision (Druidic, "D"): [PI1] or (Wizardly, "W"): [Keen Vision or 5 L&D spells]
    WAS Hawk Vision Regular Lt-Dk 1 min. 2/lvl./H# 2 sec. Keen Vision or 5 Light spells; no Blindness or Bad Sight
Healing Slumber (Clerical, "C"): [PI2]
    WAS Healing Slumber Regular/R-# Healing 8 hrs.# 6 or 10 30 sec. M2, Sleep, Minor Healing
Heat (Wizardly, "W"): [Create Fire, Shape Fire]
    WAS Heat Regular Fire 1 min. Varies 1 min. Create Fire, Shape Fire
Hide (Wizardly, "W"): [Blur or Forgetfulness]
    WAS Hide Regular Lt-Dk 1 hr. 1 to 5/S 5 sec. Blur or Forgetfulness
Hide Emotion (Wizardly, "W*"): [Sense Emotion]
    WAS Hide Emotion Regular Comm. 1 hour 2/2 1 sec. Sense Emotion
Hide (Wizardly, "W"): [Blur or Forgetfulness]
    WAS Hide Object Regular Gate 1 hr. 1/lb./S 10 sec. Hideaway, Teleport
Hide Path (Druidic, "D"): [PI2]
    WAS Hide Path Regular Plant 1 min. 2/1 1 sec. Heal Plant
Hide Thoughts (Clerical, "C"): [PI2] or (Wizardly, "W*"): [Truthsayer or Hide Emotion]
    WAS Hide Thoughts Regular Comm. 10 min. 3/1 1 sec. Truthsayer or Hide Emotion
Hide (Wizardly, "W"): [Blur or Forgetfulness]
    WAS Hideaway Enchantment Enchantment Perm. 50# – Enchant, Create Object, Lighten
Hinder (Wizardly, "W"): [Clumsiness or Haste]
    WAS Hinder Regular Body/Movement 1 min. 1 to 4/S 1 sec. Haste or Clumsiness
History (Wizardly, "W*"): [Trace]
    WAS History Information Knowledge Instant Varies sec.=cost Trace
Hold Breath (Wizardly, "W"): [M1, Vigor]
    WAS Hold Breath Regular Body 1 min. 4/2 1 sec. M1, Vigor
Hush (Wizardly, "W*"): [Silence]
    WAS Hush Regular/R-Will Sound 10 sec.# 2/1 2 sec. Silence
Hybrid Control (Druidic, "D"): [PI3]
    WAS Hybrid Control* Regular/R-Will Animal 1 min. 6/3 1 sec. 2 Control spells#
Ice Dagger (Wizardly, "W"): [Ice Sphere or Water Jet]
    WAS Ice Dagger Missile Water Instant 1 to Magery# 1 to 3 sec. Ice Sphere or Water Jet
Ice Sphere (Wizardly, "W"): [Shape Water]
    WAS Ice Sphere Missile Water Instant 1 to Magery# 1 to 3 sec. Shape Water
Icy Missiles (Wizardly, "W"): [Icy Weapon]
    WAS Icy Missiles Regular Water 1 min. 4/2 3 sec. Icy Weapon
Icy Weapon (Wizardly, "W"): [Create Water]
    WAS Icy Weapon Regular Water 1 min. 3/1 3 sec. Create Water
Identify Plant (Druidic, "D"): [PI1]
    WAS Identify Plant Information Plant Instant 2 1 sec. Seek Plant
Identify Spell (Wizardly, "W*"): [Detect Magic]
    WAS Identify Spell Information Knowledge Instant 2 1 sec. Detect Magic
Ignite Fire (Wizardly, "W"): [M0]
    WAS Ignite Fire Regular Fire 1 sec. 1 to 4/S 1 sec. –
Illusion Disguise (Wizardly, "W"): [Simple Illusion]
    WAS Illusion Disguise Regular Illusion Varies 3 1 sec. Simple Illusion
Illusion Shell (Wizardly, "W"): [Simple Illusion]
    WAS Illusion Shell Regular Illusion 1 min. 1 or 2/H 1 sec. Simple Illusion
Independence (Wizardly, "W"): [Simple Illusion]
    WAS Independence Area Illusion Varies 2 Varies Simple Illusion
Infravision (Wizardly, "W"): [Keen Vision or 5 L&D spells]
    WAS Infravision Regular Lt-Dk 1 min. 3/1 1 sec. Keen Vision or 5 Light spells
Initiative (Wizardly, "W"): [Independence, Wisdom]
    WAS Initiative Area Illusion Indef.# Varies 10 sec. Independence, Wisdom
Invisibility (Wizardly, "W"): [6 L&D spells including Blur]
    WAS Invisibility Regular Lt-Dk 1 min. 5/3 3 sec. 6 Light spells inc. Blur
Iron Arm (Wizardly, "W"): [Resist Pain, DX 11+]
    WAS Iron Arm Blocking Protection Instant 1 1 sec. Resist Pain, DX 11+
Itch (Wizardly, "W"): [M0]
    WAS Itch Regular/R-HT Body Scratch# 2 1 sec. –
Keen Sense (Wizardly, "W*"): [M0/BT1]
    WAS Keen Sense Regular Mind 30 min. 1 per +/H# 1 sec. –
Knot (Wizardly, "W"): [Stiffen]
    WAS Knot Regular Mk-Brk Indef.# 2 3 sec. Stiffen
Know Illusion (Wizardly, "W"): [Simple Illusion]
    WAS Know Illusion Information Illusion Instant 2 1 sec. Simple Illusion
Know Location (Druidic, "D"): [PI2] or (Wizardly, "W*"): [M1/ BT1, Tell Position]
    WAS Know Location Information Knowledge Instant 2 10 sec. M1, Tell Position
Lend Energy (Clerical, "C"): [PI1] or (Wizardly, "W"): [M1]
    WAS Lend Energy Regular Healing Perm. Varies 1 sec. M1 or Empathy advantage
Lend Language (Wizardly, "W*"): [3 C&E spells]
    WAS Lend Language Regular Comm. 1 min. 3/1 3 sec. 3 Communication spells, or Beast Speech
Lend Skill (Wizardly, "W*"): [Mind-Sending, IQ 11+]
    WAS Lend Skill Regular Comm. 1 min. 3/2 3 sec. Mind-Sending, IQ 11+
Lend Vitality (Clerical, "C"): [PI1]
    WAS Lend Vitality Regular Healing 1 hr. 1 per HP loaned 1 sec. Lend Energy
Levitation (Wizardly, "W"): [Apportation]
    WAS Levitation Regular/R-ST or Will Movement 1 min. 1 per 80 lbs./H# 2 sec. Apportation
Light (Clerical, "C"): [PI1] or (Wizardly, "W"): [M0]
    WAS Light Regular Lt-Dk 1 min. 1/1 1 sec. –
Light Jet (Clerical, "C"): [PI2] or (Wizardly, "W"): [Continual Light]
    WAS Light Jet Regular Lt-Dk 1 min. 2/1 1 sec. Continual Light or Shape Light
Light Tread (Druidic, "D"): [PI2] or (Wizardly, "W"): [Apportation, Shape Earth]
    WAS Light Tread Regular Movement 10 min. 4/1# 1 sec. Apportation, Shape Earth
Light (Clerical, "C"): [PI1] or (Wizardly, "W"): [M0]
    WAS Lighten Enchantment Enchantment Perm. Varies – Enchant
Lighten Burden (Wizardly, "W"): [Apportation]
    WAS Lighten Burden Regular Movement 10 min. 3 or 5/H# 3 sec. Apportation
Lightning (Druidic, "D"): [PI4] or (Wizardly, "W"): [M1, 6 Air spells]
    WAS Lightning Missile Weather/Air Instant 1 to Magery# 1 to 3 sec. M1, 6 Air spells
Lightning (Druidic, "D"): [PI4] or (Wizardly, "W"): [M1, 6 Air spells]
    WAS Lightning Armor Regular Weather/Air 1 min. 7/4 1 sec. 6 Lightning spells inc. Resist Lightning
Lightning Missiles (Wizardly, "W"): [Lightning Weapon]
    WAS Lightning Missiles Regular Weather/Air 1 min. 4/2# 3 sec. Lightning Weapon
Lightning (Druidic, "D"): [PI4] or (Wizardly, "W"): [M1, 6 Air spells]
    WAS Lightning Stare* Regular Weather/Air 1 sec. 1 to 4 2 sec. Lightning, Resist Lightning
Lightning Weapon (Wizardly, "W"): [M2, Lightning]
    WAS Lightning Weapon Regular Weather/Air 1 min. 4/1 2 sec. M2, Lightning
Lightning (Druidic, "D"): [PI4] or (Wizardly, "W"): [M1, 6 Air spells]
    WAS Lightning Whip Regular Weather/Air 10 sec. 1 per 2 yards# 2 sec. Lightning
Lockmaster (Wizardly, "W"): [Either M2, Apportation or Locksmith]
    WAS Lockmaster Regular/R-Magelock Movement Perm. 3 10 sec. Locksmith or Apportation and M2
Locksmith (Wizardly, "W"): [Apportation]
    WAS Locksmith Regular Movement 1 min. 2/2 1 sec. Apportation
Loyalty (Wizardly, "W*"): [Bravery, 2 Mind Control spells]
    WAS Loyalty Regular/R-Will Mind 1 hr. 2/2# 2 sec. Bravery, 2 other Mind Control spells
Mage Sight (Wizardly, "W*"): [Detect Magic]
    WAS Mage Sight Regular Knowledge 1 min. 3/2 1 sec. Detect Magic
Mage-Stealth (Wizardly, "W*"): [Hush]
    WAS Mage-Stealth Regular Sound 1 min. 3/2 3 sec. Hush
Magelock (Wizardly, "W"): [M1]
    WAS Magelock Regular Protection 6 hrs. 3/2 4 sec. M1
Magic Resistance (Clerical, "C"): [PI3] or (Wizardly, "W"): [M1, 1 spell from 7 different colleges]
    WAS Magic Resistance Regular/R-Will+M Meta-Spell 1 min. 1 to 5/S# 3 sec. M1, 1 spell each from 7 colleges
Major Healing (Clerical, "C"): [PI2]
    WAS Major Healing* Regular Healing Perm. 1 to 4 1 sec. M1, Minor Healing
Manipulate (Wizardly, "W"): [Locksmith]
    WAS Manipulate Regular Movement 1 min. 4/3# 3 sec. Locksmith
Mapmaker (Wizardly, "W"): [Copy, Measurement]
    WAS Mapmaker Special Mk-Brk 1 hr. 4/2 10 sec. Inscribe, Measurement
Mass Daze (Wizardly, "W*"): [Daze, IQ 13+]
    WAS Mass Daze Area/R-HT Mind Instant 2/1# sec.=cost Daze, IQ 13+
Mass Sleep (Wizardly, "W*"): [Sleep, IQ 13+]
    WAS Mass Sleep Area/R-HT Mind Instant 3# sec.=cost Sleep, IQ 13+
Master (Druidic, "D"): [PI1]
    WAS Master Reg./Block./R-IQ Animal Indef. 2 1 sec. Beast-Soother
Measurement (Wizardly, "W*"): [M0/BT1]
    WAS Measurement Area/Inform. Knowledge Instant 1 1 sec. –
Message (Wizardly, "W*"): [Great Voice, Seeker]
    WAS Message Regular/R-spell Sound/Comm. Varies 1/15 sec. Varies Great Voice, Seeker
Might (Clerical, "C"): [PI1] or (Wizardly, "W"): [Lend Energy]
    WAS Might Regular Body 1 min. 2 per ST+/S 1 sec. Lend Energy
Mind-Reading (Wizardly, "W*"): [Truthsayer or Borrow Language]
    WAS Mind-Reading Regular/R-Will Comm. 1 min. 4/2 10 sec. Truthsayer or Borrow Language
Mind-Search (Wizardly, "W*"): [Mind-Reading]
    WAS Mind-Search* Regular/R-Will Comm. 1 min. 6/3 1 min. Mind-Reading
Mind-Sending (Wizardly, "W*"): [Mind-Reading]
    WAS Mind-Sending Regular Comm. 1 min. 4/4 4 sec. Mind-Reading
Minor Healing (Clerical, "C"): [PI1]
    WAS Minor Healing Regular Healing Perm. 1 to 3 1 sec. Lend Vitality
Mirror (Wizardly, "W"): [Colors]
    WAS Mirror Regular Lt-Dk 1 min. 2/2 1 sec. Colors
Missile Shield (Wizardly, "W"): [Apportation or Shield]
    WAS Missile Shield Regular Protection 1 min. 5/2 1 sec. Apportation or Shield
Monk’s Banquet (Clerical, "C"): [PI4]
    WAS Monk’s Banquet Regular Food 24 hrs. 6 1 sec. Fool’s Banquet, Resist Pain
Mystic Mist (Druidic, "D"): [PI2] or (Wizardly, "W"): [M1, Watchdog or Shield]
    WAS Mystic Mist Area Protection 10 hrs. 1/S 5 min. M1 and Watchdog or Shield
Nauseate (Wizardly, "W"): [5 Body Control spells]
    WAS Nauseate Regular/R-HT Body 10 sec. 2/S 1 sec. 2 Body spells inc. Perfume
Neutralize Poison (Clerical, "C"): [PI3] or (Druidic, "D"): [PI2]
    WAS Neutralize Poison Regular Healing Perm. 5 30 sec. Cure Disease or M3 and Test Food
Night Vision (Wizardly, "W"): [Keen Vision or 5 L&D spells]
    WAS Night Vision Regular Lt-Dk 1 min. 3/1 1 sec. Keen Vision or 5 Light spells
Nightingale (Wizardly, "W"): [Sense Danger]
    WAS Nightingale Area Protection 10 hrs. 2/2 1 sec. Sense Danger
No-Smell (Druidic, "D"): [PI1] or (Wizardly, "W"): [Purify Air]
    WAS No-Smell Regular Air 1 hr. 2/2 1 sec. Purify Air
Noise (Wizardly, "W*"): [Wall of Silence]
    WAS Noise Area Sound 5 sec. 4/2 1 sec. Wall of Silence
Pain (Wizardly, "W"): [Spasm]
    WAS Pain Regular/R-HT Body 1 sec. 2 2 sec. Spasm
Panic (Wizardly, "W*"): [Fear]
    WAS Panic Area/R-Will Mind 1 min. 4/2 1 sec. Fear
Paralyze Limb (Wizardly, "W"): [M1, 5 Body Control spells including Clumsiness]
    WAS Paralyze Limb Melee/R-HT Body 1 min. 3 1 sec. M1, 5 Body Control spell inc. Clumsiness
Pathfinder (Druidic, "D"): [PI2] or (Wizardly, "W*"): [M1/ BT1, IQ 12+, 2 “Seek” spells]
    WAS Pathfinder Information Knowledge Instant 4 10 sec. M1, IQ 12+, 2 Seek spells
Pentagram (Clerical, "C"): [PI5] or (Wizardly, "W"): [Spell Shield]
    WAS Pentagram Special Meta-Spell Perm. 1/sq. ft.# 1/sq. ft.# Spell Shield
Perfect Illusion (Wizardly, "W"): [M1, Complex Illusion]
    WAS Perfect Illusion Area Illusion 1 min. 3/H# 1 sec. M1, Complex Illusion
Persuasion (Clerical, "C"): [PI2] or (Wizardly, "W*"): [Sense Emotion]
    WAS Persuasion Regular/R-Will Comm. 1 min. 2¥bonus# 1 sec. Sense Emotion
Phantom (Wizardly, "W"): [M2, Apportation, Hinder, Perfect Illusion]
    WAS Phantom* Area Illusion 1 min. 5/H# 1 sec. M2, Perfect Illusion, Hinder, Apportation
Phantom (Wizardly, "W"): [M2, Apportation, Hinder, Perfect Illusion]
    WAS Phantom Flame Area Fire/Illusion 1 min. 1/S 1 sec. Shape Fire or Simple Illusion
Phase (Wizardly, "W"): [M3, Ethereal Body]
    WAS Phase Blocking Gate Instant 3 1 sec. M3, Plane Shift or Ethereal Body
Phase Other (Wizardly, "W"): [Phase]
    WAS Phase Other* Blocking Gate Instant 3 1 sec. Phase
Plant Control (Druidic, "D"): [PI3]
    WAS Plant Control Regular/R-Will Plant 1 min. 3/H 1 sec. Plant Sense
Plant Sense (Druidic, "D"): [PI3]
    WAS Plant Sense Regular/R-Hide Path Plant 1 min. 3/2 1 sec. Forest Warning, Hide Path
Plant Speech (Druidic, "D"): [PI3]
    WAS Plant Speech Regular Plant 1 min. 3/2 1 sec. M1, Plant Sense
Plant Vision (Druidic, "D"): [PI2]
    WAS Plant Vision Regular Plant/Knowledge 30 sec. 1/10 yds. 1 sec. Shape Plant
Poison Food (Wizardly, "W"): [Purify Food]
    WAS Poison Food Regular Food Perm. 3 per meal 1 sec. Purify Food, Decay
Pollen Cloud (Druidic, "D"): [PI2]
    WAS Pollen Cloud Area/R-HT Plant 5 min.# 1 1 sec. Shape Plant
Prepare Game (Wizardly, "W"): [Purify Food]
    WAS Prepare Game Regular Food Perm. 2 10 sec. Purify Food
Projection (Wizardly, "W*"): [Sense Spirit, 4 Knowledge spells]
    WAS Projection Regular Knowledge 1 min. 4/2 3 sec. Sense Spirit, 4 Knowledge spells
Protect Animal (Druidic, "D"): [PI3]
    WAS Protect Animal Area Animal/Protection 1 min. 1/S 1 min. Armor, Watchdog, 3 Animal spells
Purify Air (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [M0]
    WAS Purify Air Area Air Instant 1 1 sec. –
Purify Earth (Druidic, "D"): [PI1] or (Wizardly, "W"): [6 Earth spells including Create Earth]
    WAS Purify Earth Area Earth/Plant Perm. 2# 30 sec. Create Earth, Plant Growth
Purify Food (Clerical, "C"): [PI2] or (Druidic, "D"): [PI2] or (Wizardly, "W"): [Decay]
    WAS Purify Food Regular Food Perm. 1 per lb. 1 sec. Decay
Purify Water (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [Seek Water]
    WAS Purify Water Special Water Perm. 1/gal. 5-10 sec./gal.# Seek Water
Quick March (Druidic, "D"): [PI1] or (Wizardly, "W"): [M1, Haste]
    WAS Quick March Regular Movement 1 day’s march 4# 1 min. M1, Haste
Recover Energy (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [M1, Lend Energy]
    WAS Recover Energy Special Healing Special none Special M1, Lend Energy
Reflect (Wizardly, "W"): [Ward]
    WAS Reflect Block/R-spell Meta-Spell Instant 4 or 6# none Ward
Reflect Gaze (Wizardly, "W"): [Mirror]
    WAS Reflect Gaze* Blocking/R-Spec Protection Instant 2 1 sec. Mirror
Reflexes (Wizardly, "W"): [Grace, Haste]
    WAS Reflexes Regular Body 1 min. 5/3 1 sec. Grace, Haste
Regeneration (Clerical, "C"): [PI4]
    WAS Regeneration* Regular Healing Perm. 20 Special# Magery 2, Restoration
Rejoin (Wizardly, "W"): [Weaken, Restore]
    WAS Rejoin Regular Mk-Brk 10 min. 1 per 10 lbs./H 4 sec./10 lbs. Weaken, Restore
Relieve Paralysis (Clerical, "C"): [PI3]
    WAS Relieve Paralysis Regular Healing 1 min. Varies 10 sec. Stop Paralysis
Relieve Sickness (Clerical, "C"): [PI2]
    WAS Relieve Sickness Regular/R-spell Healing 10 min. 2 10 sec. Lend Vitality
Remember Path (Druidic, "D"): [PI3]
    WAS Remember Path Regular Knowledge 1 hr. 3/1 10 sec. Find Direction, Memorize
Remove Curse (Clerical, "C"): [PI5]
    WAS Remove Curse Regular/R-spell Meta-Spell Instant 20 1 hr. M2, Suspend Curse or 1 spell each from 15 colleges
Repair (Wizardly, "W"): [M2, Rejoin]
    WAS Repair Regular Mk-Brk Perm. 2/5 lbs.# 1 sec./lb. M2, Rejoin
Repel Animal (Druidic, "D"): [PI2]
    WAS Repel Animal Area/R-HT Animal 1 hour Varies 10 sec. 1 Control spell#
Repel Hybrids (Druidic, "D"): [PI3]
    WAS Repel Hybrids* Area/R-HT Animal 1 hour 6/3 10 sec. Hybrid Control
Repel Spirits (Clerical, "C"): [PI3] or (Wizardly, "W"): [Banish, Turn Spirit]
    WAS Repel Spirits Area/R-Will Necro. 1 hr. 4/H 10 sec. Banish, Turn Spirit
Resist Acid (Clerical, "C"): [PI2]
    WAS Resist Acid Regular Water/Protection 1 min. 2/H# 1 sec. Create Acid
Resist Cold (Clerical, "C"): [PI2] or (Druidic, "D"): [PI3] or (Wizardly, "W"): [Heat]
    WAS Resist Cold Regular Fire 1 min. 2/1 1 sec. Heat
Resist Disease (Clerical, "C"): [PI2]
    WAS Resist Disease Regular Healing/Protection 1 hour 4/3 10 sec. Remove Contagion or Vigor
Resist Fire (Clerical, "C"): [PI2] or (Wizardly, "W"): [Fireproof]
    WAS Resist Fire Regular Fire 1 min. 2/1# 1 sec. Fireproof
Resist Lightning (Clerical, "C"): [PI2] or (Druidic, "D"): [PI3] or (Wizardly, "W"): [6 Air spells]
    WAS Resist Lightning Regular Weather/Air/Protection 1 min. 2/1 1 sec. 6 Air spells
Resist Pain (Clerical, "C"): [PI2] or (Wizardly, "W"): [M2, Pain]
    WAS Resist Pain Regular Body 1 min. 4/2 1 sec. M2, Pain
Resist Poison (Clerical, "C"): [PI2]
    WAS Resist Poison Regular Healing/Protection 1 hr. 4/3 10 sec. Vigor 169 Resist Pressure Regular Protection 1 min. Varies 1 sec. Weather Dome
Resist Sound (Wizardly, "W*"): [4 Sound spells]
    WAS Resist Sound Regular Sound/Protection 1 min. 2/1 1 sec. 4 Sound spells
Resist Water (Wizardly, "W"): [Umbrella, or Shape Water, Destroy Water]
    WAS Resist Water Regular Water/Protection 1 min. 2/1 1 sec. Umbrella, or Shape Water and Destroy Water
Restoration (Clerical, "C"): [PI3]
    WAS Restoration* Regular Healing Perm. 15 1 min.# Major Healing, or any 2 of Relieve Paralysis and the
Restore (Wizardly, "W"): [Find Weakness or Simple Illusion]
    WAS Restore Regular Mk-Brk 10 min. 2/1 3 sec. Find Weakness or Simple Illusion
Restore Hearing (Clerical, "C"): [PI2]
    WAS Restore Hearing Regular Healing 1 hr. Varies 5 sec. Minor Healing, Keen Hearing or Strike Deaf
Restore (Wizardly, "W"): [Find Weakness or Simple Illusion]
    WAS Restore Mana* Area Meta-Spell Perm. 10 1 hr. Dispel Magic, Suspend Mana
Restore Memory (Clerical, "C"): [PI2]
    WAS Restore Memory Regular Healing Perm. 3 10 sec. Awaken, IQ 11+
Restore Sight (Clerical, "C"): [PI2]
    WAS Restore Sight Regular Healing 1 hr. Varies 5 sec. Minor Healing, Keen Vision or Strike Blind
Restore Speech (Clerical, "C"): [PI2]
    WAS Restore Speech Regular Healing 1 hr. 5/3 5 sec. Minor Healing, Great Voice or Strike Dumb
Retch (Wizardly, "W"): [Nauseate, Spasm]
    WAS Retch Regular/R-HT Body Instant 3 4 sec. Nauseate, Spasm
Rider (Druidic, "D"): [PI2]
    WAS Rider Regular Animal 5 min. 2/1 1 sec. 1 Control spell#
Rider Within (Druidic, "D"): [PI2]
    WAS Rider Within Regular Animal 1 min. 4/1 3 sec. 2 Control spells#
Rive (Wizardly, "W"): [M2, Shatter]
    WAS Rive* Regular Mk-Brk Instant 1 per die 1 sec. M2, Shatter
Rooted Feet (Wizardly, "W"): [Hinder]
    WAS Rooted Feet Regular/R-ST Body 1 min.# 3 1 sec. Hinder
Roundabout (Wizardly, "W"): [Tanglefoot]
    WAS Roundabout Regular/R-HT Body Instant 3 1 sec. Tanglefoot
Sanctuary (Clerical, "C"): [PI6]
    WAS Sanctuary* Special Gate 1 hr. 5/S 10 sec. Hide Object
Sandstorm (Druidic, "D"): [PI4] or (Wizardly, "W"): [Create Earth, Windstorm]
    WAS Sandstorm Area Air/Earth 1 minute# 3/H Instant# Windstorm, Create Earth
Scry Gate (Wizardly, "W"): [Seek Gate]
    WAS Scry Gate Regular Gate 1 min. 4/4 10 sec. Seek Gate
Scryguard (Wizardly, "W"): [M1]
    WAS Scryguard Regular Meta-Spell 10 hrs. 3/1 5 sec. M1
See Invisible (Wizardly, "W"): [Invisibility, or Dark Vision, Infravision]
    WAS See Invisible Regular Lt-Dk 1 min. 4/2 1 sec. Invisibility, or Dark Vision and Infravision
See Secrets (Clerical, "C"): [PI3] or (Wizardly, "W*"): [Seeker, Aura]
    WAS See Secrets Regular Knowledge 1 min. 5/2 5 sec. Seeker, Aura
Seek Earth (Druidic, "D"): [PI1] or (Wizardly, "W"): [M0]
    WAS Seek Earth Information Earth Instant 3 10 sec. –
Seek Fire (Wizardly, "W"): [M0]
    WAS Seek Fire Information Fire Instant 1 1 sec. –
Seek Food (Druidic, "D"): [PI1] or (Wizardly, "W"): [M0]
    WAS Seek Food Information Food Instant 2 1 sec. –
Seek Gate (Wizardly, "W"): [M2, Seek Magic, 1 spell from 10 colleges]
    WAS Seek Gate Information Gate Instant 3 10 sec. M2, Seek Magic, 1 spell each from 10 colleges
Seek Magic (Wizardly, "W*"): [Detect Magic]
    WAS Seek Magic Information Knowledge/Meta-Spell Instant 6 10 sec. Detect Magic
Seek Plant (Druidic, "D"): [PI1]
    WAS Seek Plant Information Plant Instant 2 1 sec. –
Seek Water (Druidic, "D"): [PI1] or (Wizardly, "W"): [M0]
    WAS Seek Water Information Water Instant 2 1 sec. –
Seeker (Clerical, "C"): [PI2] or (Wizardly, "W*"): [M1/ BT1, IQ 12+, 2 “Seek” spells]
    WAS Seeker Information Knowledge Instant 3 1 sec. M1, IQ 12+, 2 Seek spells
Sense Danger (Wizardly, "W"): [Sense Foes]
    WAS Sense Danger Information Protection Instant 3 1 sec. Sense Foes or Danger Sense
Sense Emotion (Wizardly, "W*"): [Sense Foes]
    WAS Sense Emotion Regular Comm. Instant 2 1 sec. Sense Foes
Sense Foes (Wizardly, "W*"): [M0/BT1]
    WAS Sense Foes Inform./Area Comm. Instant 2# 1 sec. –
Sense Life (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W*"): [M0/BT1]
    WAS Sense Life Inform./Area Comm. Instant 1# 1 sec. –
Sense Spirit (Clerical, "C"): [PI1] or (Wizardly, "W"): [Death Vision, or M1, Sense Life]
    WAS Sense Spirit Inform./Area Necro. Instant 1# 1 sec. Death Vision, or Sense Life and M1
Sensitize (Wizardly, "W"): [M1, Stun]
    WAS Sensitize Regular/R-HT Body 1 min. 3/2 1 sec. M1, Stun
Shape Air (Druidic, "D"): [PI2] or (Wizardly, "W"): [Create Air]
    WAS Shape Air Regular Air 1 min. 1 to 10# 1 sec. Create Air
Shape Earth (Druidic, "D"): [PI2] or (Wizardly, "W"): [Seek Earth]
    WAS Shape Earth Regular Earth 1 min. 1/25 cu. ft./h 1 sec. Seek Earth
Shape Fire (Wizardly, "W"): [Ignite Fire]
    WAS Shape Fire Area Fire 1 min. 2/H 1 sec. Ignite Fire
Shape Plant (Druidic, "D"): [PI2]
    WAS Shape Plant Regular Plant 1 min. 3/1# 10 sec. Identify Plant
Shape Water (Druidic, "D"): [PI2] or (Wizardly, "W"): [Create Water]
    WAS Shape Water Regular Water 1 min. 1/1# 2 sec. Create Water
Share Energy (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [Lend Energy]
    WAS Share Energy Regular Healing Special Varies 1 sec. Lend Energy
Share Vitality (Clerical, "C"): [PI1]
    WAS Share Vitality Regular Healing Perm. 0# 1 sec./HP Lend Vitality
Sharpen (Wizardly, "W"): [Repair]
    WAS Sharpen Regular Mk-Brk 1 min. Varies 4 sec. Repair
Shatter (Wizardly, "W"): [M1, Weaken]
    WAS Shatter* Regular Mk-Brk Instant 1 to 3 1 sec. M1, Weaken
Shatterproof (Wizardly, "W"): [Repair, Shatter]
    WAS Shatterproof Regular Mk-Brk 1 hr. 3/3 1 sec. Repair, Shatter
Shield (Clerical, "C"): [PI1] or (Wizardly, "W"): [M2]
    WAS Shield Regular Protection 1 min. Varies 1 sec. M2
Shocking Touch (Wizardly, "W"): [Lightning]
    WAS Shocking Touch Melee Weather/Air Instant 1 to 3 1 sec. Lightning
Sickness (Wizardly, "W*"): [Drunkenness]
    WAS Sickness Regular/R-HT Mind/Body 1 min. 3/3 4 sec. Drunkenness or Pestilence
Silence (Clerical, "C"): [PI1] or (Wizardly, "W*"): [Sound]
    WAS Silence Area Sound 1 min. 2/1 1 sec. Sound
Silver Tongue (Clerical, "C"): [PI3] or (Wizardly, "W*"): [Voices, 5 Mind Control spells]
    WAS Silver Tongue Regular Sound 1 min. 3/2 1 sec. Voices, Emotion Control
Simple Illusion (Wizardly, "W"): [M0, IQ 11+, no Blindness]
    WAS Simple Illusion Area Illusion 1 min. 1/H 1 sec. not blind, IQ 11+
Sleep (Wizardly, "W*"): [Daze]
    WAS Sleep Regular/R-HT Mind Instant 4 3 sec. Daze
Slow (Wizardly, "W"): [M1, Haste, Hinder]
    WAS Slow Regular/R-HT Movement 10 sec. 5/4 3 sec. M1, Haste, Hinder
Slow Fall (Wizardly, "W"): [Apportation]
    WAS Slow Fall Regular Movement 1 min. 1 per 50 lbs./H 1 sec. Apportation
Slow (Wizardly, "W"): [M1, Haste, Hinder]
    WAS Slow Fire Regular Fire 1 min. Varies 1 sec. Extinguish Fire
Slow (Wizardly, "W"): [M1, Haste, Hinder]
    WAS Slow Healing Regular/R-HT Necro. 1 day 1 to 5/S 10 sec. M1, Frailty, Steal Vitality
Slow (Wizardly, "W"): [M1, Haste, Hinder]
    WAS Slow Time* Area/R-Spec. Gate 1 min. Varies 2 sec. M2, IQ 13+, 2 spells each from 10 colleges
Smoke (Wizardly, "W"): [Shape Fire, Extinguish Fire]
    WAS Smoke Area Fire 5 min.# 1/H 1 sec. Shape Fire, Extinguish Fire
Soul Rider (Wizardly, "W*"): [Mind-Reading]
    WAS Soul Rider Regular/R-Will Comm. 1 min. 5/2 3 sec. Mind-Reading
Sound (Wizardly, "W*"): [M0/BT1]
    WAS Sound Regular Sound Varies Varies 1 sec. –
Sound Jet (Wizardly, "W*"): [Great Voice]
    WAS Sound Jet Regular Sound 1 sec. 1 to 4/S 1 sec. Great Voice
Sound (Wizardly, "W*"): [M0/BT1]
    WAS Sound Vision Regular Sound 1 min. 5/2 1 sec. Keen Hearing or Acute Hearing
Spark Cloud (Wizardly, "W"): [Lightning, Shape Air]
    WAS Spark Cloud Area Weather/Air 10 sec. 1 to 5/S 1 to 5 sec. Shape Air, Lightning
Spark Storm (Druidic, "D"): [PI5] or (Wizardly, "W"): [Lightning, Windstorm]
    WAS Spark Storm Area Weather/Air 1 min.# 2, 4, or 6/H Instant# Windstorm, Lightning
Spasm (Wizardly, "W"): [Itch]
    WAS Spasm Regular/R-HT Body Instant 2 1 sec. Itch
Spell Shield (Wizardly, "W"): [M2, Magic Resistance, Scryguard]
    WAS Spell Shield Area Meta-Spell 1 min. 3/2 1 sec. M2, Scryguard, Magic Resistance
Steelwraith (Wizardly, "W"): [M2, Walk Through Earth]
    WAS Steelwraith Regular/R-HT Earth 1 min. 7/4 2 sec. M2, Walk Through Earth
Stench (Wizardly, "W"): [Purify Air]
    WAS Stench Area Air 5 min. 1 1 sec. Purify Air
Stiffen (Wizardly, "W"): [Rejoin]
    WAS Stiffen Regular/R-Spec. Mk-Brk 10 min. 1 per lb./H# 2 sec./lb. Rejoin
Stone Missile (Wizardly, "W"): [Create Earth]
    WAS Stone Missile Missile Earth Instant 1 to Magery 1 to 3 sec. Create Earth
Stone to Earth (Wizardly, "W"): [Earth to Stone or 4 Earth spells]
    WAS Stone to Earth Regular Earth Perm. 6/25 cu. ft. 1 sec. Earth to Stone or any 4 Earth spells
Stone to Flesh (Clerical, "C"): [PI3] or (Wizardly, "W"): [M2, Flesh to Stone, Stone to Earth]
    WAS Stone to Flesh Regular Earth Perm. 10 5 sec. M2, Stone to Earth, Flesh to Stone
Stop Bleeding (Clerical, "C"): [PI1]
    WAS Stop Bleeding Regular Healing Perm. 1 or 10 1 sec. Lend Vitality
Stop Paralysis (Clerical, "C"): [PI3]
    WAS Stop Paralysis Regular Healing Perm. 1 or 2 1 sec. Major Healing, or Minor Healing and Paralyze Limb
Stop Spasm (Clerical, "C"): [PI2]
    WAS Stop Spasm Regular Body/Healing Instant 1 1 sec. Spasm or Lend Vitality
Strengthen Will (Clerical, "C"): [PI3] or (Wizardly, "W*"): [M1/ BT1, 6 Mind Control spells]
    WAS Strengthen Will Regular Mind 1 min. 1/pt/H 1 sec. M1, 6 Mind spells
Strike Blind (Wizardly, "W"): [Spasm, 2 L&D spells]
    WAS Strike Blind Regular/R-HT Body 10 sec. 4/2 1 sec. 2 Light spells, Spasm
Strike Deaf (Wizardly, "W"): [Spasm, 2 Sound spells]
    WAS Strike Deaf Regular/R-HT Body 10 sec. 3/1 1 sec. 2 Sound spells, Spasm
Strike Dumb (Wizardly, "W"): [Spasm]
    WAS Strike Dumb Regular/R-HT Body 10 sec. 3/1 1 sec. Spasm
Stun (Wizardly, "W"): [Pain]
    WAS Stun Regular/R-HT Body Instant 2 1 sec. Pain
Summon Spirit (Clerical, "C"): [PI2] or (Wizardly, "W"): [M2, Death Vision]
    WAS Summon Spirit Inform./R-Will Necro. 1 min. 20/10# 5 min. Death Vision, M2
Sunbolt (Clerical, "C"): [PI3] or (Wizardly, "W"): [6 L&D spells including Sunlight]
    WAS Sunbolt Missile Lt-Dk Instant 1 to Magery# 1 to 3 sec. 6 Light spells inc. Sunlight
Sunlight (Clerical, "C"): [PI3] or (Druidic, "D"): [PI3] or (Wizardly, "W"): [M1, Colors, Glow]
    WAS Sunlight Area Lt-Dk 1 min. 2/1 1 sec. M1, Glow, Colors
Suspended Animation (Clerical, "C"): [PI3]
    WAS Suspended Animation Regular/R-HT Healing Indef.# 6 30 sec. Sleep, 4 Healing spells
Swim (Druidic, "D"): [PI3] or (Wizardly, "W"): [Levitation, Shape Water]
    WAS Swim Regular Water/Move 1 min. 6/3 3 sec. Shape Water, Levitation
Tangle Growth (Druidic, "D"): [PI3]
    WAS Tangle Growth Area Plant 1 min. 1 or 2#/H 2 sec. Plant Growth
Tanglefoot (Wizardly, "W"): [Clumsiness]
    WAS Tanglefoot Regular/R-DX Body Instant 2 1 sec. Clumsiness
Telepathy (Wizardly, "W*"): [Mind-Sending]
    WAS Telepathy* Regular Comm. 1 min. 4/4# 4 sec. Mind-Sending
Teleport Shield (Wizardly, "W"): [Spell Shield, Watchdog]
    WAS Teleport Shield Area Protection/Gate 1 hr. 1/S# 10 sec. Watchdog, either Spell Shield or Teleport
Tell Position (Druidic, "D"): [PI1] or (Wizardly, "W*"): [Measurement]
    WAS Tell Position Information Knowledge Instant 1 1 sec. Measurement
Terror (Wizardly, "W*"): [Fear]
    WAS Terror Area/R-Will Mind Instant 4 1 sec. Fear
Test Food (Clerical, "C"): [PI1] or (Wizardly, "W"): [M0]
    WAS Test Food Information Food Instant 1 to 3# 1 sec. –
Test Load (Wizardly, "W*"): [Measurement]
    WAS Test Load Area/Inform. Knowledge Instant 2# 1 sec. Measurement
Thunderclap (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W*"): [Sound]
    WAS Thunderclap Regular Sound Instant 2 1 sec. Sound
Tickle (Wizardly, "W"): [Spasm]
    WAS Tickle Regular/R-Will Body 1 min. 5/5 1 sec. Spasm
Total Paralysis (Wizardly, "W"): [Paralyze Limb]
    WAS Total Paralysis Melee/R-HT Body 1 min. 5 1 sec. Paralyze Limb
Trace (Wizardly, "W*"): [Seeker]
    WAS Trace Regular Knowledge 1 hr. 3/1 1 min. Seeker
Trace Teleport (Wizardly, "W"): [M2, IQ 13+, 1 spell from 10 colleges]
    WAS Trace Teleport Information/R-spell Gate/Movement Instant 3 1 sec. Teleport, Timeport, or Plane Shift
Truthsayer (Clerical, "C"): [PI2] or (Wizardly, "W*"): [Sense Emotion]
    WAS Truthsayer Inform./R-Will Comm. Instant 2 1 sec. Sense Emotion
Turn Spirit (Clerical, "C"): [PI2] or (Wizardly, "W"): [Fear, Sense Spirit]
    WAS Turn Spirit Regular/R-Will Necro. 10 sec. 4/2# 1 sec. Fear, Sense Spirit
Turn Zombie (Clerical, "C"): [PI2]
    WAS Turn Zombie Area Necro. 1 day 2 4 sec. Zombie#
Umbrella (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [Shape Water or Shield]
    WAS Umbrella Regular Water/Protection 10 min. 1/1 2 sec. Shape Water or Shield
Undo (Wizardly, "W"): [Locksmith]
    WAS Undo Regular/R-Spec. Movement Instant 3 or 6# 1 sec. Locksmith
Vigil (Clerical, "C"): [PI4]
    WAS Vigil* Regular Mind 1 night 8 1 sec. M2, Sleep, Lend Energy
Vigor (Clerical, "C"): [PI1] or (Wizardly, "W"): [Frailty or Might]
    WAS Vigor Regular Body 1 min. 2 per HT+/S# 1 sec. Lend Vitality or Frailty
Voices (Wizardly, "W*"): [Sound]
    WAS Voices Regular Sound 1 min. 3/2 1 sec. Sound
Walk on Air (Wizardly, "W"): [Shape Air]
    WAS Walk on Air Regular Air 1 min. 3/2 1 sec. Shape Air
Walk on Water (Wizardly, "W"): [Shape Water]
    WAS Walk on Water Regular Water 1 min. 3/2 4 sec. Shape Water
Walk Through Earth (Wizardly, "W"): [4 Earth spells]
    WAS Walk Through Earth Regular Earth 10 sec. 3/3# 1 sec. 4 Earth spells
Walk Through Plants (Druidic, "D"): [PI3]
    WAS Walk Through Plants Regular Plant 1 min. 3/1 1 sec. Hide Path, Shape Plant
Walk Through Wood (Druidic, "D"): [PI3]
    WAS Walk Through Wood Regular Plant 1 sec. 3/2 1 sec. Walk Through Plants
Wall of Lightning (Wizardly, "W"): [Lightning]
    WAS Wall of Lightning Regular Weather/Air 1 min. 2 to 6/S 1 sec. Lightning
Wall of Silence (Wizardly, "W*"): [Silence]
    WAS Wall of Silence Area Sound 1 min. 2/1 1 sec. Silence
Wallwalker (Wizardly, "W"): [Apportation]
    WAS Wallwalker Regular Movement 1 min. 1 per 50 lbs./H# 1 sec. Apportation
Ward (Wizardly, "W"): [M1]
    WAS Ward Block/R-spell Meta-Spell Instant 2 or 3# none M1
Warmth (Clerical, "C"): [PI1] or (Druidic, "D"): [PI1] or (Wizardly, "W"): [Heat]
    WAS Warmth Regular Fire/Protection 1 hour 2/1 10 sec. Heat
Watchdog (Clerical, "C"): [PI1] or (Wizardly, "W"): [Sense Danger]
    WAS Watchdog Area Protection 10 hrs. 1/1 10 sec. Sense Danger
Water Jet (Wizardly, "W"): [Shape Water]
    WAS Water Jet Regular Water 1 sec. 1 to 3 1 sec. Shape Water
Water Vision (Druidic, "D"): [PI3] or (Wizardly, "W*"): [Shape Water]
    WAS Water Vision Information Water/Knowledge 30 sec. 1/1# 1 sec. Shape Water
Weaken (Wizardly, "W"): [Find Weakness]
    WAS Weaken Regular Mk-Brk Perm. 2 to 6 5 sec. Find Weakness
Weaken (Wizardly, "W"): [Find Weakness]
    WAS Weaken Blood Regular/R-HT Body/Necro. 1 day 9/5 1 sec. Sickness or Steal Vitality
Weaken Will (Wizardly, "W*"): [M1/BT1, Foolishness]
    WAS Weaken Will Regular/R-Will Mind 1 min. 2/pt/H 1 sec. M1, Foolishness
Weather Dome (Druidic, "D"): [PI2]
    WAS Weather Dome Area Protection/Weather 6 hrs. 3/2 1 sec. 2 spells each from 4 elements
Windstorm (Druidic, "D"): [PI2] or (Wizardly, "W"): [Shape Air]
    WAS Windstorm Area Air 1 min.# 2/H Instant# Shape Air
Wisdom (Clerical, "C"): [PI3] or (Wizardly, "W*"): [6 Mind Control spells]
    WAS Wisdom Regular Mind 1 min. 4 per IQ+/S 1 sec. 6 other Mind Control spells
Wither Limb (Wizardly, "W"): [M2, Paralyze Limb]
    WAS Wither Limb Melee/R-HT Body Perm. 5 1 sec. M2, Paralyze Limb
Wither Plant (Druidic, "D"): [PI4]
    WAS Wither Plant Area/R-HT Plant Perm. 2 10 sec. Blight
Wizard Eye (Wizardly, "W*"): [Apportation, Keen Vision]
    WAS Wizard Eye Regular Knowledge 1 min. 4/2 2 sec. Apportation, Keen Vision
"""
