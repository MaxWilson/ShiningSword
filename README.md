# Shining Sword

[https://maxwilson.github.io/ShiningSword](https://maxwilson.github.io/ShiningSword)

Dungeons and Dragons combat game inspired by XCOM: UFO Defense and The Bard's Tale.

## Design goals for Shining Sword

1.) Transparency: making it easy for spectators and after-the-fact viewers to understand what happened and why, e.g. that the Kraken hit the Barbarian but did only 9 points of damage due to rage. Hierarchical tree views of events to let people drill down to the level of detail they want; Bards Tale-style summary tables of HP and spell points and current status; animations showing movement and floating damage numbers showing changes to HP. See Appendix A: Spectation, below.

2.) Easy and optional automation: allows DMs to add new monsters on the fly with minimal data entry and have the monsters follow the rules on their own (attacks, crits, interactions with features like Rage) while still allowing the DM to manually intervene and e.g. impose an ad hoc -2 to hit on a given attack due to improvised partial cover, or change monster HP from its default values, or force a monster to flee instead of attacking for roleplaying reasons. Let the DM focus on DMing and not on tracking which monsters have attacked yet. Automation should allow gdb-style text commands like "Ragnar will attack Beholder" but also be available through button clicks, for both players and DMs. Automation should optionally be able to replace dice rolls while still allowing players to roll physical dice if they prefer and report the results.

3.) Collaboration: trying to coordinate games across multiple tools like Discord for chat and dice rolling and Owlbear Rodeo for maps and online SRDs for monster stats adds cognitive overhead. The tool should be able to show the same view to everyone in the game, like Owlbear Rodeo but with automation and event logging/etc. built in, so that when the DM says "Hobgoblin Warlord will flee but Beholder will antimagic Delsenora and zap Ragnar" everybody can see the Beholder attempting to disintegrate Ragnar on its turn while the Hobgoblin Warlord flees. Again, spectators and after-the-fact viewers (like redditors) should also be able to see events unfold.

Very much in-progress.

## Building and running the app

* Install JS dependencies: `npm install`
* **Move to `src` folder**: `cd src`
* Install F# dependencies: `dotnet restore`
* Run webpack dev server: `yarn start`
* In your browser, open: http://localhost:8080/

> `yarn webpack` (or `npm-start`) will internally start the Fable daemon and run a script in package.json concurrently.

If you are using VS Code + [Ionide](http://ionide.io/), you can also use the key combination: Ctrl+Shift+B (Cmd+Shift+B on macOS) instead of typing the `yarn webpack` command. This also has the advantage that Fable-specific errors will be highlighted in the editor along with other F# errors.

Any modification you do to the F# code will be reflected in the web page after saving. When you want to output the JS code to disk, run `yarn build` (or `npm build`) and you'll get a minified JS bundle in the `public` folder.

# Whitespace errors

If you see superfluous ^Ms in git diff, do git config core.whitespace cr-at-eol

## Appendix A: Spectation

One of my favorite points from the book _Characteristics of Games_ is that games vary in how much fun they are to spectate:

_If one thinks of games as evolving and competing organisms, in the sense that they change over time and become more or less prevalent, it's clear that spectation is a big bonus for a game. Players will usually feel better about their choice of game knowing that people want to watch it. An audience may inspire them to want to play better, which in turns may make them spend more time playing and practicing so as to improve their skills. Members of the audience are in some sense participants in the game, and some of them may decide to become players later on._

_Spectation does not even have to happen at the same time as the game itself. Watching a game can be interesting even if one knows the outcome. A newspaper article written about a game that happened the previous day is an example of delayed (and filtered) spectation--a friend describing the game to you is another._

_Games that have distinct highlights rather than a more or less continuous stream of action tend to do better here, because they are easier to recount in a condensed yet still interesting form. In this sense, football is probably better than soccer, which is still better than basketball, and a marathon is worse than any of them. Another reflection of how easy it is to condense the events of a game can be seen in the amount of information that can useful be given in something like a box score._

Desire to enable spectation has influenced the design goals for Shining Sword.
