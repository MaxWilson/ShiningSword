# Shining Sword

[https://maxwilson.github.io/ShiningSword](https://maxwilson.github.io/ShiningSword)

Dungeons and Dragons combat game inspired by XCOM: UFO Defense and The Bard's Tale.

Design goals:

1.) Transparency: making it easy for spectators and after-the-fact viewers to understand what happened and why, e.g. that the Kraken hit the Barbarian but did only 9 points of damage due to rage. Hierarchical tree views of events to let people drill down to the level of detail they want; Bards Tale-style summary tables of HP and spell points and current status; animations showing movement and floating damage numbers showing changes to HP.

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