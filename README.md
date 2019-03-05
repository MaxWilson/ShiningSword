# Shining Sword

Dungeons and Dragons combat game inspired by XCOM: UFO Defense and The Bard's Tale.

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

