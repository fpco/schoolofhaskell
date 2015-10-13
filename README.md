# School of Haskell

The School of Haskell is a service providing interactive Haskell
tutorials and documentation.

## How to setup

Run `./dev-scripts/setup-environment.sh` to install the correct GHC and GHCJS.
Note that GHCJS is currently installed from source, and will take quite a while
to compile and boot.

## How to build

Installation requires stack [stack](https://github.com/commercialhaskell/stack)
0.1.6 or newer.

To build the serverside code: `stack build`

To build the client code and CSS: `./dev-scripts/build-client.sh`

## How to run the local demo

`/dev-scripts/run-demo.sh` will run `soh-runner` and open up a browser window
with the demo. The web snippets in the demo requires `yesod` and
`yesod-media-simple`. Install these with `stack build yesod-media-simple` in
order for the web snippets to compile.

## Development tips

I like to rebuild the client whenever any files change. This is what
`dev-scripts/client-file-watch.sh` does. It requires the
[entr](http://entrproject.org/) file watcher program, which can be installed
with `sudo apt-get install entr`.

If you just change the lucius file, you can run `./dev-scripts/generate-css.sh`
to regenerate the CSS. `dev-scripts/css-file-watch.sh` is similar to
client-file-watch, but it only rebuilds the CSS.
