#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./dev-scripts/build-client.sh
#
# You can pass stack arguments in as arguments to this script.

set -xe

(cd soh-client; stack build --flag soh-client:local-soh-runner $@)
rm -f demo/soh.js
cp $(cd soh-client; stack path --local-install-root)/bin/soh-client.jsexe/all.js demo/soh.js
./dev-scripts/generate-css.sh
