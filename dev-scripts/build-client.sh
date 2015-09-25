#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./dev-scripts/build-client.sh
#
# You can pass stack arguments in as arguments to this script.  For example:
#
# ./dev-scripts/build-client.sh --flag soh-client:local-soh-runner

set -x

stack --stack-yaml ghcjs-stack.yaml build $@
rm -f demo/soh.js
cp $(stack --stack-yaml ghcjs-stack.yaml path --local-install-root)/bin/soh-client.jsexe/all.js demo/soh.js
stack runghc --package shakespeare-2.0.6 dev-scripts/internal/CompileLucius.hs
