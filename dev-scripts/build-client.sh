#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./dev-scripts/build-client.sh
#
# You can pass cabal arguments in as arguments to this script.  For example:
#
# ./dev-scripts/build-client.sh -flocal-soh-runner

set -x

stack exec -- $PWD/dev-scripts/internal/build-client.sh $@
