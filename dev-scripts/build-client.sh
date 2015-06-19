#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./dev-scripts/build-client.sh

set -x

stack exec -- $PWD/dev-scripts/internal/build-client.sh
