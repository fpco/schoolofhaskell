#!/bin/sh

# Run ./scripts/create-ghcjs-cabal-config.sh before this.
#
# Run this script from the schoolofhaskell directory, like so:
#
# ./scripts/build-ghcjs-deps.sh

set -x

fpbuild exec -- $PWD/dev-scripts/build-ghcjs-deps-internal.sh
