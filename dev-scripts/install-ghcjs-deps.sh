#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./scripts/install-ghcjs-deps.sh

set -x

fpbuild exec -- $PWD/dev-scripts/install-ghcjs-deps-internal.sh
