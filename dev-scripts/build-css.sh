#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./dev-scripts/build-css.sh

set -x

# FIXME: something a little less hacky than this :)
stack exec -- runhaskell dev-scripts/internal/CompileLucius.hs
