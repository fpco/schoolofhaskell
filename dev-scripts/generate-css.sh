#!/bin/sh

# Run this script from the schoolofhaskell directory, like so:
#
# ./dev-scripts/generate-css.sh

stack runghc --package shakespeare-2.0.6 dev-scripts/internal/CompileLucius.hs
