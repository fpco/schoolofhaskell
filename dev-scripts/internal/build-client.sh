#!/bin/sh

# This is an internal script used by dev-scripts/build-client.sh.

set -xe

cabal --config-file=cabal-ghcjs-config install --ghcjs soh-client/ $@

# Replace the old soh.js with the newly built one.
mkdir -p soh-scheduler/static
rm -f soh-scheduler/static/soh.js
cp soh-client/dist/build/soh-client/soh-client.jsexe/all.js soh-scheduler/static/soh.js
