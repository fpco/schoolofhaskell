#!/bin/sh

# This is an internal script used by dev-scripts/install-ghcjs-deps.sh.

set -x

cabal --config-file=cabal-ghcjs-config update
cabal --config-file=cabal-ghcjs-config install --ghcjs deps/ghcjs-react/ deps/ide-backend/ide-backend-common/ deps/ide-backend-client/ide-backend-json deps/JsonGrammar2 deps/ghcjs-from-typescript/ghcjs-ace deps/ghcjs-jquery soh-runner-api/ soh-scheduler-api/
