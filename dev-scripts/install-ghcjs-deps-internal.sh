#!/bin/sh

set -x

# This is an internal script used by scripts/build-ghcjs-deps.sh.
cabal --config-file=cabal-ghcjs-config update
cabal --config-file=cabal-ghcjs-config install --ghcjs deps/ghcjs-react/ deps/ide-backend/ide-backend-common/ deps/ide-backend-client/ide-backend-json deps/ghcjs-websockets deps/JsonGrammar2 deps/stack-prism deps/language-typescript deps/ghcjs-from-typescript/ghcjs-ace deps/ghcjs-jquery soh-runner-api/ soh-scheduler-api/ --force-reinstalls
