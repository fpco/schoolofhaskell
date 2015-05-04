#!/bin/sh

# This is an internal script used by scripts/build-ghcjs-deps.sh.
PATH=/opt/ghcjs/0.1.0/lts-1.0/bin:$PATH cabal --config-file=cabal-ghcjs-config update
PATH=/opt/ghcjs/0.1.0/lts-1.0/bin:$PATH cabal --config-file=cabal-ghcjs-config install --ghcjs deps/ghcjs-react/ deps/ide-backend/ide-backend-common/ deps/ide-backend-client/ide-backend-json deps/ghcjs-websockets --constraint="aeson==0.8.0.0"
