#!/bin/sh

set -x

# This is an internal script used by scripts/build-ghcjs-deps.sh.
PATH=/opt/ghcjs/0.1.0/lts-1.0/bin:$PATH cabal --config-file=cabal-ghcjs-config update
PATH=/opt/ghcjs/0.1.0/lts-1.0/bin:$PATH cabal --config-file=cabal-ghcjs-config install --ghcjs deps/ghcjs-react/ deps/ide-backend/ide-backend-common/ deps/ide-backend-client/ide-backend-json deps/ghcjs-websockets deps/JsonGrammar2 deps/stack-prism deps/language-typescript deps/ghcjs-from-typescript/ghcjs-ace --constraint="aeson==0.8.0.0"

# Remove dist, as these confuse fpbuild rebuild
rm -r deps/ide-backend/ide-backend-common/dist
rm -r deps/ide-backend-client/ide-backend-json/dist
rm -r deps/ghcjs-websockets/dist
rm -r deps/JsonGrammar2/dist
rm -r deps/language-typescript/dist
