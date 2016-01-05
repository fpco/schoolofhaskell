#!/bin/sh

set -xe

stack setup
# FIXME: this is needed due to https://github.com/commercialhaskell/stack/issues/1258
stack install happy
(cd soh-client; stack setup)
