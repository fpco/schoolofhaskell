#!/bin/sh

find soh-client -path soh-client/.stack-work -prune -o \( -name *.hs -o -name *.js -o -name *.lucius -o -name *.cabal \) |
   entr ./dev-scripts/build-client.sh
