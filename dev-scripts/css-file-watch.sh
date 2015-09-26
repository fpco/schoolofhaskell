#!/bin/sh

find soh-client -path soh-client/.stack-work -prune -o -name *.lucius |
    entr ./dev-scripts/generate-css.sh
