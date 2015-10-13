#!/bin/sh

set -x

sensible-browser demo/index.html &
./dev-scripts/soh-runner.sh
