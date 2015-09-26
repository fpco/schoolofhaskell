#!/bin/sh

set -x

sensible-browser demo/calculator.html &
./dev-scripts/soh-runner.sh
