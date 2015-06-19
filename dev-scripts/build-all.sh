#!/bin/sh

# Build and install required GHCJS packages.
./dev-scripts/install-ghcjs-deps.sh
# Build the SoH client.
./dev-scripts/build-client.sh
# Build the SoH css.
./dev-scripts/build-css.sh
# Build the SoH libraries and executables.
stack build --flag soh-server:dev
# Build the docker image used by soh-runner.
./dev-scripts/make-container-image.sh
