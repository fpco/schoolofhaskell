School of Haskell
=================

Installation
------------

Installation requires the latest version of
[stack](https://github.com/commercialhaskell/stack).  Currently,
building the SoH at least the HEAD version.

```
# Pull the docker image, if necessary.
stack docker pull
# Build and install required GHCJS packages.
./dev-scripts/install-ghcjs-deps.sh
# Build the SoH libraries and executables.
stack build
```
