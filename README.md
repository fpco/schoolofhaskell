School of Haskell
=================

The School of Haskell is a service providing interactive Haskell
tutorials and documentation.

How to install
--------------

Installation requires the latest version of
[stack](https://github.com/commercialhaskell/stack).  The first time
you install SoH, you'll need to pull the docker image with:

```
stack docker pull
```

After this, use the following commands to build:

```
# Build and install required GHCJS packages.
./dev-scripts/install-ghcjs-deps.sh
# Build the SoH libraries and executables.
stack build
```

How to run
----------

Run the `soh-server` binary to run the School of Haskell webserver.
Then, point your browser at `localhost:3000`.

If it's built with `-fdev`, then it will need `ghcjs` to recompile the
`soh-client/` code.  One way to do this is to run it within the
docker container, via `stack exec soh-server`.
