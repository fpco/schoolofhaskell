School of Haskell
=================

The School of Haskell is a service providing interactive Haskell
tutorials and documentation.

How to build
------------

Installation requires the latest version of
[stack](https://github.com/commercialhaskell/stack).  The first time
you install SoH, you'll need to pull the docker image with:

```
stack docker pull
```

After this, run `./dev-scripts/build-all.sh` to build everything.
Often, you can avoid doing a full rebuild, and only invoke the
commands you need:

* If you change any of the GHCJS dependencies (everything in the
  `deps/` folder except `deps/ide-backend/`), then you need to run
  `./dev-scripts/install-ghcjs-deps.sh`.

* If you change anything but `soh-client/`, then `stack build --flag
  soh-server:dev` is necessary.

* If you change `soh-runner/`, `deps/ide-backend-client/`, or
  `deps/ide-backend`, then next, you need to run
  `./dev-scripts/make-container-image.sh`.  This creates the docker
  image used for the runner.

This can be a bit tricky to remember, so maybe it's best to just stick
with `./dev-scripts/build-all.sh`!

Note that `--flag soh-server:dev` enables dev mode for the server.
This is described below.

About `-fdev` mode
------------------

`--flag soh-server:dev` enables the following:

* Sets default Yesod dev settings (see Settings.hs).

* Enables dev mode for the client code, which has the following
  effects:

  - Instead of connecting to soh-scheduler to request a container, it
    expects that a container is already running on `localhost:4000`,
    with a predetermined container receipt.

A different flag, `--flag soh-server:runtime-ghcjs` enables
recompiling the Client JS for every request.  This allows the
`soh-client/` code to be edited without doing rebuilding the server.

How to run locally
------------------

Assuming it's built with `--flag soh-server:dev`, run the
following scripts in separate terminals:

```
./dev-scripts/soh-server.sh
```

```
./dev-scripts/soh-runner.sh
```

Then, point your browser at `localhost:5000`.
