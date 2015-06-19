School of Haskell
=================

The School of Haskell is a service providing interactive Haskell
tutorials and documentation.

How to use SoH on your site
---------------------------

Add the following script tags:

    <script src="//fb.me/react-0.12.0.min.js"></script>
    <script src="//cdn.rawgit.com/ajaxorg/ace-builds/v1.1.9/src-noconflict/ace.js"></script>
    <script src="//cdn.rawgit.com/chjj/term.js/v0.0.4/src/term.js"></script>
    <script src="http://soh-scheduler-1627848338.us-east-1.elb.amazonaws.com/static/soh.js"></script>

FIXME: it isn't so ideal to use git cdn.  These should be served by
our server.

FIXME: more description here.

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

* If you change `soh-client/`, then you need to run
  `dev-scripts/build-client.sh`.

* If you change `soh-runner/`, `deps/ide-backend-client/`, or
  `deps/ide-backend`, then next, you need to run
  `./dev-scripts/make-container-image.sh`.  This creates the docker
  image used for the runner.

This can be a bit tricky to remember, so maybe it's best to just stick
with `./dev-scripts/build-all.sh`!

How to run locally
------------------

FIXME: this section is out of date because now soh-scheduler serves
the client.

Assuming it's built with `--flag soh-server:dev`, run the
following scripts in separate terminals:

```
./dev-scripts/soh-server.sh
```

```
./dev-scripts/soh-runner.sh
```

Then, point your browser at `localhost:5000`.
