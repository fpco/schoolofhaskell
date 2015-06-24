School of Haskell
=================

The School of Haskell is a service providing interactive Haskell
tutorials and documentation.

How to use SoH on your site
---------------------------

Add the following tags to your `<head>`:

```
<script src="https://fb.me/react-0.12.0.min.js"></script>
<script src="https://cdn.rawgit.com/ajaxorg/ace-builds/v1.1.9/src-noconflict/ace.js"></script>
<script src="https://cdn.rawgit.com/chjj/term.js/v0.0.4/src/term.js"></script>
<link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css" rel="stylesheet" type="text/css">
<link href="https://fonts.googleapis.com/css?family=Lato:400,700" rel="stylesheet" type="text/css">
<script src="http://soh-scheduler-1627848338.us-east-1.elb.amazonaws.com/static/soh.js"></script>
<link href="http://soh-scheduler-1627848338.us-east-1.elb.amazonaws.com/static/soh.css" rel="stylesheet" type="text/css">
```

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
  `deps/` folder except `deps/ide-backend/` and `deps/airship`), then
  you need to run `./dev-scripts/install-ghcjs-deps.sh`.

* If you change `soh-client/`, then you need to run
  `dev-scripts/build-client.sh`.

* If you cahnge anything but `soh-client/`, then you need to run
  `stack build`.

* If you change `soh-runner/`, `deps/ide-backend-client/`, or
  `deps/ide-backend`, then next, you need to run
  `./dev-scripts/make-container-image.sh` after the stack build.  This
  creates the docker image used for the runner.

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

Resolving build issues
----------------------

I've run into an issue where an error like this occurs while
installing ghcjs deps:

```
IdeSession/RPC/Server.hs:94:22:
    Exception when trying to run compile-time code:
      exported function not found: Fun {funPackage = Package {unPackage = "ide-backend-common-0.9.1.2"}, funModule = "IdeSession.RPC.Stream", funSymbol = "h$idezmbackendzmcommonzm0zi9zi1zi2ZCIdeSessionziRPCziStreamzinextInStream2"}
    Code: waitAny
    In the splice: $waitAny
```

It seems probable that it's
[this ghcjs bug](https://github.com/ghcjs/ghcjs/issues/349) which has
to do with using an older `.js_a` with a newer `.js_o`.  That bug has
been fixed, but the fix is not in our version of ghcjs.  I've found
that a good way to resolve this is to do a `stack docker reset`.
