School of Haskell
=================

The School of Haskell is a service providing interactive Haskell
tutorials and documentation.

How to build
------------

Installation requires stack [stack](https://github.com/commercialhaskell/stack)
0.1.6 or newer.

Here's how to build, assuming local usage:

```
# Build the serverside code
stack build
# Build the client code
./dev-scripts/build-client.sh --flag soh-client:local-soh-runner
```

How to run the local demo
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
