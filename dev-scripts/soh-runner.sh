#!/bin/sh

set -x

docker run -p 4000:4000 mgsloan/soh-ghcjs-dev:local bash -c 'PATH=/opt/soh/:$PATH soh-runner run --port 4000 --receipt 00000000-0000-0000-0000-000000000000'
