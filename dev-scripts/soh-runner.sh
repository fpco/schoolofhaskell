#!/bin/sh

set -x

docker run -p 4000:4000 -p 3000:3000 mgsloan/soh-runner:local bash -c 'PATH=/opt/soh/:$PATH soh-runner run --receipt 00000000-0000-0000-0000-000000000000'
