#!/bin/sh

set -x

docker run -p 4000:4000 -p 3001:3000 mgsloan/soh-runner:local '00000000-0000-0000-0000-000000000000'
