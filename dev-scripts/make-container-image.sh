#!/bin/sh

set -xe

cd dev-scripts/container-docker

rm -f soh-runner
rm -f ide-backend-server

ln -P ../../.stack-work/install/x86_64-linux/lts-2.14/7.8.4/bin/soh-runner soh-runner
ln -P ../../.stack-work/install/x86_64-linux/lts-2.14/7.8.4/bin/ide-backend-server ide-backend-server

docker build -t mgsloan/soh-runner:local .

# After this, we can run it via:
#
# docker run -p 127.0.0.1:4000:4000 -t -i mgsloan/soh-ghcjs-dev:local /bin/bash -c 'PATH=/opt/soh/:$PATH /opt/soh/soh-runner'
