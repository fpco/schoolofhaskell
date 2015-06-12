#!/bin/sh

set -x

cd dev-scripts/container-docker

rm soh-runner
rm ide-backend-server

ln -P ../../.stack-work/install/x86_64-linux/lts-2.13/7.8.4/bin/soh-runner soh-runner
ln -P ../../.stack-work/install/x86_64-linux/lts-2.13/7.8.4/bin/ide-backend-server ide-backend-server

docker build -t mgsloan/soh-ghcjs-dev:lts-2.13 .

# After this, we can run it via:
#
# docker run -p 127.0.0.1:4000:4000 -t -i mgsloan/soh-ghcjs-dev:lts-2.13 /bin/bash -c 'PATH=/home/:$PATH /home/soh-runner'
