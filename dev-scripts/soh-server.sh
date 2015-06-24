#!/bin/sh

set -xe

cd soh-server
stack docker exec -- bash -c 'PORT=5000 APPROOT=http://localhost:5000 ../.stack-work/install/x86_64-linux/lts-2.14/7.8.4/bin/soh-server'
