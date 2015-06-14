#!/bin/sh

set -x

cd soh-server
stack docker exec ../.stack-work/install/x86_64-linux/lts-2.13/7.8.4/bin/soh-server
