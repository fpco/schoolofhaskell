#!/bin/sh

set -xe

stack setup
stack --stack-yaml ghcjs-stack.yaml setup
