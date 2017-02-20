#!/bin/bash

set -e
set -x

source ./env.sh

mkdir bin ||:
cd bin

go build duke/...
