#!/bin/bash

mkdir -p /usr/local/src
cd /usr/local/src
git clone git://github.com/gittup/tup.git
cd tup
./bootstrap.sh
cp tup /usr/local/bin/tup
