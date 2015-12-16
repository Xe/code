#!/bin/bash

set -x

go build -buildmode=c-shared -o libsum.so add.go
nim c -r test.nim
