#!/bin/bash

set -x

go build -buildmode=c-shared -o libsum.so add.go
gcc -c -Wall -Werror -fPIC ./libcsum.c
gcc -shared -o libcsum.so libcsum.o
nim c -d:release -r test.nim
