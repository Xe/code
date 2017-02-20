#!/bin/bash

docker run --rm -it --net=container:cockroachdb cockroachdb/cockroach sql
