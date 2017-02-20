#!/bin/bash

for i in $(seq 1 50)
do
    docker run -d --net hive hive helloworldd
done
