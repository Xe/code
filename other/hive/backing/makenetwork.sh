#!/bin/bash

set -x

docker network create hive
docker volume create --name cockroach-stg
docker volume create --name ipfs-stg

docker run -d --name nats --net=host --net=hive docker.io/nats
docker run -d --name cockroachdb --net=hive -v cockroach-stg:/data/cockroach cockroachdb/cockroach start --store=path=/data/cockroach --insecure
docker run --rm -it -v ipfs-stg:/data/ipfs alpine:3.3 sh -c 'chown 1000:1000 /data/ipfs'
docker run -d --name ipfsd --net=hive -p 4002:4001 -v ipfs-stg:/data/ipfs jbenet/go-ipfs:latest
