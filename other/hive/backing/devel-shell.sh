#!/bin/bash

docker run --rm -itv cockroach-stg:/data/cockroach -v ipfs-stg:/data/ipfs --net=hive ubuntu:14.04 /bin/bash
