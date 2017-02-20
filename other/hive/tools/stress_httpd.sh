#!/bin/bash

function stress {
	while true
	do
		curl http://greedo.xeserv.us:9090 >/dev/null 2>/dev/null

		sleep 0.125
	done
}

for i in $(seq 1 50)
do
	stress &
done

sleep 60

kill -TERM -$$

wait
