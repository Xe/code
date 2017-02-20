#!/bin/sh

trap 'pkill -KILL -P $$; exit 0' 0 1 2 3 13 15 # EXIT HUP INT QUIT PIPE TERM

for i in $(seq 1 5)
do
	(
		while true
		do
			$1 -uplink.role worker -uplink.parent $HOSTNAME &

			wait

			echo "$? I'm dead now, restarting"

			sleep 1
		done
	) &

	sleep 1
done

pstree

wait
