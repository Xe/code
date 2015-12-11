<?php

/*
 * This is a posix compliant init(8) in PHP. If you use this you are a monster.
 */

function sig_handler($signo) {
	switch($signo) {
	case SIGCHLD:
		$status = null;
		pcntl_waitpid(-1, $status);
	}
}

pcntl_signal(SIGCHLD, 'sig_handler');

echo 'Lol, posix compliant init!';

while (true) {}
