package main

/*
Copyright (C) 2014 Christine Dodrill <xena@yolo-swag.com> All rights reserved.

This software is provided 'as-is', without any express or implied
warranty. In no event will the authors be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

1. The origin of this software must not be misrepresented; you must not
   claim that you wrote the original software. If you use this software
   in a product, an acknowledgment in the product documentation would be
   appreciated but is not required.

2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.

3. This notice may not be removed or altered from any source
   distribution.
*/

import (
	"flag"
	"log"
	"os"
	"os/exec"
	"os/signal"
	"syscall"
	"time"
)

var (
	WNOHANG       int = 1
	startupScript     = flag.String("rc", "/etc/rc.local", "file to run on startup")
)

func reap() {
	syscall.Wait4(-1, nil, WNOHANG, &syscall.Rusage{})
}

func main() {
	reapsigs := make(chan os.Signal, 1)
	signal.Notify(reapsigs, syscall.SIGCHLD)
	go func() {
		for _ = range reapsigs {
			reap()
		}
	}()

	termsigs := make(chan os.Signal, 1)
	signal.Notify(termsigs, syscall.SIGTERM)
	signal.Notify(termsigs, syscall.SIGKILL)
	go func() {
		for _ = range termsigs {
			syscall.Kill(-1, syscall.SIGTERM)
			time.Sleep(9 * time.Second)
			syscall.Kill(-1, syscall.SIGKILL)

			os.Exit(0)
		}
	}()

	cmd := exec.Command("/bin/sh", *startupScript)

	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr

	err := cmd.Start()
	if err != nil {
		log.Fatal(err)
	}

	err = cmd.Wait()
	if err != nil {
		log.Fatal(err)
	}

	for {
	}
}
