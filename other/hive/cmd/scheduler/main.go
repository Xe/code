package main

import (
	"flag"
	"log"
	"runtime"

	"github.com/Xe/hive/common"
	"github.com/fsouza/go-dockerclient"
)

var (
	u *common.Uplink
	c *docker.Client
)

type App struct {
	Domain  string `json:"domain"`
	API     bool   `json:"api"`
	Version string `json:"version"`
	Name    string `json:"name"`
	Root    string `json:"root"`

	ImageID string `json:"imageid"`
}

func main() {
	flag.Parse()

	var err error
	u, err = common.Connect()
	if err != nil {
		panic(err)
	}

	c, err = docker.NewClientFromEnv()

	_, err = u.QueueSubscribe("hive.containerlist", "actor",
		func(subject, reply string, detail string) {
			log.Printf("Got request %q on %s from %s", detail, subject, reply)

			ctrs, err := c.ListContainers(docker.ListContainersOptions{})
			if err != nil {
				u.Publish(reply, err.Error())
				return
			}

			u.Publish(reply, ctrs)
		},
	)

	runtime.Goexit()
}
