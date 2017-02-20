package main

import (
	"flag"
	"log"
	"time"

	"github.com/Xe/hive/common"
	"github.com/fsouza/go-dockerclient"
)

func main() {
	flag.Parse()

	u, err := common.Connect()
	if err != nil {
		panic(err)
	}

	reply := []*docker.APIContainers{}

	err = u.Request("hive.containerlist", "", &reply, 5*time.Second)
	if err != nil {
		panic(err)
	}

	for _, ctr := range reply {
		log.Printf("%s %s", ctr.ID, ctr.Image)
	}
}
