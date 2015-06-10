package main

import (
	"flag"
	"log"

	"github.com/Xe/hw/dht11"
)

var (
	pinNumber = flag.String("pin", "gpio4", "gpio pin the dht11 is on")
)

func main() {
	flag.Parse()

	sensor, err := dht11.New(*pinNumber)
	if err != nil {
		log.Fatal(err)
	}

	sensor.Read()
}
