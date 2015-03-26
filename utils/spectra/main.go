package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/Xe/spectra/text/garcia"
)

var (
	inputFile  = flag.String("env", "environments.yaml", "list of environments to create configs for")
	outputDir  = flag.String("dir", "output", "output directory to use")
	udtemplate = flag.String("userdata", "user-data", "base user-data to extend from")
	help       = flag.Bool("help", false, "shows this message")
)

func usage() {
	garcia.WriteHeader("Usage of Spectra:")
	fmt.Printf("  spectra [options]\n\n")
	fmt.Printf("  Flags:\n")
	flag.PrintDefaults()

	os.Exit(0)
}

func main() {
	flag.Parse()

	if *help {
		usage()
	}

	// Create output directory if it doesn't exist already
	if _, err := os.Stat(*outputDir); err != nil {
		if os.IsNotExist(err) {
			err := os.Mkdir(*outputDir, 0700)
			if err != nil {
				log.Fatalf("Could not create directory %s, please check permissions.", *outputDir)
			}
		}
	}

	// See if input file exists
	if _, err := os.Stat(*inputFile); err != nil {
		if os.IsNotExist(err) {
			log.Fatalf("Environment file %s does not exist", *inputFile)
		}
	}

	// See if userdata template exists
	if _, err := os.Stat(*udtemplate); err != nil {
		if os.IsNotExist(err) {
			log.Fatalf("userdata template %s does not exist", *udtemplate)
		}
	}

	cmdSeed()
}
