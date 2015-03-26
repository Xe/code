package main

import (
	"io/ioutil"
	"log"

	"github.com/Pallinder/go-randomdata"
	"github.com/Xe/spectra/models"
	"github.com/Xe/spectra/text/garcia"
	"github.com/Xe/spectra/util"
	cconfig "github.com/coreos/coreos-cloudinit/config"
	"gopkg.in/yaml.v1"
)

func cmdSeed() {
	// Create our environment
	env := &models.Environment{}
	garcia.WriteHeader("Creating environment...")

	// Error if file is unreadable
	inYaml, err := ioutil.ReadFile(*inputFile)
	if err != nil {
		log.Fatalf("Could not read %s", *inputFile)
	}
	garcia.WriteData("Loading input description...")

	// Demarshal environment configuration from the argument
	err = yaml.Unmarshal(inYaml, env)
	if err != nil {
		log.Fatal(err)
	}

	if env.EtcdURL == "" {
		env.EtcdURL = util.GetEtcdUrl()
	}

	garcia.WriteData("Cluster discovery URL: " + env.EtcdURL)

	// Make a list of all the kinds of nodes we have to make
	var nodes []*models.Node

	// Flatten subclusters
	for _, e := range env.Environments {
		if e.Children != nil {
			for _, c := range e.Children {
				c.Name = e.Name + "-" + c.Name
				nodes = append(nodes, c)
			}
		} else {
			nodes = append(nodes, e)
		}
	}

	// Base cloudconfig text
	config, err := ioutil.ReadFile(*udtemplate)
	if err != nil {
		log.Fatal(err)
	}

	// Create all the cloudconfigs
	for _, n := range nodes {
		garcia.WriteDataLevel(2, "Creating cloudconfig output for "+n.Name)

		if env.Override.VPCId != "" {
			n.VPCId = env.Override.VPCId
		}

		cloudconfig, err := cconfig.NewCloudConfig(string(config))
		if err != nil {
			log.Fatal(err)
		}

		name := randomdata.Adjective() + "-" + randomdata.Noun()

		cloudconfig.Hostname = name + "." + n.Name + "." + env.Suffix
		cloudconfig.Coreos.OEM.Name = "spectra" // Totally have to do this
		cloudconfig.Coreos.Etcd.Discovery = env.EtcdURL

		// Set metadata based on the needs of the machine
		cloudconfig.Coreos.Fleet.Metadata =
			n.Name + "," + n.Tags + "," + n.Wants

		cc, err := yaml.Marshal(cloudconfig)
		if err != nil {
			log.Fatal(err)
		}

		err = ioutil.WriteFile(*outputDir+"/"+n.Name+".cloudconfig.yaml", cc, 0600)
		if err != nil {
			log.Fatal(err)
		}
	}

	garcia.WriteEnd("Created output directory with configuration files")
}
