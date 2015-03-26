package controllers

import (
	"log"
	"os"

	"github.com/Xe/servapi/atheme"
)

var (
	a        *atheme.Atheme
	endpoint string
)

func init() {
	endpoint = os.Getenv("ATHEME_ENDPOINT")

	var err error
	a, err = atheme.NewAtheme(endpoint)
	if err != nil {
		log.Fatal(err)
	}
}
