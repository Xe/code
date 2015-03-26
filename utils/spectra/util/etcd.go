// Package util defines utility functions for spectra to use.
package util

import (
	"io/ioutil"
	"log"
	"net/http"
)

// GetEtcdUrl returns a new etcd discovery token. It will hard-fail
// (kill the program) if this call fails.
func GetEtcdUrl() string {
	response, err := http.Get("http://discovery.etcd.io/new")
	if err != nil {
		log.Fatal(err)
	}

	defer response.Body.Close()
	contents, err := ioutil.ReadAll(response.Body)
	if err != nil {
		log.Fatal(err)
	}

	return string(contents)
}
