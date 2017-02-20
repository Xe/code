/*
Command gurren is a simple log sending test command.
*/
package main

import (
	"flag"

	"github.com/Xe/gurren"
)

var (
	esURL = flag.String("host", "http://127.0.0.1:9200", "where to log to")
)

func main() {
	flag.Parse()

	l, err := gurren.New([]string{*esURL}, "syslog")
	if err != nil {
		panic(err)
	}

	err = l.Log("hi mom")
	if err != nil {
		panic(err)
	}
}
