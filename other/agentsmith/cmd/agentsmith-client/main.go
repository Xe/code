package main

import (
	"net"
	"fmt"
	"flag"

	"golang.org/x/crypto/ssh/agent"
)

var (
	server = flag.String("server", "127.0.0.1:42069", "where to connect to")
)

func main() {
	flag.Parse()

	conn, err := net.Dial("tcp", *server)
	if err != nil {
		panic(err)
	}

	pnt := agent.NewClient(conn)

	keys, err := pnt.List()
	if err != nil {
		panic(err)
	}

	for _, key := range keys {
		fmt.Println(key.String())
	}
}
