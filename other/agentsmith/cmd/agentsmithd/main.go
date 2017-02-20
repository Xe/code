package main

import (
	"fmt"
	"net"
	"log"

	"golang.org/x/crypto/ssh/agent"
	"github.com/davidmz/go-pageant"
)

func main() {
	pnt := pageant.New()

	keys, err := pnt.List()
	if err != nil {panic(err)}

	for _, key := range keys {
		fmt.Printf("%s\n", key.String())
	}

	s, err := net.Listen("tcp", ":42069")
	if err != nil {panic(err)}

	for {
		conn, err := s.Accept()
		if err != nil {
			conn.Close()
		}

		go func() {
			err := agent.ServeAgent(pnt, conn)

			if err != nil {
				log.Println(err)
			}
		} ()
	}
}
