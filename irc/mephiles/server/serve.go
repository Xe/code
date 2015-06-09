package server

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"net/textproto"
	"sync"

	"github.com/Xe/mephiles/server/types"
)

var (
	wg    sync.WaitGroup
	conns chan *types.Client
)

func init() {
	conns = make(chan *types.Client, 16)
}

// Serve kicks off the daemon.
func Serve(configfile string, etcdMachine string) (err error) {
	wg = sync.WaitGroup{}

	list, err := net.Listen("tcp", ":6067")
	if err != nil {
		log.Fatal(err)
	}

	go func() {
		for {
			conn, err := list.Accept()
			if err != nil {
				log.Printf("Could not accept: %s", err.Error())
			}

			log.Printf("Connection from %s", conn.RemoteAddr())

			client := types.NewClient(conn)

			conns <- client
		}
	}()

	for {
		go func(c *types.Client) {
			b := bufio.NewReader(c.Conn)
			tp := textproto.NewReader(b)

			for {
				line, err := tp.ReadLine()
				if err != nil {
					log.Printf("%s: error: %s", c.Conn.(net.Conn).RemoteAddr(), err)

					c.Conn.Close()
					break
				}

				log.Printf("%s: %s", c.Conn.(net.Conn).RemoteAddr(), line)
				fmt.Fprintf(c.Conn, "%s\r\n", line)
			}
		}(<-conns)
	}

	return
}
