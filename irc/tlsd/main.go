package main

import (
	"errors"
	"fmt"
	"log"
	"net"
	"os"
	"strings"
)

var (
	ctlFd       int
	ctlConn     net.Conn
	ctlPipe     int
	ctlPipeConn net.Conn

	ErrNoSuchVar    = errors.New("No such envvar")
	ErrNoSuchSocket = errors.New("No such socket")
)

func fail() {
	fmt.Fprintln(os.Stderr, "This is the Elemental-IRCd tlsd.\n"+
		"You know you aren't supposed to run me directly?\n"+
		"Have a nice life")
	os.Exit(1)
}

func main() {
	defer func() {
		fmt.Println("bye!")
	}()

	ctlConn, err := net.FileConn(os.NewFile(45, "ircd control socket"))
	if err != nil {
		fail()
	}

	//ctlConn.Write([]byte("")) // signal we are useless

	for {
		buf := make([]byte, 512)
		_, err := ctlConn.Read(buf)
		if err != nil {
			panic(err)
		}

		line := strings.Split(string(buf), "\x00")[:5]
		fmt.Printf("line: %#v\n", line)

		cmd, ok := IncomingCommandsToNames[line[0][:1]]
		if !ok {
			log.Println("Could not get appropriate command")
		}

		log.Printf("Got command %s", cmd.String())

		switch cmd {
		case CommandIncInitPRNG:
			log.Println("PRNG inited")

		case CommandIncKeys:
			log.Printf("have keys %s %s and %s", line[1], line[2], line[3])

		case CommandIncAccept:
			log.Printf("%x, %d", line[0][1], line[0][1])

			res := bufToInt32(string(line[0][1]))
			log.Printf("%x, %d", res, res)

			myConn, err := net.FileConn(os.NewFile(uintptr(line[0][1]), "client connection"))

			if err != nil {
				log.Println(err.Error())
				continue
			}

			myConn.Write([]byte("foo!\n"))
			myConn.Close()
		}
	}
}
