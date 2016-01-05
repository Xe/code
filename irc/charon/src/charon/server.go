package main

import (
	"irc/message"
	"time"
)

type Server struct {
	uuid        string
	name        string
	description string
	kind        string
	epoch       time.Time
}

func NewServer(line *message.Message) *Server {
	epoch, err := time.Parse(time.RFC3339, line.Args[1])
	if err != nil {
		panic(err)
	}

	s := &Server{
		uuid:        line.Source,
		kind:        line.Args[0],
		name:        line.Args[2],
		description: line.Args[3],
		epoch:       epoch,
	}

	serverlist[s.uuid] = s

	return s
}
