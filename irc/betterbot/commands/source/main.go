package main

import (
	"runtime"
	"strings"

	"github.com/Xe/betterbot-common"
	"github.com/namsral/flag"
)

var (
	uplink *common.Uplink
)

func main() {
	flag.Parse()

	conn, err := common.Connect()
	if err != nil {
		panic(err)
	}
	uplink = conn

	conn.Subscribe(uplink.ID+":input", func(subj, unusedReply string, msg *common.Message) {
		if msg.Kind != "TextMessage" {
			return
		}

		if !strings.HasPrefix(msg.Body, ";source") {
			return
		}

		reply := &common.Message{
			Destination: msg.ReplyTo,
			Kind:        "TextMessage",
			Body:        "Source code: https://github.com/Xe/betterbot",
		}

		conn.Publish(msg.Via+":input", reply)
	})

	conn.Publish("betterbot.birth", &common.Birth{
		ID:         uplink.ID,
		EventKinds: []string{"TextMessage"},
	})

	runtime.Goexit()
}
