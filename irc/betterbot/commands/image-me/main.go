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

		if strings.HasPrefix(msg.Body, ";") {
			return
		}

		reply := &common.Message{
			Destination: msg.ReplyTo,
			Kind:        "TextMessage",
		}

		var replyMsg string
		var err error
		if strings.Contains(msg.Body, "animate me") {
			replyMsg, err = giphySearch(helperExtractTerms("animate me", msg.Body))
		} else if strings.Contains(msg.Body, "image me") {
			replyMsg, err = imageMe(helperExtractTerms("image me", msg.Body), msg.Sender)
		}

		if err != nil {
			reply.Body = err.Error()
		} else {
			reply.Body = replyMsg
		}

		conn.Publish(msg.Via+":input", reply)
	})

	conn.Publish("betterbot.birth", &common.Birth{
		ID:         uplink.ID,
		EventKinds: []string{"TextMessage"},
	})

	runtime.Goexit()
}
