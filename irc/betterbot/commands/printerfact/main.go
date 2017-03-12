package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
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

		if !strings.HasPrefix(msg.Body, ";printerfact") {
			return
		}

		reply := &common.Message{
			Destination: msg.ReplyTo,
			Kind:        "TextMessage",
		}

		fact, err := getPrinterFact()
		if err != nil {
			reply.Body = err.Error()
			return
		} else {
			reply.Body = fact
		}

		conn.Publish(msg.Via+":input", reply)
	})

	conn.Publish("betterbot.birth", &common.Birth{
		ID:         uplink.ID,
		EventKinds: []string{"TextMessage"},
	})

	runtime.Goexit()
}

func getPrinterFact() (string, error) {
	resp, err := http.Get("http://void.vodka/api")
	if err != nil {
		return "", err
	}

	factStruct := &struct {
		Facts []string `json:"facts"`
	}{}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	json.Unmarshal(body, factStruct)

	text := fmt.Sprintf("%s", factStruct.Facts[0])

	return text, nil
}
