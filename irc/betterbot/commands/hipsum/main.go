package main

import (
	"encoding/json"
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

		if strings.HasPrefix(msg.Body, ";") {
			return
		}

		reply := &common.Message{
			Destination: msg.ReplyTo,
			Kind:        "TextMessage",
		}

		var replyMsg string
		var err error
		if strings.Contains(msg.Body, "hipster me") {
			replyMsg, err = getHipsterText()
		} else {
			return
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

func getHipsterText() (string, error) {
	resp, err := http.Get("http://hipsterjesus.com/api/?type=hipster-centric&html=false&paras=1")
	if err != nil {
		return "", err
	}

	textStruct := &struct {
		Text string `json:"text"`
	}{}

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		return "", err
	}

	json.Unmarshal(body, textStruct)

	text := strings.Split(textStruct.Text, ". ")[0]

	return text, nil
}
