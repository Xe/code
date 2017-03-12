package main

import (
	"github.com/Xe/betterbot-common"
	"github.com/Xe/uuid"
	"github.com/geir54/goMatrix"
	"github.com/namsral/flag"
)

var (
	uplink *common.Uplink

	config           = flag.String("config", "", "configuration file")
	matrixUser       = flag.String("matrix-username", "", "username (email address) for matrix")
	matrixPass       = flag.String("matrix-password", "", "password (not hunter2)")
	matrixHomeserver = flag.String("matrix-homeserver", "https://matrix.org", "matrix homeserver")

	botID string
)

func init() {
	flag.Parse()
}

func main() {
	conn, err := common.Connect()
	if err != nil {
		panic(err)
	}
	uplink = conn

	session := goMatrix.Init(*matrixHomeserver)
	err = session.Login(*matrixUser, *matrixPass)
	if err != nil {
		panic(err)
	}

	session.Start()
	uplink.Println("Connected to matrix")

	conn.Subscribe(uplink.ID+":input", func(subj, reply string, msg *common.Message) {
		switch msg.Kind {
		case "TextMessage":
			if msg.Body == "" {
				return
			}

			session.SendToRoom(msg.Destination, msg.Body)
		}
	})

	for {
		msg := <-session.OnNewMsg
		msgToq := &common.Message{
			ID:          uuid.New(),
			Protocol:    "matrix",
			Kind:        "TextMessage",
			Via:         uplink.ID,
			Body:        msg.Text,
			Sender:      msg.Sender,
			ReplyTo:     msg.RoomID,
			Destination: msg.RoomName,
		}

		uplink.Publish("betterbot.input", msgToq)
	}
}
