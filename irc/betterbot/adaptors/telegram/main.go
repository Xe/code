package main

import (
	"fmt"
	"log"
	"strconv"

	"github.com/Xe/betterbot-common"
	"github.com/Xe/uuid"
	"github.com/namsral/flag"
	"gopkg.in/telegram-bot-api.v4"
)

var (
	uplink *common.Uplink

	config   = flag.String("config", "", "configuration file")
	apiToken = flag.String("telegram-token", "", "telegram bot token")
	botUser  = flag.String("telegram-user", "", "telegram bot username")

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

	bot, err := tgbotapi.NewBotAPI(*apiToken)
	if err != nil {
		panic(err)
	}

	uplink.Println("Connected to telegram")
	uplink.Printf("Authorized on account %s", bot.Self.UserName)

	u := tgbotapi.NewUpdate(0)
	u.Timeout = 60

	updates, err := bot.GetUpdatesChan(u)

	conn.Subscribe(uplink.ID+":input", func(subj, reply string, msg *common.Message) {
		switch msg.Kind {
		case "TextMessage":
			if msg.Body == "" {
				return
			}

			s, err := strconv.ParseInt(msg.Destination, 10, 64)
			if err != nil {
				uplink.Printf("Could not send message: %v", err)
				return
			}

			msg := tgbotapi.NewMessage(s, msg.Body)

			_, err = bot.Send(msg)
			if err != nil {
				uplink.Printf("Error in sending: %#v", err)
			}
		}
	})

	for update := range updates {
		if update.Message == nil {
			continue
		}

		log.Printf("[%s] %s", update.Message.From.UserName, update.Message.Text)

		msg := update.Message.Text
		cid := fmt.Sprintf("%v", update.Message.Chat.ID)

		msgToq := &common.Message{
			ID:       uuid.New(),
			Protocol: "telegram",
			Kind:     "TextMessage",
			Via:      uplink.ID,
			Body:     msg,
			Sender:   update.Message.From.UserName,
			ReplyTo:  cid,
		}

		uplink.Publish("betterbot.input", msgToq)
	}
}
