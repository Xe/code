package main

import (
	"runtime"

	"github.com/Xe/betterbot-common"
	"github.com/Xe/uuid"
	"github.com/bwmarrin/discordgo"
	"github.com/namsral/flag"
)

var (
	uplink *common.Uplink

	config      = flag.String("config", "", "configuration file")
	discordUser = flag.String("discord-username", "", "username (email address) for discord")
	discordPass = flag.String("discord-password", "", "password (not hunter2)")

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

	discord, err := discordgo.New(*discordUser, *discordPass)
	if err != nil {
		panic(err)
	}

	uplink.Println("Connected to discord")

	discord.Open()
	discord.AddHandler(messageCreate)
	discord.UpdateStatus(0, "Golang message input queue")

	conn.Subscribe(uplink.ID+":input", func(subj, reply string, msg *common.Message) {
		switch msg.Kind {
		case "TextMessage":
			discord.ChannelMessageSend(msg.Destination, msg.Body)
		}
	})

	runtime.Goexit()
}

func messageCreate(s *discordgo.Session, m *discordgo.MessageCreate) {
	uplink.Printf("%20s %20s > %s\n", m.ChannelID, m.Author.Username, m.Content)

	msg := &common.Message{
		ID:       uuid.New(),
		Protocol: "discord",
		Kind:     "TextMessage",
		Via:      uplink.ID,
		Body:     m.Content,
		Sender:   m.Author.Username,
		ReplyTo:  m.ChannelID,
	}

	uplink.Publish("betterbot.input", msg)
}
