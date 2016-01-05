package cqbot

import (
	"fmt"
	"testing"
)

func TestBotLoading(t *testing.T) {
	bot := NewBot("../test/test.ini")

	nick := bot.GetNick()

	if nick != bot.Config.Bot.Nick {
		fmt.Printf("Configuration parser broken!\n")
		t.Fail()
	}

	fmt.Println("Configuration loaded!")
}

func TestConnection(t *testing.T) {
	bot := NewBot("../test/test.ini")

	bot.AddHandler("002", func(line *Line) {
		fmt.Printf("Connection to server %s established!\n", line.Source)
		bot.Quit("Plugh!")
	})

	bot.Connect()
	bot.Auth()
	bot.HandleCommands()
}
