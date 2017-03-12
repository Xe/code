package main

import (
	"runtime"
	"strings"

	"github.com/Xe/betterbot-common"
	"github.com/Xe/uuid"
	"github.com/namsral/flag"
	"github.com/thoj/go-ircevent"
)

var (
	uplink *common.Uplink

	config       = flag.String("config", "", "configuration file")
	ircServer    = flag.String("irc-server", "ircd:6667", "which IRC server to connect to")
	ircServerSSL = flag.Bool("irc-server-ssl", false, "connect over ssl?")
	ircChannels  = flag.String("irc-channels", "#betterbot", "which IRC channels to join on connect")
	ircNick      = flag.String("irc-nick", "betterbot", "bot nickname")
	ircUser      = flag.String("irc-user", "betterbot", "irc username")
	ircPassword  = flag.String("irc-password", "hunter2", "password on connection (default *******)")
	nsPass       = flag.String("nickserv-pass", "hunter2", "which password to use for NickServ (default *******)")
)

func main() {
	flag.Parse()

	conn, err := common.Connect()
	if err != nil {
		panic(err)
	}
	uplink = conn

	bot := irc.IRC(*ircNick, *ircUser)
	bot.UseTLS = *ircServerSSL

	bot.AddCallback("*", func(event *irc.Event) {
		msg := eventToMessage(event)

		uplink.Printf("Recieved %q (%s)", msg.Kind, event.Raw)
		uplink.Publish("betterbot.input", msg)
	})

	bot.AddCallback("001", func(e *irc.Event) {
		for _, channel := range strings.Split(*ircChannels, ",") {
			bot.Join(channel)
		}
	})

	conn.Subscribe(uplink.ID+":input", func(subj, reply string, msg *common.Message) {
		kind := ""
		if strings.HasPrefix(msg.Kind, "irc:") {
			kind = strings.TrimPrefix(msg.Kind, "irc:")
		} else {
			for key, value := range verbKind {
				if value == msg.Kind {
					kind = key
					break
				}
			}
		}

		bot.SendRawf("%s %s :%s", kind, msg.Destination, msg.Body)
	})

	err = bot.Connect(*ircServer)
	if err != nil {
		panic(err)
	}

	runtime.Goexit()
}

var (
	verbKind = map[string]string{
		"PRIVMSG": "TextMessage",
		"JOIN":    "Join",
	}
)

func eventToMessage(e *irc.Event) *common.Message {
	kind, ok := verbKind[e.Code]
	if !ok {
		kind = "irc:" + e.Code
	}

	if len(e.Arguments) == 0 {
		e.Arguments = []string{""}
	}

	return &common.Message{
		ID:       uuid.New(),
		Protocol: "irc",
		ReplyTo:  e.Arguments[0],
		Via:      uplink.ID,

		Body:   e.Arguments[len(e.Arguments)-1],
		Sender: e.Nick,
		SenderParams: map[string]interface{}{
			"user": e.User,
			"host": e.Host,
		},
		Destination: e.Arguments[0],

		Kind: kind,
		Metadata: map[string]interface{}{
			"ircserver": *ircServer,
			"raw_line":  e.Raw,
			"source":    e.Source,
		},
	}
}
