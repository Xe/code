package irc

import (
	"errors"
	"os"
	"strings"
	"time"

	"github.com/thoj/go-ircevent"
)

// Errors that the bot may return when instansiating.
var (
	ErrBadConfig = errors.New("A configuration element is missing.")
)

// Bot is an IRC bot that Krewella controls.
type Bot struct {
	IrcObj      *irc.Connection // IRC connection
	NetworkName string          // network name to connect to
	Signal      chan *irc.Event // signals from the parent

	nick string
	user string
	host string
	port string

	channels []string
}

// New creates a new Bot that will connect to network and returns it
// or an error if a configuration element is missing.
func New(network string) (b *Bot, err error) {
	n := strings.ToUpper(network)

	b = &Bot{}

	b.NetworkName = network
	b.Signal = make(chan *irc.Event, 5)

	b.nick = os.Getenv(n + "_NICK")
	b.user = os.Getenv(n + "_USER")
	b.host = os.Getenv(n + "_HOST")
	b.port = os.Getenv(n + "_PORT")

	if b.nick == "" {
		return nil, ErrBadConfig
	}

	if b.user == "" {
		return nil, ErrBadConfig
	}

	if b.host == "" {
		return nil, ErrBadConfig
	}

	if b.port == "" {
		return nil, ErrBadConfig
	}

	b.IrcObj = irc.IRC(b.nick, b.user)
	if b.IrcObj == nil {
		return nil, ErrBadConfig
	}

	err = b.IrcObj.Connect(b.host + ":" + b.port)
	if err != nil {
		return nil, err
	}

	go b.IrcObj.Loop()

	return
}

// Join makes a bot join a channel.
//
// If the bot is already in said channel, it will return false. If it had to
// join the channel, it will return true.
//
// TODO: handle being banned
func (b *Bot) Join(channel string) bool {
	for _, mychan := range b.channels {
		if strings.ToLower(channel) == strings.ToLower(mychan) {
			return false
		}
	}

	b.IrcObj.Join(channel)

	b.channels = append(b.channels, channel)

	return true
}

// Talk speaks to a channel with a given message.
func (b *Bot) Talk(channel, message string) {
	if b.Join(channel) {
		time.Sleep(250 * time.Millisecond)
	}

	b.IrcObj.Privmsg(channel, message)
}
