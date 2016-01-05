package cqbot

import (
	"bufio"
	"fmt"
	"log"
	"net"
	"net/textproto"
	"os"
	"regexp"
	"strings"
	"time"
)

// A Bot is the base struct for all of cqbot's functionality. It is planned
// to be split up into a few other structs later.
type Bot struct {
	server   string
	port     string
	nick     string
	user     string
	gecos    string
	handlers map[string]map[string]*Handler
	Commands map[string]*Command
	Channels map[string]*Channel
	Scripts  map[string]*Script
	Config   Config
	Conn     net.Conn
	Log      *log.Logger
}

// Allocate the maps and seed the configuration details of the bot
// from the configuration file. Also specify handlers for common IRC
// protocol verbs.
func NewBot(confname string) (bot *Bot) {
	bot = &Bot{
		handlers: make(map[string]map[string]*Handler),
		Config:   LoadConfig(confname),
		Commands: make(map[string]*Command),
		Channels: make(map[string]*Channel),
		Scripts:  make(map[string]*Script),
		Log:      log.New(os.Stdout, "", log.LstdFlags),
	}

	bot.nick = bot.Config.Bot.Nick
	bot.user = bot.Config.Bot.User
	bot.gecos = bot.Config.Bot.Gecos
	bot.server = bot.Config.Server.Host
	bot.port = bot.Config.Server.Port

	bot.AddHandler("PING", func(line *Line) {
		bot.SendLine("PONG " + line.Args[0])
	})

	bot.AddHandler("PRIVMSG", func(line *Line) {
		regex, err := regexp.Compile("\x1f|\x02|\x12|\x0f|\x16|\x03(?:\\d{1,2}(?:,\\d{1,2})?)?")
		if err != nil {
			panic(err)
		}

		strippedmessage := regex.ReplaceAll([]byte(line.Args[1]), []byte(""))

		if string(strippedmessage[0]) != bot.Config.Bot.Prefix {
			return
		}

		message := strings.Split(string(strippedmessage), " ")
		verb := strings.ToUpper(message[0][1:])
		source := NewUser(line.Source)

		if _, present := bot.Commands[verb]; present {
			command := bot.Commands[verb]
			go func() {
				response := command.Func(source, message[1:])
				bot.Say(line.Args[0], response)
			}()
		} else {
			bot.Notice(source.Nick, "Unknown command %s", verb)
		}
	})

	bot.AddHandler("001", func(line *Line) {
		go func() {
			bot.Log.Printf("Connected to %s!", strings.Split(line.Args[1], " ")[3])
			bot.Privmsg("NickServ", "ID %s", bot.Config.Bot.Nspass)
			time.Sleep(2 * time.Second)
			joinstring := ""
			for _, str := range strings.Split(bot.Config.Bot.Channel, " ") {
				joinstring = joinstring + ",#" + str
			}
			bot.SendLine("JOIN " + joinstring)
		}()
	})

	bot.AddHandler("NICK", func(line *Line) {
		if source := NewUser(line.Source); source.Nick == bot.nick {
			bot.nick = line.Args[0]
		}
	})

	return
}

// Get the bot's nickname. Keeping the nickname variable hidden for now to
// avoid possible desyncs.
func (bot *Bot) GetNick() (nick string) {
	return bot.nick
}

// Request a nickname change with the daemon
func (bot *Bot) SetNick(nick string) {
	bot.SendLine("NICK " + nick)
}

// Ask the bot to connect to the server specified in the config.
func (bot *Bot) Connect() (err error) {
	bot.Conn, err = net.Dial("tcp", bot.server+":"+bot.port)
	if err != nil {
		bot.Log.Printf("Error: %v", err)
		time.Sleep(10 * time.Second)
		bot.Connect()
		return
	}

	bot.Auth()

	return
}

// Have the bot quit
func (bot *Bot) Quit(message string) {
	bot.SendLine("QUIT :" + message)
	bot.Conn.Close()
}

// Auth with the IRC daemon. This will be more complicated over time as things
// like CAP handling are added.
func (bot *Bot) Auth() (e error) {
	bot.SendLine("NICK " + bot.nick)
	bot.SendLine("USER " + bot.user + " * * :" + bot.gecos)

	return nil
}

// Have the bot send an arbitrary IRC line.
// TODO: have this check for newlines
func (bot *Bot) SendLine(line string) {
	bot.Log.Printf("OUT< %s", line)
	fmt.Fprintf(bot.Conn, "%s\r\n", line)
}

// A wrapper around PRIVMSG and NOTICE
func (bot *Bot) sendMessage(target string, kind string, format string, stuff ...interface{}) {
	bot.SendLine(kind + " " + target + " :" + fmt.Sprintf(format, stuff...))
}

// Sends a PRIVMSG towards the target you specify. Supports printf syntax.
func (bot *Bot) Privmsg(target string, format string, stuff ...interface{}) {
	bot.sendMessage(target, "PRIVMSG", fmt.Sprintf(format, stuff...))
}

// Sends a NOTICE towards the target you specify. Supports printf syntax.
func (bot *Bot) Notice(target string, format string, stuff ...interface{}) {
	bot.sendMessage(target, "NOTICE", fmt.Sprintf(format, stuff...))
}

// PRIVMSG a target with the appropriate color codes.
func (bot *Bot) Say(target string, format string, stuff ...interface{}) {
	bot.Privmsg(target, "\x0315,12%s", fmt.Sprintf(format, stuff...))
}

// Main loop
func (bot *Bot) HandleCommands() {
	reader := bufio.NewReader(bot.Conn)
	tp := textproto.NewReader(reader)

	defer bot.Quit("Goodbye!")

	for {
		line, err := tp.ReadLine()
		if err != nil {
			break
		}

		myLine := NewLine(line)

		if myLine.Verb == "ERROR" {
			bot.Log.Printf("!!!! I was killed !!!!")
			bot.Log.Printf("!!!! %s !!!!", line)
			time.Sleep(60 * time.Second)
			bot.Connect()
			bot.HandleCommands()
			break
		}

		if impl, ok := bot.handlers[myLine.Verb]; ok {
			bot.Log.Printf("IN>> %s", line)

			for _, handler := range impl {
				/*if handler.Script == nil {
					bot.Log.Printf("Handler core:%s:%s being called", handler.verb, handler.Uid)
				} else {
					bot.Log.Printf("Handler %s:%s:%s being called", handler.Script.Name, handler.verb, handler.Uid)
				}*/
				go handler.impl(myLine)
			}
		} else {
			bot.Log.Printf("IN>> %s", line)
		}
	}
}
