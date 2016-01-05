package main

import (
	"irc/message"
	"strconv"
	"time"

	"github.com/garyburd/redigo/redis"
)

type RemoteHandler func(*message.Message)

var RemoteHandlers map[string]RemoteHandler

func init() {
	RemoteHandlers = map[string]RemoteHandler{
		"CHGHOST": ChghostRemoteHandler,
		"CONNECT": ConnectRemoteHandler,
		"CMODE":   CmodeRemoteHandler,
		"JOIN":    JoinRemoteHandler,
		"NEWCHAN": NewchanRemoteHandler,
		"NOTICE":  MessageRemoteHandler,
		"ONLINE":  OnlineRemoteHandler,
		"OFFLINE": OfflineRemoteHandler,
		"OPER":    OperRemoteHandler,
		"PART":    PartRemoteHandler,
		"PRIVMSG": MessageRemoteHandler,
	}
}

func OperRemoteHandler(line *message.Message) {
	u := GetUserByUUID(line.Args[0])

	u.oper = true
}

func ChghostRemoteHandler(line *message.Message) {
	u := GetUserByUUID(line.Args[0])

	u.host = line.Args[1]
}

func JoinRemoteHandler(line *message.Message) {
	ch := GetChannelByName(line.Args[0])

	if ch == nil {
		panic("Asked to join " + line.Args[1] + " to " + line.Args[0] + " which doesn't exist?")
	}

	u := GetUserByUUID(line.Args[1])

	ch.userlist[u.id] = u
	ch.SendLinef(
		":%s JOIN %s",
		u.GetHostMask(),
		ch.name,
	)
}

func PartRemoteHandler(line *message.Message) {
	ch := GetChannelByName(line.Args[0])

	if ch == nil {
		panic("Asked to part " + line.Args[1] + " from " + line.Args[0] + " which doesn't exist?")
	}

	u := GetUserByUUID(line.Args[1])

	ch.SendLinef(
		":%s PART %s :%s",
		u.GetHostMask(),
		ch.name,
		line.Args[2],
	)

	delete(ch.userlist, u.id)
	delete(u.chanlist, ch.name)
	delete(ch.usermodes, u)

	logger.Printf(
		"User %s PART %s: %s",
		u.nick,
		ch.name,
		line.Args[2],
	)
}

func CmodeRemoteHandler(line *message.Message) {
	switch line.Args[0] {
	case "ADD":
		who := GetUserByUUID(line.Args[1])
		channel := GetChannelByName(line.Args[2])

		channel.SetMode(line.Args[3], who)
	}
}

func MessageRemoteHandler(line *message.Message) {
	src := GetUserByUUID(line.Args[0])
	j := GetChannelByName(line.Args[1])

	if j != nil {
		// Channel message
		j.SendLinef(
			":%s %s %s :%s",
			src.GetHostMask(),
			line.Verb,
			j.name,
			line.Args[2],
		)
	} else {
		// Private message
		target := GetUserByUUID(line.Args[1])

		if target.local {
			target.SendLinef(
				":%s %s %s :%s",
				src.GetHostMask(),
				line.Verb,
				target.nick,
				line.Args[2],
			)
		}
	}
}

func OnlineRemoteHandler(line *message.Message) {
	s := NewServer(line)

	logger.Printf(
		"New %s server %s (%s) connected",
		s.kind,
		s.name,
		s.uuid,
	)
}

func OfflineRemoteHandler(line *message.Message) {
	s, ok := serverlist[line.Source]
	if !ok {
		logger.Printf(
			"Asked for %s to go offline but is unknown...",
			line.Source,
		)

		return
	}

	for _, k := range userlist {
		if !k.oper || !k.local {
			continue
		}

		k.SendLinef(
			":%s NOTICE %s :Server %s has gone offline as of %s: %s",
			config.Server.Name,
			k.nick,
			s.name,
			line.Args[0],
			line.Args[1],
		)
	}

	serverlist[line.Source] = nil
}

func ConnectRemoteHandler(line *message.Message) {
	u := NewUser()

	u.uuid = line.Args[0]
	u.nick = line.Args[1]
	u.ident = line.Args[2]
	u.host = line.Args[3]
	u.realhost = line.Args[4]
	u.realip = line.Args[5]
	u.account = line.Args[6]
	u.system = line.Args[7] == "true"
	u.realname = line.Args[8]
	u.sid = line.Source
	u.local = false
}

func ProcessRemoteHandlers() {
	c := RedisPool.Get()
	psc := redis.PubSubConn{c}

	psc.Subscribe("global", SID)

	if *debugFlag {
		logger.Printf(
			"Subscribed to `global`, `%s`",
			SID,
		)
	}

	for {
		switch n := psc.Receive().(type) {
		case redis.Message:
			line := message.ParseMessage(string(n.Data))

			if line.Source == SID {
				continue // Ignore messages from ourself.
			}

			if *debugFlag {
				logger.Printf(
					"Receive from Redis (%s): %s",
					n.Channel,
					n.Data,
				)
			}

			if handler, ok := RemoteHandlers[line.Verb]; ok {
				handler(line)
			} else {
				logger.Printf("Remote: Could not understand %s", n.Data)
			}
		}
	}
}

func NewchanRemoteHandler(line *message.Message) {
	c_ := GetChannelByName(line.Args[0])
	if c_ != nil {
		return
	}

	c := NewChannel(line.Args[0])
	c.cmodes = line.Args[1]

	epoch, err := strconv.ParseInt(line.Args[2], 10, 64)
	if err != nil {
		panic(err)
	}

	c.epoch = time.Unix(epoch, 0)
}
