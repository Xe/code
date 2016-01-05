package main

import (
	"irc/message"
	"log"
	"strings"
)

func GetUserByNick(nick string) *User {
	nick = strings.ToLower(nick)
	if nick == strings.ToLower(SystemUser.nick) {
		return SystemUser
	}

	for _, k := range userlist {
		if strings.ToLower(k.nick) == nick {
			return k
		}
	}

	return nil
}

func GetUserByUUID(uuid string) *User {
	for _, l := range userlist {
		if uuid == l.uuid {
			return l
		}
	}

	return nil
}

func GetServerByUUID(uuid string) *Server {
	for _, s := range serverlist {
		if uuid == s.uuid {
			return s
		}
	}

	return nil
}

func SetUserIPInfo(user *User) {
	ip := user.connections[0].RemoteAddr().String()

	if !strings.HasPrefix(ip, "[") {
		//ipv4
		user.ConnType = "IP4"
		user.realip = strings.Split(ip, ":")[0]
	} else {
		user.ConnType = "IP6"
		ip = strings.Split(ip, "]")[0]
		ip = strings.TrimPrefix(ip, "[")
		user.realip = ip
	}
}

func GetChannelByName(name string) *Channel {
	return chanlist[strings.ToLower(name)]
}

func SendToMany(msg string, list []*User) {
	users := make(map[*User]int)
	for _, j := range list {
		users[j] = 0
	}

	for j, _ := range users {
		j.SendLine(msg)
	}
}

func SendToManyButOne(user *User, msg string, list []*User) {
	mylist := []*User{}

	for _, u := range list {
		if u.id == user.id {
			log.Printf("Ignoring for %s", u.GetHostMask())
			continue
		}

		mylist = append(mylist, u)
	}

	SendToMany(msg, list)
}

func ValidChanName(name string) bool {
	if ChanHasBadChars(name) {
		return false
	}
	for _, k := range valid_chan_prefix {
		if strings.HasPrefix(name, k) {
			return true
		}
	}
	return false
}

func NickHasBadChars(nick string) bool {
	for _, k := range global_bad_chars {
		if strings.Contains(nick, k) {
			return true
		}
	}
	for _, k := range valid_chan_prefix {
		if strings.Contains(nick, k) {
			return true
		}
	}
	return false
}

func ChanHasBadChars(nick string) bool {
	for _, k := range global_bad_chars {
		if strings.Contains(nick, k) {
			return true
		}
	}
	return false
}

func ChanUserNone(name string) int {
	if GetChannelByName(name) != nil {
		return 1
	} else if GetUserByNick(name) != nil {
		return 2
	} else {
		return 0
	}
}

func WildcardMatch(text string, pattern string) bool {
	cards := strings.Split(pattern, "*")
	for _, card := range cards {
		index := strings.Index(text, card)
		if index == -1 {
			return false
		}
		text = text[index+len(card):]
	}
	return true
}

func SetupSystemUser() {
	line := []interface{}{
		"charon:user:" + SystemUser.uuid,
		"nick", SystemUser.nick,
		"user", SystemUser.ident,
		"host", SystemUser.host,
		"ip", SystemUser.realip,
		"realhost", SystemUser.realhost,
		"gecos", SystemUser.realname,
		"account", SystemUser.account,
		"oper", "true",
		"service", "true",
	}

	conn := RedisPool.Get()

	defer conn.Close()

	_, err := conn.Do("HMSET", line...)
	if err != nil {
		logger.Printf("Error in HMSET %#v: %#v", line, err)
	}

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "CONNECT",
		Args: []string{
			SystemUser.uuid,     // uuid of user
			SystemUser.nick,     // nickname of user
			SystemUser.ident,    // username of user
			SystemUser.host,     // hostname of user
			SystemUser.realhost, // real host of user
			SystemUser.realip,   // IP address of user
			SystemUser.account,  // Account name of user
			"true",              // Services user?
			SystemUser.realname, // gecos of user
		},
	})

	for _, k := range config.Log.Channel {
		NewChannel(k)
		ModeHandler(SystemUser, &message.Message{Args: []string{k, "+AP"}})
	}
}

func SendLineToRedis(group string, line *message.Message) (err error) {
	conn := RedisPool.Get()

	if *debugFlag {
		logger.Printf(
			"Send to Redis (%s): %s",
			group,
			line.String(),
		)
	}

	_, err = conn.Do("PUBLISH", group, line.String())
	if err != nil {
		return err
	}

	err = conn.Flush()

	return err
}
