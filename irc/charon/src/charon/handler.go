package main

import (
	"bytes"
	"charon/myconfig"
	"fmt"
	"io"
	"irc/message"
	"os"
	"strings"
	"time"

	"github.com/dchest/blake2b"
)

func QuitCommandHandler(user *User, line *message.Message) {
	var reason string

	if len(line.Args) > 0 {
		reason = line.Args[0]
	} else {
		reason = config.Default.QuitReason
	}

	user.Quit(reason)

	if user.oper {
		opercount--
	}

	logger.Printf("User %s Quit (%s)", user.nick, reason)
}

func NickHandler(user *User, line *message.Message) {
	oldnick := user.nick

	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NONICKNAMEGIVEN)
		return
	}

	if NickHasBadChars(line.Args[0]) {
		user.FireNumeric(ERR_ERRONEOUSNICKNAME, line.Args[0])
		return
	}

	if GetUserByNick(line.Args[0]) != nil {
		user.FireNumeric(ERR_NICKNAMEINUSE, line.Args[0])
		return
	}

	if !user.nickset {
		user.nickset = true
	} else if user.registered {
		targets := []*User{}
		targets = append(targets, user)

		for _, k := range user.chanlist {
			targets = append(targets, k.GetUserList()...)
		}

		SendToMany(fmt.Sprintf(":%s NICK %s", user.GetHostMask(), line.Args[0]), targets)

		conn := RedisPool.Get()
		defer conn.Close()

		_, err := conn.Do("HSET", "charon:user:"+user.uuid, "nick", line.Args[0])
		if err != nil {
			logger.Printf("Could not update nick for charon:user:%s : %#v", user.uuid, err)
		}

		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "NICK",
			Args:   []string{user.uuid, line.Args[0]},
		})
	}

	user.nick = line.Args[0]

	logger.Printf("User %s changed nick to %s", oldnick, user.nick)

	if !user.registered && user.userset {
		user.UserRegistrationFinished()
	}
}

func UserHandler(user *User, line *message.Message) {
	if len(line.Args) < 4 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "USER")
		return
	}

	if !user.userset {
		user.ident = line.Args[0]
		user.realname = line.Args[3]
		user.userset = true

		if !user.registered && user.nickset {
			user.UserRegistrationFinished()
		}

		time.Sleep(1 * time.Second) //XXX remove this?

		return
	}

	CommandNotFound(user, line)
}

func CommandNotFound(user *User, line *message.Message) {
	logger.Printf("User %s attempted unknown command %s", user.nick, line.Verb)

	user.FireNumeric(ERR_UNKNOWNCOMMAND, line.Verb)
}

func JoinHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "JOIN")
		return
	}

	for _, channame := range strings.Split(line.Args[0], ",") {
		if !ValidChanName(channame) {
			user.FireNumeric(ERR_NOSUCHCHANNEL, channame)
			return
		}

		//TODO: support channel keys

		channel := GetChannelByName(channame)
		if channel == nil {
			channel = NewChannel(channame)
		}

		if channel.HasMode("A") && !user.oper {
			user.FireNumeric(ERR_NOSUCHCHANNEL, channel.name)
			logger.Printf("User %s tried to join %s while +A was set.", user.nick, channel.name)
			return
		}

		if channel.HasUser(user) {
			logger.Printf("User %s tried to join %s while already joined.", user.nick, channel.name)
			return
		}

		if channel.IsUserBanned(user) && !user.oper {
			user.FireNumeric(RPL_BANNEDFROMCHAN, channel.name)
			logger.Printf("User %s tried to join %s while banned.", user.nick, channel.name)
			return
		}

		channel.JoinUser(user)
		user.chanlist[channel.name] = channel

		c := RedisPool.Get()
		defer c.Close()

		_, err := c.Do("LPUSH", "charon:channelmembers:"+channel.name, user.uuid)
		if err != nil {
			logger.Printf(
				"Error adding %s to %s joinlist in redis: %#v",
				user.GetHostMask(),
				channel.name,
				err,
			)
		}

		logger.Printf("User %s joined %s", user.nick, channel.name)
	}
}

func LusersHandler(user *User, line *message.Message) {
	user.FireNumeric(RPL_LUSERCLIENT, len(userlist), 1, 1)
	user.FireNumeric(RPL_LUSEROP, opercount)
	user.FireNumeric(RPL_LUSERCHANNELS, len(chanlist))
	user.FireNumeric(RPL_LUSERME, len(userlist), 1)
}

func PartHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "PART")
		return
	}

	var reason string

	if len(line.Args) > 1 {
		reason = line.Args[1]
	} else {
		reason = config.Default.PartReason
	}

	channel := GetChannelByName(line.Args[0])

	if channel != nil {
		channel.SendLinef(":%s PART %s :%s", user.GetHostMask(), channel.name, reason)
		delete(channel.userlist, user.id)
		delete(user.chanlist, channel.name)
		delete(channel.usermodes, user)
		logger.Printf("User %s PART %s: %s", user.nick, channel.name, reason)

		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "PART",
			Args: []string{
				channel.name,
				user.uuid,
				reason,
			},
		})

		c := RedisPool.Get()
		defer c.Close()

		_, err := c.Do("LREM", "charon:channelmembers:"+channel.name, "1", user.uuid)
		if err != nil {
			logger.Printf(
				"Could not log part of %s from %s to redis: %#v",
				user.GetHostMask(),
				channel.name,
				err,
			)
		}

		channel.ShouldIDie()
	} else {
		user.FireNumeric(ERR_NOSUCHCHANNEL, line.Args[0])
	}
}

func MessageHandler(user *User, line *message.Message) {
	if len(line.Args) < 2 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, line.Verb)
		return
	}

	//is ValidChanName even needed here anymore?
	if ValidChanName(line.Args[0]) { //TODO part of this should be sent to the channel "object"
		//presumably a channel
		j := GetChannelByName(line.Args[0])

		if j != nil {
			if j.HasMode("n") && !user.IsIn(j) {
				user.FireNumeric(ERR_CANNOTSENDTOCHAN, j.name)
				return
			}

			userpriv := j.GetUserPriv(user)

			if j.HasMode("m") && userpriv < 10 {
				user.FireNumeric(ERR_CANNOTSENDTOCHAN, j.name)
				return
			}

			if j.IsUserBanned(user) && userpriv < 10 {
				user.FireNumeric(ERR_CANNOTSENDTOCHAN, j.name)
				return
			}

			//channel exists, send the message
			msg := line.Args[1]

			c, _ := j.GetCount()

			if j.HasMode("4") {
				AddToMode4Cache(user, c, j)
			}

			list := j.GetUserList()

			for _, l := range list {
				if l != user && l.local {
					if j.HasMode("4") {
						Mode4CacheMutex.Lock()

						var h bool
						var num int64

						for e := Mode4Cache.Front(); e != nil; e = e.Next() {
							ci := e.Value.(Mode4CacheItem)
							if ci.channel == j && strings.Contains(msg, fmt.Sprintf("%d", ci.number)) && ci.user == l {
								h = true
								num = ci.number

								break
							}
						}

						Mode4CacheMutex.Unlock()

						if h {
							l.SendLinef(":%s %s %s :%s", fmt.Sprintf("%d!%d@%d", c, c, c), line.Verb, j.name, strings.Replace(msg, fmt.Sprintf("%d", num), l.nick, -1))
						} else {
							l.SendLinef(":%s %s %s :%s", fmt.Sprintf("%d!%d@%d", c, c, c), line.Verb, j.name, msg)
						}
					} else {
						l.SendLinef(":%s %s %s :%s", user.GetHostMask(), line.Verb, j.name, msg)
					}
				}
			}

			SendLineToRedis("global", &message.Message{
				Source: SID,
				Verb:   line.Verb,
				Args:   []string{user.uuid, j.name, msg},
			})

			var loggerchan bool

			for _, testc := range config.Log.Channel {
				if GetChannelByName(testc) == j {
					loggerchan = true
				}
			}

			//TODO: remove?
			if !loggerchan && !config.Server.Privacy {
				switch line.Verb {
				case "PRIVMSG":
					logger.Printf("User %s CHANMSG %s: %s", user.nick, j.name, msg)
				case "NOTICE":
					logger.Printf("User %s CHANNOTICE %s: %s", user.nick, j.name, msg)
				}
			}

			return
		} else {
			user.FireNumeric(ERR_NOSUCHCHANNEL, line.Args[0])

			return
		}
	}

	//maybe its a user
	target := GetUserByNick(line.Args[0])

	if target != nil && target.local {
		msg := line.Args[1]
		target.SendLinef(":%s %s %s :%s", user.GetHostMask(), line.Verb, target.nick, msg)
	} else {
		SendLineToRedis(target.sid, &message.Message{
			Source: SID,
			Verb:   line.Verb,
			Args:   []string{user.uuid, target.uuid, line.Args[1]},
		})
	}
}

func TopicHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "TOPIC")
		return
	}

	k := GetChannelByName(line.Args[0])

	if k == nil {
		user.FireNumeric(ERR_NOSUCHCHANNEL, line.Args[0])
		return
	}

	if len(line.Args) < 2 {
		k.FireTopic(user)
		return
	}

	if k.GetUserPriv(user) < 100 && k.HasMode("t") {
		return //doesn't have privs to change channel
		// TODO fire the correct numeric
	}

	msg := line.Args[1]

	k.SetTopic(msg, user.GetHostMask())
}

func ModeHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "MODE")
		return
	}

	if ChanUserNone(line.Args[0]) == 1 {
		channel := GetChannelByName(line.Args[0])

		if len(line.Args) < 2 {
			//just digging around...
			channel.FireModes(user)
		} else {
			s := line.Args[1]
			mode := 2
			mcounter := 0

			var targs []string

			if len(line.Args) > 2 {
				targs = line.Args[2:]
			}

			for _, k := range s {
				switch k {
				case '+':
					mode = 2
					break
				case '-':
					mode = 1
					break
				case 'o', 'v':
					if len(targs)-1 < mcounter {
						user.FireNumeric(ERR_NEEDMOREPARAMS, "MODE")
						break
					}

					target := GetUserByNick(targs[mcounter])

					if target == nil {
						user.FireNumeric(ERR_NOSUCHNICK, line.Args[mcounter])
						mcounter = +1
						break
					}

					if !channel.HasUser(target) {
						user.FireNumeric(ERR_USERNOTINCHANNEL, target.nick, channel.name)
						mcounter = +1
						break
					}

					if mode == 2 {
						channel.SetUmode(target, user, string(k))
						mcounter = +1
						break
					}

					if mode == 1 {
						channel.UnsetUmode(target, user, string(k))
						mcounter = +1
					}

				case 'b':
					if len(targs)-1 < mcounter {
						channel.FireBanlist(user)
						break
					}

					if mode == 2 {
						channel.SetBan(targs[mcounter], user)
						mcounter++
						break
					}

					if mode == 1 {
						channel.UnsetBan(targs[mcounter], user)
						mcounter++
						break
					}

				case 't', 'n', 'm', 'A', '4', 'P':
					if mode == 2 {
						channel.SetMode(string(k), user)
					} else if mode == 1 {
						channel.UnsetMode(string(k), user)
					}
					break
				}
			}
		}
	}
}

func PingHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "PING")
		return
	}

	user.SendLinef(":%s PONG %s :%s", config.Server.Name, config.Server.Name, line.Args[0])
}

func WhoHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "WHO")
		return
	}

	whotype := ChanUserNone(line.Args[0])

	if whotype == 0 {
		//its a channel
		k := GetChannelByName(line.Args[0])
		line.Args[0] = k.name // normalizing case

		for _, j := range k.userlist {
			h := "H" + k.GetUserPrefix(j) // TODO: add away status
			user.FireNumeric(RPL_WHOREPLY, k.name, j.ident, j.host, config.Server.Name, j.nick, h, ":0", j.realname)
		}

	} else if whotype == 2 {
		//user
		k := GetUserByNick(line.Args[0])
		line.Args[0] = k.nick
		user.FireNumeric(RPL_WHOREPLY, "*", k.ident, k.host, config.Server.Name, k.nick, "H", ":0", k.realname) // TODO: add away status
	}

	user.FireNumeric(RPL_ENDOFWHO, line.Args[0])
}

func KickHandler(user *User, line *message.Message) {
	if len(line.Args) < 2 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "KICK")
		return
	}

	channel := GetChannelByName(line.Args[0])

	if channel == nil {
		user.FireNumeric(ERR_NOSUCHCHANNEL, line.Args[0])
		return
	}

	target := GetUserByNick(line.Args[1])

	if target == nil {
		user.FireNumeric(ERR_NOSUCHNICK, line.Args[1])
		return
	}

	if channel.GetUserPriv(user) < 100 {
		user.FireNumeric(ERR_CHANOPRIVSNEEDED, channel.name)
		return
	}

	if !channel.HasUser(target) {
		user.FireNumeric(ERR_USERNOTINCHANNEL, target.nick, channel.name)
		return
	}

	if user.system {
		// TODO: numeric here
		return //This could be bad.
	}

	var reason string

	if len(line.Args) > 2 {
		reason = line.Args[1]
	} else {
		reason = config.Default.KickReason
	}

	channel.SendLinef(":%s KICK %s %s :%s", user.GetHostMask(), channel.name, target.nick, reason)
	delete(channel.userlist, target.id)
	delete(target.chanlist, channel.name)
	delete(channel.usermodes, target)
	logger.Printf("%s kicked %s from %s", user.nick, target.nick, channel.name)

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "KICK",
		Args: []string{
			channel.name,
			user.uuid,
			target.uuid,
			reason,
		},
	})

	channel.ShouldIDie()
}

func ListHandler(user *User, line *message.Message) {
	// TODO: extended LIST support or ALIS searching
	user.FireNumeric(RPL_LISTSTART)

	for _, k := range chanlist {
		user.FireNumeric(RPL_LIST, k.name, len(k.userlist), k.topic)
	}

	user.FireNumeric(RPL_LISTEND)
}

func MkPasswdHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "MKPASSWD")
		return
	}

	c := &blake2b.Config{
		Salt: []byte(config.Server.Salt),
		Size: blake2b.Size,
	}

	b2b, err := blake2b.New(c)
	if err != nil {
		panic(err)
	}

	b2b.Reset()

	fin := bytes.NewBufferString(line.Args[0])

	io.Copy(b2b, fin)
	sum := b2b.Sum(nil)

	user.SendLinef(
		":%s NOTICE %s :Sum of `%s` is: %x",
		config.Server.Name,
		user.nick,
		line.Args[0],
		sum,
	)
}

func OperHandler(user *User, line *message.Message) {
	if len(line.Args) < 2 {
		CommandNotFound(user, line)
		return
	}

	//TODO: Add password hashing via b2sum -- Xena
	oper, ok := config.Oper[line.Args[0]]
	if !ok {
		CommandNotFound(user, line)
		return
	}

	if oper.Encrypted {
		hash := Blake2bString(line.Args[1])

		if hash == oper.Password {
			user.oper = true
			opercount++
			user.FireNumeric(RPL_YOUREOPER)
			user.SendLinef(":%s MODE %s +o", config.Server.Name, user.nick)

			defer announceOper(user, oper)
		}
	} else {
		if oper.Password == line.Args[1] {
			user.oper = true

			opercount++

			user.FireNumeric(RPL_YOUREOPER)
			user.SendLinef(":%s MODE %s +o", config.Server.Name, user.nick)

			defer announceOper(user, oper)
		} else {
			logger.Printf("User %s failed OPER attempt, killing", user.GetHostMask())
			KillHandler(SystemUser, &message.Message{Args: []string{user.nick, "Failed OPER attempt"}})
		}
	}
}

func announceOper(user *User, oline *myconfig.Oper) {
	conn := RedisPool.Get()
	defer conn.Close()

	_, err := conn.Do("HSET", "charon:user:"+user.uuid, "oper", "true")
	if err != nil {
		logger.Printf("Error in updating oper flag for charon:user:%s : %#v", user.uuid, err)
	}

	conn.Do("PUBLISH", "oper", user.uuid)
	conn.Flush()

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "OPER",
		Args:   []string{user.uuid},
	})

	if oline.Vhost != "" {
		user.ChgHost(oline.Vhost)
	}
}

func NamesHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "NAMES")
		return
	}

	channel := GetChannelByName(line.Args[0])

	if channel == nil {
		user.FireNumeric(ERR_NOSUCHCHANNEL, line.Args[0])
		return
	}

	channel.FireNames(user)
}

func RehashHandler(user *User, line *message.Message) {
	if user.oper {
		SetupConfig(*configFlag)
		user.FireNumeric(RPL_REHASHING, *configFlag)
		logger.Printf("OPER %s requested rehash...", user.nick)
	} else {
		CommandNotFound(user, line)
	}
}

func ShutdownHandler(user *User, line *message.Message) {
	//TODO: rename to DIE -- Xena
	if user.oper {
		logger.Printf("Shutdown requested by OPER %s", user.nick)

		reason := fmt.Sprintf("Server is being shutdown by %s", user.nick)

		for _, k := range userlist {
			k.Quit(reason)
		}

		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "OFFLINE",
			Args: []string{
				time.Now().Format(time.RFC3339),
				reason,
			},
		})

		conn := RedisPool.Get()

		// Clean up redis
		mysection := strings.Split(SID, "-")[4]
		conn.Do("EVAL", `local l = 0 for _,k in ipairs(redis.call('keys','charon:*`+mysection+`')) do redis.call('del',k) l = l + 1 end return l`, "0")

		conn.Close()

		os.Exit(0)
	} else {
		CommandNotFound(user, line)
	}
}

func KillHandler(user *User, line *message.Message) {
	if user.oper {
		if len(line.Args) < 2 {
			user.FireNumeric(ERR_NEEDMOREPARAMS, "KILL")
			return
		}

		target := GetUserByNick(line.Args[0])

		if target == nil {
			user.FireNumeric(ERR_NOSUCHNICK, line.Args[0])
			return
		}

		var reason string
		if len(line.Args) > 2 {
			reason = line.Args[1]
		} else {
			reason = config.Default.KillReason
		}

		target.Quit(fmt.Sprintf("KILL: %s", reason))
		logger.Printf("oper %s killed %s (%s)", user.nick, target.nick, reason)

	} else {
		CommandNotFound(user, line)
	}
}

func WhoisHandler(user *User, line *message.Message) {
	if len(line.Args) < 1 {
		user.FireNumeric(ERR_NEEDMOREPARAMS, "WHOIS")
		return
	}

	target := GetUserByNick(line.Args[0])

	if target == nil {
		user.FireNumeric(ERR_NOSUCHNICK, line.Args[0])

		return
	}

	var buf bytes.Buffer

	// TODO: +s channels
	for _, k := range target.chanlist {
		buf.WriteString(k.name + " ")
	}

	chanstring := strings.TrimSpace(buf.String())

	user.FireNumeric(RPL_WHOISUSER, target.nick, target.ident, target.host, target.realname)
	user.FireNumeric(RPL_WHOISCHANNELS, target.nick, chanstring)

	server := GetServerByUUID(target.sid)
	user.FireNumeric(RPL_WHOISSERVER, target.nick, server.name, server.description)

	if target.oper {
		user.FireNumeric(RPL_WHOISOPERATOR, target.nick)
	}

	if user.oper || user == target {
		user.FireNumeric(RPL_WHOISHOST, target.nick, target.realhost, target.realip)
	} else {
		user.FireNumeric(RPL_WHOISHOST, target.nick, target.host, target.ip)
	}

	user.FireNumeric(RPL_ENDOFWHOIS, target.nick)
}

func AddToMode4Cache(user *User, number int64, channel *Channel) {
	Mode4CacheMutex.Lock()
	defer Mode4CacheMutex.Unlock()
	if Mode4Cache.Len() > 1000 {
		for i := 0; i < 500; i++ {
			Mode4Cache.Remove(Mode4Cache.Back())
		}
	}
	k := Mode4CacheItem{}
	k.user = user
	k.number = number
	k.channel = channel
	Mode4Cache.PushFront(k)
}
