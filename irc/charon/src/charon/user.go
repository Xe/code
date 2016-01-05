package main

import (
	"bufio"
	"fmt"
	"irc/message"
	debuglogger "log"
	"net"
	"strings"
	"time"

	"github.com/Xe/uuid"
)

type User struct {
	// Information flags
	nick     string
	ident    string
	id       int
	uuid     string
	realname string
	host     string
	realhost string
	epoch    time.Time
	ip       string
	realip   string
	account  string
	chanlist map[string]*Channel
	sid      string

	// Bookkeeping flags
	dead        bool
	nickset     bool
	userset     bool
	registered  bool
	waiting     bool
	connections []net.Conn
	lastrcv     time.Time
	nextcheck   time.Time
	ConnType    string
	resolved    chan bool

	// Privilege flags
	oper   bool
	system bool
	local  bool
}

func (user *User) Quit(reason string) {
	targets := []*User{user}

	for _, k := range user.chanlist {
		targets = append(targets, k.GetUserList()...)
		delete(k.userlist, user.id)
		delete(user.chanlist, k.name)
		delete(k.usermodes, user)
		k.ShouldIDie()
	}

	SendToMany(fmt.Sprintf(":%s QUIT :%s", user.GetHostMask(), reason), targets)
	user.SendLinef("ERROR :Closing Link: %s (%s)", user.host, reason)
	user.dead = true

	if user.connections[0] != nil {
		user.connections[0].Close()
	}

	delete(userlist, user.id)

	conn := RedisPool.Get()
	defer conn.Close()

	err := SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "QUIT",
		Args:   []string{user.uuid, reason},
	})
	if err != nil {
		logger.Printf("Could not publish quit for %s: %#v", user.uuid, err)
	}

	_, err = conn.Do("DEL", `"charon:user:`+user.uuid+`"`)
	if err != nil {
		logger.Printf("Could not delete charon:user:%s from redis: %#v", user.uuid, err)
	}

	_, err = conn.Do("EVAL", "for _,k in ipairs(redis.call('keys','charon:user:"+user.uuid+"*')) do redis.call('del',k) end", "0")
	if err != nil {
		logger.Printf("Could not delete charon:user:%s* from redis: %#v", user.uuid, err)
	}

	conn.Flush()
}

func (user *User) ChgHost(newhost string) (err error) {
	var targets []*User

	for _, k := range user.chanlist {
		var mytargets []*User

		for _, cuser := range k.GetUserList() {
			if cuser.id == user.id {
				continue
			}

			mytargets = append(mytargets, cuser)
		}

		targets = append(targets, mytargets...)
	}

	SendToMany(
		fmt.Sprintf(
			":%s QUIT :Changing host (%s -> %s)",
			user.GetHostMask(),
			user.host,
			newhost,
		),
		targets,
	)

	user.host = newhost

	user.FireNumeric(RPL_HOSTHIDDEN, newhost)

	for _, channel := range user.chanlist {
		for _, k := range channel.userlist {
			if k.id == user.id {
				continue
			}

			k.SendLinef(":%s JOIN %s", user.GetHostMask(), channel.name)

			if um, ok := channel.usermodes[user]; ok {
				if len(um) == 1 { //HACK: TODO FIX
					k.SendLinef(
						":%s MODE %s +%s %s",
						config.Server.Name,
						channel.name,
						um,
						user.nick,
					)
				} else {
					k.SendLinef(
						":%s MODE %s +%s %s %s",
						config.Server.Name,
						channel.name,
						um,
						user.nick,
						user.nick,
					)
				}
			}
		}
	}

	conn := RedisPool.Get()
	defer conn.Close()

	conn.Do("HSET", "charon:user:"+user.uuid, "host", newhost)

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "CHGHOST",
		Args:   []string{user.uuid, newhost},
	})

	return nil
}

func (user *User) PingChecker() {
	for {
		if user.dead || !user.local {
			return
		}

		if time.Now().After(user.nextcheck) {
			if user.waiting {
				since := time.Since(user.lastrcv).Seconds()
				user.Quit(fmt.Sprintf("Ping Timeout: %.0f seconds", since))
				return
			} else {
				user.SendLine(fmt.Sprintf("PING :%s", config.Server.Name))
				user.waiting = true
				user.nextcheck.Add(config.Server.PingTime * time.Second)
				logger.Printf("Sent user %s ping", user.nick)
			}
		}

		time.Sleep(config.Server.PingCheckTime * time.Second)
	}
}

func (user *User) FireNumeric(numeric int, args ...interface{}) {
	msg := fmt.Sprintf(":%s %.3d %s ", config.Server.Name, numeric, user.nick) + fmt.Sprintf(NUM[numeric], args...)
	user.SendLine(msg)
}

func NewUser() (user *User) {
	counter = counter + 1

	user = &User{
		id:        counter,
		nick:      "*",
		chanlist:  make(map[string]*Channel),
		epoch:     time.Now(),
		lastrcv:   time.Now(),
		nextcheck: time.Now().Add(config.Server.PingTime * time.Second),
		resolved:  make(chan bool),
		account:   "*",
		sid:       SID,
	}

	userlist[user.id] = user

	return
}

func (user *User) SetConn(conn net.Conn) {
	//TODO: Make this attachable
	user.connections = append(user.connections, conn)

	SetUserIPInfo(user)

	logger.Printf("New connection from " + user.realip)
	user.realhost = user.realip

	if !config.Server.Cloaking {
		user.host = user.realip
	} else {
		if user.ConnType == "IP4" {
			k := CloakIP4(user.realip)
			user.host = k
			user.ip = k
		} else {
			k := CloakIP6(user.realip)
			user.host = k
			user.ip = k
		}
	}

	if config.Server.ResolveHosts {
		go user.UserHostLookup()
	}

	go user.PingChecker()

	user.local = true
}

func (user *User) SendLine(msg string) {
	msg = fmt.Sprintf("%s\n", msg)

	if user.dead || user.system {
		return
	}

	for _, conn := range user.connections {
		_, err := conn.Write([]byte(msg))

		if err != nil {
			user.dead = true
			user.Quit("Error")
			logger.Printf("Error sending message to %s, disconnecting\n", user.nick)
			return
		}
	}

	if *debugFlag {
		debuglogger.Printf("Send to %s: %s", user.nick, msg)
	}
}

func (user *User) SendLinef(msg string, args ...interface{}) {
	user.SendLine(fmt.Sprintf(msg, args...))
}

func (user *User) HandleRequests() {
	b := bufio.NewReader(user.connections[0])

	for {
		if user.dead {
			return
		}

		line, err := b.ReadString('\n')

		if err != nil {
			logger.Printf("Error reading: " + err.Error())
			user.dead = true
			user.Quit("Error")
			return
		}

		if line == "" {
			user.dead = true
			user.Quit("Error")
			return
		}

		line = strings.TrimSpace(line)

		if *debugFlag {
			debuglogger.Println("Receive from", fmt.Sprintf("%s:", user.nick), line)
		}

		//TODO: send this via a channel to a main thread for sync issues
		ProcessLine(user, line)
	}
}

func (user *User) UserRegistrationFinished() {
	<-user.resolved // Wait for DNS resolution

	user.registered = true

	user.uuid = uuid.NewUUID().String()

	logger.Printf("User %s finished registration", user.uuid)
	user.FireNumeric(RPL_WELCOME, user.nick, user.ident, user.host)
	user.FireNumeric(RPL_YOURHOST, config.Server.Name, software, softwarev)
	user.FireNumeric(RPL_CREATED, epoch)

	//TODO fire RPL_MYINFO when we actually have enough stuff to do it
	user.FireNumeric(RPL_ISUPPORT, isupport)

	user.FireNumeric(RPL_YOURUUID, user.uuid)

	// Show user their hidden host if applicable
	if user.host != user.realhost {
		user.FireNumeric(RPL_HOSTHIDDEN, user.host)
		user.SendLinef(":%s MODE %s +x", config.Server.Name, user.nick)
	}

	if *debugFlag {
		user.SendLinef(":%s NOTICE %s :This server is in debug mode. Someone is attached to the console reading debug output. Tread with care.", config.Server.Name, user.nick)
	}

	if !config.Server.Privacy {
		user.SendLinef(":%s NOTICE %s :This server has privacy protections disabled.", config.Server.Name, user.nick)
	}

	line := []interface{}{
		"charon:user:" + user.uuid,
		"nick", user.nick,
		"user", user.ident,
		"host", user.host,
		"ip", user.realip,
		"realhost", user.realhost,
		"gecos", user.realname,
		"account", user.account,
		"oper", "false",
		"service", "false",
		"sid", SID,
		"uuid", user.uuid,
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
			user.uuid,     // uuid of user
			user.nick,     // nickname of user
			user.ident,    // username of user
			user.host,     // hostname of user
			user.realhost, // real host of user
			user.realip,   // IP address of user
			user.account,  // Account name of user
			"false",       // Services user?
			user.realname, // gecos of user
		},
	})

	LusersHandler(user, nil)

	for _, k := range config.Server.JoinChannel {
		JoinHandler(user, &message.Message{Args: []string{k}})
	}
}

func (user *User) UserHostLookup() {
	// wait for the reverse DNS lookup to finish
	defer func() {
		user.resolved <- true
	}()

	user.SendLinef(":%s NOTICE %s :*** Looking up your hostname...", config.Server.Name, user.nick)

	adds, err := net.LookupAddr(user.realip)

	if err != nil {
		user.SendLinef(":%s NOTICE %s :*** Unable to resolve your hostname", config.Server.Name, user.nick)

		return
	}

	addstring := adds[0]
	adds, err = net.LookupHost(addstring)

	if err != nil {
		user.SendLinef(":%s NOTICE %s :*** Unable to resolve your hostname", config.Server.Name, user.nick)

		return
	}

	for _, k := range adds {
		if user.realip == k {
			addstring = strings.TrimSuffix(addstring, ".")
			user.realhost = addstring

			if config.Server.Cloaking {
				user.host = CloakHost(addstring)
			} else {
				user.host = addstring
			}

			user.SendLinef(":%s NOTICE %s :*** Found your hostname", config.Server.Name, user.nick)

			return
		}
	}

	user.SendLinef(":%s NOTICE %s :*** Your forward and reverse DNS do not match, ignoring hostname", config.Server.Name, user.nick)
}

func (user *User) GetHostMask() string {
	if strings.Contains(user.nick, ".") {
		return user.nick
	}

	return fmt.Sprintf("%s!%s@%s", user.nick, user.ident, user.host)
}

func (user *User) IsIn(channel *Channel) bool {
	for _, k := range user.chanlist {
		if k == channel {
			return true
		}
	}
	return false
}
