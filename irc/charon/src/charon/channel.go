package main

import (
	"bytes"
	"fmt"
	"irc/message"
	"strings"
	"time"

	"github.com/garyburd/redigo/redis"
)

//Channel...
//represents an irc channel
type Channel struct {
	name      string
	epoch     time.Time
	userlist  map[int]*User
	usermodes map[*User]string
	banlist   map[int]*Ban
	cmodes    string
	topic     string
	topichost string
	topictime int64
	creator   string
}

func (channel *Channel) SetTopic(newtopic string, hostmask string) {
	channel.topic = newtopic
	channel.topichost = hostmask
	channel.topictime = time.Now().Unix()
	channel.SendLinef(":%s TOPIC %s :%s", hostmask, channel.name, newtopic)
}

func NewChannel(newname string) *Channel {
	chann := &Channel{
		name:      newname,
		epoch:     time.Now(),
		creator:   SID,
		cmodes:    config.Default.Cmode,
		userlist:  make(map[int]*User),
		usermodes: make(map[*User]string),
		banlist:   make(map[int]*Ban),
	}

	chanlist[strings.ToLower(chann.name)] = chann

	chann.cmodes = config.Default.Cmode

	logger.Printf("Channel %s created", chann.name)

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "NEWCHAN",
		Args: []string{
			chann.name,
			config.Default.Cmode,
			fmt.Sprintf("%d", chann.epoch.Unix()),
		},
	})

	c := RedisPool.Get()
	defer c.Close()

	line := []interface{}{
		"charon:channel:" + strings.ToLower(chann.name),
		"topic", chann.topic,
		"topichost", chann.topichost,
		"topictime", fmt.Sprintf("%d", chann.topictime),
		"epoch", fmt.Sprintf("%d", chann.epoch.Unix()),
		"modes", chann.cmodes,
		"name", chann.name,
	}

	_, err := c.Do("HMSET", line...)
	if err != nil {
		logger.Printf(
			"Can't write creator SID to channel %s: %#v",
			chann.name,
			err,
		)
	}

	return chann
}

func (channel *Channel) len() int {
	k := len(channel.userlist)

	var check bool

	for _, k := range config.Log.Channel {
		if channel == GetChannelByName(k) {
			check = true
			break
		}
	}

	if channel.HasUser(SystemUser) && !check {
		k--
	}

	return k
}

func (channel *Channel) JoinUser(user *User) {
	channel.userlist[user.id] = user

	defer channel.FireNames(user)

	if channel.len() == 1 && !channel.HasMode("P") {
		channel.usermodes[user] = "o"

		defer SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "CPRIVMODE",
			Args: []string{
				"ADD",
				SID,
				channel.name,
				"o",
				user.uuid,
			},
		})
	}

	if channel.HasMode("A") && user.oper {
		channel.usermodes[user] = "o"

		defer SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "CPRIVMODE",
			Args: []string{
				channel.name,
				"+o",
				user.uuid,
			},
		})

		defer channel.SendLinef(
			":%s MODE %s +o %s",
			config.Server.Name,
			channel.name,
			user.nick,
		)

		defer logger.Printf(
			"%s was given ops in %s for being an oper in a +A channel",
			user.nick,
			channel.name,
		)
	}

	channel.SendLinef(":%s JOIN %s", user.GetHostMask(), channel.name)

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "JOIN",
		Args: []string{
			channel.name,
			user.uuid,
		},
	})

	if len(channel.topic) > 0 {
		channel.FireTopic(user)
	}
}

func (channel *Channel) GetUserPrefix(user *User) string {
	if strings.Contains(channel.usermodes[user], "o") {
		return "@"
	}

	if strings.Contains(channel.usermodes[user], "v") {
		return "+"
	}

	return ""
}

func (channel *Channel) FireTopic(user *User) {
	if len(channel.topic) > 0 {
		user.FireNumeric(RPL_TOPIC, channel.name, channel.topic)
		user.FireNumeric(RPL_TOPICWHOTIME, channel.name, channel.topichost, channel.topictime)
	} else {
		user.FireNumeric(RPL_NOTOPIC, channel.name)
	}
}

func (channel *Channel) FireNames(user *User) {
	var buffer bytes.Buffer

	for _, k := range channel.userlist {
		if buffer.Len()+len(channel.GetUserPrefix(k))+len(user.nick) > 500 {
			user.FireNumeric(RPL_NAMEPLY, channel.name, strings.TrimSpace(buffer.String()))
			buffer.Reset()
		}

		buffer.WriteString(channel.GetUserPrefix(k))
		buffer.WriteString(k.nick)
		buffer.WriteString(" ")
	}

	if buffer.Len() > 1 {
		resp := strings.TrimSpace(buffer.String())
		user.FireNumeric(RPL_NAMEPLY, channel.name, resp)
	}

	user.FireNumeric(RPL_ENDOFNAMES, channel.name)
}

func (channel *Channel) GetUserList() []*User {
	list := []*User{}

	for _, k := range channel.userlist {
		list = append(list, k)
	}

	return list
}

func (channel *Channel) GetUserPriv(user *User) int {
	score := 0

	if strings.Contains(channel.usermodes[user], "o") {
		score += 100
	}

	if strings.Contains(channel.usermodes[user], "v") {
		score += 10
	}

	if user.system || user.oper {
		score += 1000
	}

	if user == SystemUser {
		score += 10000
	}

	return score
}

func (channel *Channel) ShouldIDie() {
	if channel.HasMode("P") {
		return
	}

	if channel.len() < 1 {
		if channel.HasUser(SystemUser) {
			PartHandler(SystemUser, &message.Message{Args: []string{"PART", channel.name}})
		}

		delete(chanlist, strings.ToLower(channel.name))
		logger.Printf("Channel %s has no users, destroying\n", channel.name)
	}
}

func (channel *Channel) FireModes(user *User) {
	user.FireNumeric(RPL_CHANNELMODEIS, channel.name, channel.cmodes)
	user.FireNumeric(RPL_CREATIONTIME, channel.name, channel.epoch.Unix())
}

func (channel *Channel) HasMode(mode string) bool {
	if strings.Contains(channel.cmodes, mode) {
		return true
	} else {
		return false
	}
}

func (channel *Channel) SetUmode(user *User, changing *User, mode string) {
	if channel.CheckYourPrivlege(changing) {
		return
	}

	if !strings.Contains(channel.usermodes[user], mode) {
		channel.usermodes[user] = channel.usermodes[user] + mode
		channel.SendLinef(":%s MODE %s +%s %s", changing.GetHostMask(), channel.name, mode, user.nick)
		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "CPRIVMODE",
			Args: []string{
				"ADD",
				changing.uuid,
				channel.name,
				mode,
				user.uuid,
			},
		})
	}
}

func (channel *Channel) UnsetUmode(user *User, changing *User, mode string) {
	if channel.CheckYourPrivlege(changing) {
		return
	}

	if strings.Contains(channel.usermodes[user], mode) {
		channel.usermodes[user] = strings.Replace(channel.usermodes[user], mode, "", 1)
		channel.SendLinef(":%s MODE %s -%s %s", changing.GetHostMask(), channel.name, mode, user.nick)
	}

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "CPRIVMODE",
		Args: []string{
			"REMOVE",
			changing.uuid,
			channel.name,
			mode,
			user.uuid,
		},
	})
}

func (channel *Channel) SetMode(mode string, changing *User) {
	if channel.CheckYourPrivlege(changing) {
		return
	}
	if !strings.Contains(channel.cmodes, mode) {
		channel.cmodes = channel.cmodes + mode
		channel.SendLinef(":%s MODE %s +%s", changing.GetHostMask(), channel.name, mode)

		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "CMODE",
			Args: []string{
				"ADD",
				changing.uuid,
				channel.name,
				mode,
			},
		})
	}
}

func (channel *Channel) UnsetMode(mode string, changing *User) {
	if channel.CheckYourPrivlege(changing) {
		return
	}
	if strings.Contains(channel.cmodes, mode) {
		channel.cmodes = strings.Replace(channel.cmodes, mode, "", 1)
		channel.SendLinef(":%s MODE %s -%s", changing.GetHostMask(), channel.name, mode)
		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "CMODE",
			Args: []string{
				"REMOVE",
				changing.uuid,
				channel.name,
				mode,
			},
		})

	}
}

func (channel *Channel) HasUser(user *User) bool {
	if channel.userlist[user.id] == user {
		return true
	} else {
		return false
	}
}

func (channel *Channel) SendLinef(msg string, args ...interface{}) {
	for _, k := range channel.userlist {
		if k.local {
			k.SendLine(fmt.Sprintf(msg, args...))
		}
	}
}

func (channel *Channel) CheckYourPrivlege(user *User) bool {
	if channel.GetUserPriv(user) < 100 {
		//SHITLORD!
		user.FireNumeric(ERR_CHANOPRIVSNEEDED, channel.name)
		return true //privlege successfully checked.
	} else {
		return false
	}
}

func (channel *Channel) SetBan(m string, user *User) {
	if channel.CheckYourPrivlege(user) {
		return
	}

	if GetBanByMask(channel, m) != nil {
		return
	}

	hm := user.GetHostMask()
	b := NewBan(m, hm)
	channel.banlist[b.id] = b
	channel.SendLinef(":%s MODE %s +b %s", hm, channel.name, m)

	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "CLIST",
		Args: []string{
			"ADD",
			"ban",
			channel.name,
			user.uuid,
			m,
		},
	})
}

func (channel *Channel) UnsetBan(m string, user *User) {
	if channel.CheckYourPrivlege(user) {
		return
	}
	ban := GetBanByMask(channel, m)
	if ban != nil {
		delete(channel.banlist, ban.id)
		channel.SendLinef(":%s MODE %s -b %s", user.GetHostMask(), channel.name, ban.mask)

		SendLineToRedis("global", &message.Message{
			Source: SID,
			Verb:   "CLIST",
			Args: []string{
				"REMOVE",
				"ban",
				channel.name,
				user.uuid,
				ban.mask,
			},
		})
	}
}

func (channel *Channel) IsUserBanned(user *User) bool {
	hm := user.GetHostMask()
	for _, k := range channel.banlist {
		if WildcardMatch(hm, k.mask) {
			return true
		}
	}
	return false
}

func (channel *Channel) FireBanlist(user *User) {
	for _, b := range channel.banlist {
		user.FireNumeric(RPL_BANLIST, channel.name, b.mask, b.whoset, b.ts.Unix())
	}
	user.FireNumeric(RPL_ENDOFBANLIST, channel.name)
}

func (channel *Channel) IsLogChan() bool {
	me := strings.ToLower(channel.name)

	for _, k := range config.Log.Channel {
		if me == strings.ToLower(k) {
			return true
		}
	}

	return false
}

func (channel *Channel) GetCount() (int64, error) {
	p := RedisPool.Get()

	c, err := redis.Int64(p.Do("INCR", fmt.Sprintf("charon:counter:%s", channel.name)))

	if err != nil {
		if *debugFlag {
			logger.Printf("redis error: %#v", err)
		}

		return 0, err
	}

	return c, nil
}
