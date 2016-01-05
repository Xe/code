package main

import (
	"container/list"
	"os"
	"sync"
	"time"

	charonconfig "charon/myconfig"

	"github.com/garyburd/redigo/redis"
)

const (
	software  = "charon"
	softwarev = "v0.1"
	isupport  = "NAMESX CHANTYPES=#& PREFIX=(ov)@+ CHANMODES=b,,APmnt4 NETWORK=ShadowNET"
)

type Mode4CacheItem struct {
	user    *User
	number  int64
	channel *Channel
}

var (
	Mode4CacheMutex   = &sync.Mutex{}
	Mode4Cache        = list.New()
	RedisPool         *redis.Pool
	StartupIncomplete = true //used to determine if the ircd is up and running yet
	valid_chan_prefix = []string{"#", "&"}
	global_bad_chars  = []string{","}
	config            *charonconfig.Config
	counter           = 1
	userlist          = make(map[int]*User)
	chanlist          = make(map[string]*Channel)
	serverlist        = make(map[string]*Server)
	maxUsers          int
	maxRoutines       int
	epoch             = time.Now()
	opercount         = 0
	SID               string
	SystemUser        = &User{
		ident:      "system",
		id:         0,
		realname:   "system user",
		userset:    true,
		registered: true,
		ip:         "127.0.0.1",
		realip:     "127.0.0.1",
		epoch:      time.Now(),
		chanlist:   make(map[string]*Channel),
		oper:       true,
		system:     true,
	}
	DefaultConf = &charonconfig.Config{
		Server: charonconfig.Server{
			Name:          "test.net.local",
			Description:   "A test server",
			PingTime:      45,
			PingCheckTime: 20,
			StatTime:      30,
			Salt:          "foobang",
			ResolveHosts:  true,
			Cloaking:      true,
			JoinChannel:   []string{"#default"},
		},

		Default: charonconfig.Default{
			Cmode: "nt",

			KickReason:  "Your behavior is not conductive of the desired environment.",
			KillReason:  "Your behavior is not conductive of the desired environment.",
			QuitReason:  "Leaving.",
			PartReason:  "Leaving.",
			KlineReason: "K-Lined",
		},

		Log: charonconfig.Log{
			Channel: []string{"#log", "#opers"},
			File:    "charon.log",
		},

		Oper: map[string]*charonconfig.Oper{
			"default": &charonconfig.Oper{
				Password:  "password",
				Encrypted: false,
			},
		},

		Listen: charonconfig.Listen{
			PlainPort: []int{6667, 6668, 6669},
		},
	}
	logger      = &Elog{}
	LoggingFile *os.File
)
