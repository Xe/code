package main

import (
	"crypto/tls"
	_ "expvar"
	"flag"
	"fmt"
	"irc/message"
	"net"
	"net/http"
	"os"
	"os/signal"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/garyburd/redigo/redis"
)

var (
	nukeChannelsFlag = flag.Bool("nukechannels", false, "Nuke channels existing in redis?")
	debugFlag        = flag.Bool("debug", false, "Run server in debug mode?")
	configFlag       = flag.String("config", "charon.cfg", "Configuration file to use")
)

func init() {
	c := make(chan os.Signal, 1)
	signal.Notify(c, os.Interrupt, os.Kill)

	go func() {
		<-c

		ShutdownHandler(SystemUser, nil)
	}()
}

func main() {
	flag.Parse()

	e1, err := net.Listen("tcp", "0.0.0.0:8123")
	if err != nil {
		logger.Printf("Error starting http server")
	} else {
		logger.Printf("Http Server Started on port 8123")
		go http.Serve(e1, nil)
	}

	SetupNumerics()
	SetupConfig(*configFlag)

	// Listen for incoming connections.
	var tconfig tls.Config
	var tlser error

	if len(config.Listen.TLSPort) > 0 {
		var cert tls.Certificate
		cert, tlser = tls.LoadX509KeyPair(config.TLS.CertPath, config.TLS.KeyPath)

		if tlser == nil {
			tconfig = tls.Config{Certificates: []tls.Certificate{cert}}
		} else {
			logger.Printf("TLS ERR: %s", tlser.Error())
		}
	}

	var listeners []net.Listener
	for _, LISTENING_PORT := range config.Listen.PlainPort {
		l, err := net.Listen("tcp", fmt.Sprintf(":%d", LISTENING_PORT))

		if err != nil {
			logger.Printf("Error listening: " + err.Error())
			os.Exit(1)
		} else {
			listeners = append(listeners, l)
			logger.Printf("Listening on 0.0.0.0:%d", LISTENING_PORT)
		}
	}

	if tlser == nil {
		for _, LISTENING_PORT := range config.Listen.TLSPort {
			l, err := tls.Listen("tcp", fmt.Sprintf(":%d", LISTENING_PORT), &tconfig)

			if err != nil {
				logger.Printf("Error listening: " + err.Error())
				os.Exit(1)
			} else {
				listeners = append(listeners, l)
				logger.Printf("TLS Listening on 0.0.0.0:%d", LISTENING_PORT)
			}
		}
	}

	// Close the listener when the application closes.
	for _, l := range listeners {
		defer l.Close()
	}

	for _, l := range listeners {
		go listenerthing(l)
	}

	go ProcessRemoteHandlers()

	periodicStatusUpdate()
}

func listenerthing(l net.Listener) {
	for {
		conn, err := l.Accept()
		if err != nil {
			logger.Printf("Error accepting: " + err.Error())
		} else {
			user := NewUser()
			user.SetConn(conn)
			checkMaxUsers()
			go user.HandleRequests()
		}
	}
}

func checkMaxUsers() {
	if len(userlist) > maxUsers {
		maxUsers = len(userlist)
	}
}

func periodicStatusUpdate() {
	for {
		if *debugFlag {
			logger.Printf("Status: %d current Goroutines", runtime.NumGoroutine())
			logger.Printf("Status: %d current users", len(userlist))
			logger.Printf("Status: %d current channels", len(chanlist))
		}

		time.Sleep(config.Server.StatTime * time.Second)
	}
}

func SetupPool() {
	RedisPool = &redis.Pool{
		MaxIdle: 10,
		Dial: func() (redis.Conn, error) {
			c, err := redis.Dial("tcp", fmt.Sprintf("%s:%d", config.Redis.Host, config.Redis.Port))
			if err != nil {
				return nil, err
			}

			if config.Redis.Password != "" {
				if _, err := c.Do("AUTH", config.Redis.Password); err != nil {
					c.Close()
					return nil, err
				}
			}

			return c, nil
		},
	}

	conn := RedisPool.Get()
	defer conn.Close()

	// Delete stale entries in Redis
	mysection := strings.Split(SID, "-")[4]

	num, err := conn.Do("EVAL", `local l = 0 for _,k in ipairs(redis.call('keys','charon:*`+mysection+`')) do redis.call('del',k) l = l + 1 end return l`, "0")
	if err != nil {
		logger.Printf("Could not delete stale entries from redis: %#v", err)
	} else {
		logger.Printf("Got rid of %v stale entries in redis", num)
	}

	// Grab state from the network
	clients, err := redis.Strings(conn.Do("KEYS", "charon:user:*"))
	if err != nil {
		logger.Printf("Could not load users from redis")
		panic(err)
	}

	for _, c := range clients {
		chash, err := redis.StringMap(conn.Do("HGETALL", c))
		if err != nil {
			panic(err)
		}

		u := NewUser()

		u.nick = chash["nick"]
		u.ident = chash["user"]
		u.host = chash["host"]
		u.realip = chash["ip"]
		u.realhost = chash["realhost"]
		u.realname = chash["gecos"]
		u.account = chash["account"]
		u.oper = chash["oper"] == "true"
		u.system = chash["service"] == "true"
		u.sid = chash["sid"]
		u.uuid = chash["uuid"]
	}

	logger.Printf("Synced %d clients", len(clients))

	// Now channels
	if !*nukeChannelsFlag {
		logger.Printf("Loading channels from redis")
		channels, err := redis.Strings(conn.Do("KEYS", "charon:channel:*"))
		if err != nil {
			logger.Printf("Could not load channels from redis")
			panic(err)
		}

		for _, chann := range channels {
			chash, err := redis.StringMap(conn.Do("HGETALL", chann))
			if err != nil {
				panic(err)
			}

			c := NewChannel(chash["name"])

			topictime, _ := strconv.ParseInt(chash["topictime"], 10, 64)
			epoch, _ := strconv.ParseInt(chash["epoch"], 10, 64)

			c.topic = chash["topic"]
			c.topichost = chash["topichost"]
			c.topictime = topictime
			c.epoch = time.Unix(epoch, 0)
			c.cmodes = chash["modes"]
		}

		logger.Printf("Synched %d channels", len(channels))

		// And channel members
		channels, err = redis.Strings(conn.Do("KEYS", "charon:channelmembers:*"))
		if err != nil {
			panic(err)
		}

		for _, key := range channels {
			cname := strings.Join(strings.Split(key, ":")[2:], ":")
			c := GetChannelByName(cname)
			if c == nil {
				logger.Printf("%#v", chanlist)
				panic("Asked to join people to a channel (" + cname + ") that doesn't exist???")
			}

			members, err := redis.Strings(conn.Do("LRANGE", key, "0", "-1"))
			if err != nil {
				panic(err)
			}

			for _, member := range members {
				k := GetUserByUUID(member)
				if k == nil {
					conn.Do("LREM", key, "1", member)

					if *debugFlag {
						logger.Printf(
							"I don't know about %s, so deleting it from redis",
							member,
						)
					}

					continue
				}

				if *debugFlag {
					logger.Printf(
						"Joined %s to %s",
						k.GetHostMask(),
						c.name,
					)
				}

				c.JoinUser(k)
			}
		}
	}

	// Announce ircd is online
	SendLineToRedis("global", &message.Message{
		Source: SID,
		Verb:   "ONLINE",
		Args: []string{
			"leaf",
			epoch.Format(time.RFC3339),
			config.Server.Name,
			config.Server.Description,
		},
	})

	line := []interface{}{
		"charon:server:" + SID,
		"name", config.Server.Name,
		"description", config.Server.Description,
		"kind", "leaf",
		"epoch", fmt.Sprintf("%d", epoch.Unix()),
	}

	_, err = conn.Do("HMSET", line...)
	if err != nil {
		panic(err)
	}
}
