package main

import (
	"runtime"
	"time"

	"github.com/Xe/betterbot-common"
	"github.com/garyburd/redigo/redis"
	"github.com/namsral/flag"
)

var (
	uplink *common.Uplink
	pool   *redis.Pool

	redisServer = flag.String("redis", "127.0.0.1:6379", "redis host to connect to")
	config      = flag.String("config", "", "configuration file")
)

func main() {
	flag.Parse()

	pool = newPool(*redisServer)

	conn, err := common.Connect()
	if err != nil {
		panic(err)
	}
	uplink = conn

	conn.QueueSubscribe("betterbot.birth", "router", subBirth)
	conn.QueueSubscribe("betterbot.input", "router", subInput)

	runtime.Goexit()
}

func subBirth(subj, reply string, msg *common.Birth) {
	c := pool.Get()
	defer c.Close()

	_, err := c.Do("LPUSH", "betterbot:nodes", msg.ID)
	if err != nil {
		uplink.Printf("could not append %q to the list of nodes: %v", msg.ID, err)
		return
	}

	for _, kind := range msg.EventKinds {
		_, err := c.Do("LPUSH", "betterbot:kind:"+kind, msg.ID)
		if err != nil {
			uplink.Printf("Could not update kind of %q (%s): %v", msg.ID, kind, err)
			return
		}
	}
}

func subInput(subj, reply string, msg *common.Message) {
	c := pool.Get()
	defer c.Close()

	uplink.Printf("Sending message %q %q (<%s> %s)", msg.ID, msg.Kind, msg.Sender, msg.Body)

	targets, err := redis.Strings(c.Do("LRANGE", "betterbot:kind:"+msg.Kind, 0, -1))
	if err != nil {
		uplink.Printf("Could not find kind targets for %s (%q): %v", msg.Kind, msg.ID, err)
		return
	}

	for _, target := range targets {
		if target == msg.Via {
			continue
		}

		uplink.Publish(target+":input", msg)
	}
}

func newPool(server string) *redis.Pool {
	return &redis.Pool{
		MaxIdle:     3,
		IdleTimeout: 240 * time.Second,
		Dial: func() (redis.Conn, error) {
			c, err := redis.Dial("tcp", server)
			if err != nil {
				return nil, err
			}
			return c, err
		},
		TestOnBorrow: func(c redis.Conn, t time.Time) error {
			_, err := c.Do("PING")
			return err
		},
	}
}
