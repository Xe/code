package main

import (
	"log"
	"os"

	"github.com/Xe/uuid"

	charonconfig "charon/myconfig"
)

func SetupConfig(filename string) {
	var err error
	config, err = charonconfig.Load(filename)

	if err != nil {
		log.Fatal(err)
	}

	uuid.SetNodeID([]byte(config.Server.Name))
	SID = uuid.NewUUID().String()

	SetupPool()

	if *nukeChannelsFlag {
		logger.Printf("Nuking old channels...")

		conn := RedisPool.Get()
		num, err := conn.Do("EVAL", `local l = 0 for _,k in ipairs(redis.call('keys','charon:channel*')) do redis.call('del',k) l = l + 1 end return l`, "0")

		if err != nil {
			logger.Printf("Could not delete channels from redis: %#v", err)
		} else {
			logger.Printf("Got rid of %v channels in redis", num)
		}

		conn.Close()
	}

	SystemUser.nick = config.Server.Name
	SystemUser.host = config.Server.Name
	SystemUser.realhost = config.Server.Name
	SystemUser.realname = config.Server.Description + " (" + SID + ")"
	SystemUser.account = "*"
	SystemUser.uuid = uuid.NewUUID().String()

	SetupSystemUser()

	if config.Log.File != "" {
		f, err := os.OpenFile(config.Log.File, os.O_RDWR|os.O_CREATE|os.O_APPEND, 0666)
		if err != nil {
			k := config.Log.File
			config.Log.File = ""
			logger.Printf("Error opening logger file %s, disabling file loggerging", k)
		} else {
			LoggingFile = f
		}
	} else {
		logger.Printf("No logger file specified, disabling file loggerging")
	}

	StartupIncomplete = false
}
