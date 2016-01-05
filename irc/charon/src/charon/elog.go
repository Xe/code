package main

import (
	"fmt"
	"irc/message"
	oldlogger "log"
	"time"
)

type Elog struct {
	//nothing
}

func (elog *Elog) Printf(msg string, args ...interface{}) {
	oldlogger.Printf(msg, args...)
	WriteToLogFile(msg, args...)
	SendLineToLogChannels(fmt.Sprintf(msg, args...))
}

func SendLineToLogChannels(msg string) {
	if StartupIncomplete {
		return
	}

	for _, k := range config.Log.Channel {
		MessageHandler(SystemUser, &message.Message{Args: []string{k, msg}, Verb: "PRIVMSG"})

		c := GetChannelByName(k)
		if c == nil {
			continue
		}

		c.SendLinef(
			":%s NOTICE %s :%s",
			SystemUser.GetHostMask(),
			k,
			msg,
		)
	}
}

func WriteToLogFile(msg string, args ...interface{}) {
	if config == nil {
		return
	}

	if config.Log.File != "" && LoggingFile != nil {
		loggerstr := fmt.Sprintf("%s %s\n", time.Now().Format(time.RFC1123), fmt.Sprintf(msg, args...))

		_, err := LoggingFile.WriteString(loggerstr)
		if err != nil {
			config.Log.File = ""
			logger.Printf("ERROR: %s", err.Error())
			logger.Printf("Error writing to Logfile %s, disabling file loggerging", config.Log.File)
		}
	}
}
