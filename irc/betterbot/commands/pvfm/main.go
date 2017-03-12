package main

import (
	"fmt"
	"log"
	"runtime"
	"time"

	"github.com/Xe/betterbot-common"
	"github.com/Xe/betterbot/commands/pvfm/pvfm"
	"github.com/Xe/betterbot/commands/pvfm/pvfm/station"
	"github.com/Xe/betterbot/commands/pvfm/pvl"
	"github.com/namsral/flag"
	"github.com/tebeka/strftime"
)

var (
	uplink *common.Uplink
)

func main() {
	flag.Parse()

	conn, err := common.Connect()
	if err != nil {
		panic(err)
	}
	uplink = conn

	conn.Subscribe(uplink.ID+":input", func(subj, unusedReply string, msg *common.Message) {
		if msg.Kind != "TextMessage" {
			return
		}

		reply := &common.Message{
			Destination: msg.ReplyTo,
			Kind:        "TextMessage",
		}

		switch msg.Body {
		case ";np":
			body, err := np()
			if err != nil {
				reply.Body = err.Error()
			} else {
				reply.Body = body
			}
		case ";dj":
			body, err := dj()
			if err != nil {
				reply.Body = err.Error()
			} else {
				reply.Body = body
			}
		case ";stats":
			body, err := stats()
			if err != nil {
				reply.Body = err.Error()
			} else {
				reply.Body = body
			}
		}

		conn.Publish(msg.Via+":input", reply)
	})

	conn.Publish("betterbot.birth", &common.Birth{
		ID:         uplink.ID,
		EventKinds: []string{"TextMessage"},
	})

	runtime.Goexit()
}

func np() (string, error) {
	i, err := pvfm.GetStats()
	if err != nil {
		log.Printf("Can't get info: %v, failing over to plan b", err)
		return doStationRequest()
	}

	result := ""

	if i.Main.Nowplaying == "Fetching info..." {
		log.Println("Main information was bad, fetching from station directly...")
		msg, err := doStationRequest()
		if err != nil {
			return "", err
		}
		return msg, err
	} else {
		result = result + fmt.Sprintf(
			"Now playing: %s on Ponyville FM's main stream!\n",
			i.Main.Nowplaying,
		)
		result = result + fmt.Sprintf(
			"Now playing: %s on PVFM Chill!\n",
			i.Secondary.Nowplaying,
		)
		result = result + fmt.Sprintf(
			"Now playing: %s on PVFM Free!",
			i.MusicOnly.Nowplaying,
		)
	}

	return result, nil
}

func dj() (string, error) {
	cal, err := pvl.Get()
	if err != nil {
		return "", err
	}

	now := cal.Result[0]
	rep := ""

	localTime := time.Now()
	thentime := time.Unix(now.StartTime, 0)
	if thentime.Unix() < localTime.Unix() {
		rep = fmt.Sprintf("Currently live: %s\n", now.Title)
		now = cal.Result[1]
	}

	nowTime := time.Unix(now.StartTime, 0)
	zone, offset := nowTime.Zone()
	fmttime, _ := strftime.Format("%Y-%m-%d %H:%M:%S", nowTime)
	offset = offset / 60 / 60

	rep = rep + fmt.Sprintf("Next event: %s at %s \x02%s\x02 (%d)",
		now.Title,
		fmttime,
		zone, offset,
	)
	return rep, nil
}

func stats() (string, error) {
	return "TODO: implement", nil
}

func doStationRequest() (string, error) {
	stats, err := station.GetStats()
	if err != nil {
		return "", err
	}

	rep := fmt.Sprintf(
		"Now playing: %s - %s on Ponyville FM!",
		stats.Icestats.Source[0].Title,
		stats.Icestats.Source[0].Artist,
	)

	return rep, nil
}
