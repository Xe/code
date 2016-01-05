package data

import (
	"strconv"
	"time"
)

/*
Channel is a chat channel.

Users join them to talk about things.
*/
type Channel struct {
	Name     string // Show in variable case
	SortName string // Use this for interal representation

	Epoch    time.Time
	Modes    string
	Metadata map[string]string

	/* Lists    map[string][]*ListItem */

	// Channel topic information
	Topic      string
	TopicHost  string
	TopicEpoch int64
}

// ToRedis turns the recieved channel into a format that redigo likes.
func (c *Channel) ToRedis() []interface{} {
	return []interface{}{
		"charon:channel:" + c.SortName,
		"topic", c.Topic,
		"topichost", c.TopicHost,
		"topictime", c.TopicEpoch,
		"modes", c.Modes,
		"name", c.Name,
	}
}

// ChannelFromRedis takes a map of strings to strings from
// redis.StringMap and turns it into a Channel pointer or returns
// an error describing the failure.
func ChannelFromRedis(details map[string]string) (c *Channel, err error) {
	topictime, err := strconv.ParseInt(details["topictime"], 10, 64)
	if err != nil {
		return nil, err
	}

	epoch, err := strconv.ParseInt(details["epoch"], 10, 64)
	if err != nil {
		return nil, err
	}

	c.Name = details["name"]
	c.Topic = details["topic"]
	c.TopicEpoch = topictime
	c.Epoch = time.Unix(epoch, 0)
	c.Modes = details["modes"]

	return
}
