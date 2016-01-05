package data

import (
	"fmt"
	"strconv"
	"time"
)

/*
User is a user on the network.

This could be a bot or a human. It really just depends.
*/
type User struct {
	Nick     string
	Ident    string
	Host     string
	Gecos    string
	RealHost string
	IP       string

	// Type-1 universally unique identifiers
	UUID string // for the user
	SID  string // for the server the user is connected to

	Oper    bool
	Service bool

	Account  string
	Metadata map[string]string

	Joins map[string]*Channel

	Epoch time.Time
}

// ToRedis takes the recieved User pointer and returns
// a slice usable in direct redis calls with the included
// redigo client.
func (u *User) ToRedis() []interface{} {
	return []interface{}{
		"charon:user:" + u.UUID,
		"nick", u.Nick,
		"user", u.Ident,
		"host", u.Host,
		"ip", u.IP,
		"realhost", u.RealHost,
		"gecos", u.Gecos,
		"account", u.Account,
		"sid", u.SID,
		"oper", fmt.Sprintf("%b", u.Oper),
		"service", fmt.Sprintf("%b", u.Service),
		"uuid", u.UUID,
		"epoch", u.Epoch.Unix(),
	}
}

// UserFromRedis takes a map of strings to strings from redis.StringMap
// and turns it into a User pointer or returns an error describing the
// failure.
func UserFromRedis(details map[string]string) (u *User, err error) {
	epoch, err := strconv.ParseInt(details["epoch"], 10, 64)
	if err != nil {
		return nil, err
	}

	u.Nick = details["nick"]
	u.Ident = details["user"]
	u.Host = details["host"]
	u.IP = details["ip"]
	u.RealHost = details["realhost"]
	u.Gecos = details["gecos"]
	u.Account = details["account"]
	u.Oper = details["oper"] == "true"
	u.Service = details["service"] == "true"
	u.SID = details["sid"]
	u.UUID = details["uuid"]
	u.Epoch = time.Unix(epoch, 0)

	return
}
