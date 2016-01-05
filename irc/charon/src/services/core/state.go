package core

import (
	"services/data"
)

/*
State is the state of the network in memory.

This will be used to help decide actions to be taken on the source of truth.
*/
type State struct {
	Users    map[string]*data.User
	Channels map[string]*data.Channel
	Servers  map[string]*data.Server
}
