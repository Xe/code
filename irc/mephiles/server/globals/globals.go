package globals

import (
	"regexp"
	"strconv"

	"github.com/Xe/mephiles/config"
	"github.com/Xe/mephiles/server/types"
)

// The globals that can be used.
var (
	Clients            *types.Clients
	Conf               *config.Config
	Links              map[string]*types.Server
	Server             *types.Server
	ClientCommands     map[string]types.ClientCommander
	S2SCommands        map[string]types.S2SCommander
	PreConnectCommands map[string]types.PreConnectCommander

	nextUID int
)

func init() {
	Clients = &types.Clients{
		ByNick:  make(map[string]*types.Client, 512),
		ByTS6ID: make(map[string]*types.Client, 512),
		ByUUID:  make(map[string]*types.Client, 512),
	}

	Links = make(map[string]*types.Server, 16)
	ClientCommands = make(map[string]types.ClientCommander)
	S2SCommands = make(map[string]types.S2SCommander)
	PreConnectCommands = make(map[string]types.PreConnectCommander)

	nextUID = 100000
}

// NextUID returns a new TS6 UID without the server part.
func NextUID() string {
	nextUID++
	return strconv.Itoa(nextUID)
}

// Regex for a valid nickname
var (
	REValidNickName = regexp.MustCompile(`^[a-zA-Z_` + "`" + `\\\[\]\{\}][a-zA-Z0-9_-` + "`" + `\\\[\]\{\}]{0,30}$`)
)
